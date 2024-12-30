CLASS zcl_abappm_code_importer DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS scan
      IMPORTING
        !program_name   TYPE progname
        !program_source TYPE zif_abappm_importer=>ty_code OPTIONAL
      RETURNING
        VALUE(result)   TYPE stokesx_tab
      RAISING
        zcx_abappm_error.

    CLASS-METHODS import
      IMPORTING
        !program_name   TYPE progname
        !map            TYPE zif_abappm_importer=>ty_map
        !is_pretty      TYPE abap_bool DEFAULT abap_false
        !program_source TYPE zif_abappm_importer=>ty_code OPTIONAL
      RETURNING
        VALUE(result)   TYPE zif_abappm_importer=>ty_code
      RAISING
        zcx_abappm_error.

    CLASS-METHODS read
      IMPORTING
        !program_name   TYPE progname
        !program_source TYPE zif_abappm_importer=>ty_code OPTIONAL
      RETURNING
        VALUE(result)   TYPE zif_abappm_importer=>ty_code
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_class_name_from_token
      IMPORTING
        !token        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_interface_name_from_token
      IMPORTING
        !token        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_object_name_from_abapdoc
      IMPORTING
        !token        TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_abappm_code_importer IMPLEMENTATION.


  METHOD get_class_name_from_token.

    result = token.

    IF result IS NOT INITIAL AND result(1) = '('.
      SPLIT result+1 AT ')' INTO result DATA(rest).
      IF result IS NOT INITIAL AND result(1) = ''''.
        " ('zcl_test')=>method
        SPLIT result+1 AT '''' INTO result rest.
      ELSE.
        " (var)=>method
        result = ''.
      ENDIF.
      RETURN.
    ENDIF.

    IF result CS '~'.
      " zif_test~method
      SPLIT result AT '~' INTO result rest.
    ENDIF.
    IF result CS '('.
      " zcl_test( )
      SPLIT result AT '(' INTO result rest.
    ENDIF.

    IF result CS '=>'.
      " zcl_test=>method
      SPLIT result AT '=' INTO result rest.
    ELSEIF result CS '->'.
      " zcl_test->method
      SPLIT result AT '->' INTO result rest.
    ENDIF.

    " @zcl_test=>constant
    IF result(1) = '@'.
      result = result+1.
    ENDIF.

  ENDMETHOD.


  METHOD get_interface_name_from_token.

    result = token.

    IF result CS '=>'.
      " zcl_test=>zif_test~method
      SPLIT result AT '>' INTO DATA(rest) result.
    ELSEIF result CS '->'.
      " zcl_test->zif_test~method
      SPLIT result AT '>' INTO rest result.
    ENDIF.

    IF result IS NOT INITIAL AND result(1) = '('.
      SPLIT result+1 AT ')' INTO result rest.
      IF result IS NOT INITIAL AND result(1) = ''''.
        " zcl_test->('zif_test~method')
        SPLIT result+1 AT '''' INTO result rest.
      ELSE.
        " zcl_test->(var)
        result = ''.
        RETURN.
      ENDIF.
    ENDIF.

    IF result CS '~'.
      " zif_test~method
      SPLIT result AT '~' INTO result rest.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_name_from_abapdoc.

    result = token.

    " Example:
    " "! @raising zcx_test_error | Exception
    FIND REGEX '"! @raising (.*) \|' IN token SUBMATCHES result.
    IF sy-subrc = 0.
      result = to_upper( result ).
    ENDIF.

  ENDMETHOD.


  METHOD import.

    CONSTANTS c_token_types TYPE string VALUE 'HIlm34C'.

    DATA:
      tokens         TYPE stokesx_tab,
      tokens_checked TYPE stokesx_tab.

    DATA(code) = read(
      program_name   = program_name
      program_source = program_source ).

    tokens = scan(
      program_name   = program_name
      program_source = code ).

    IF tokens IS INITIAL.
      RETURN.
    ENDIF.

    " Collect all tokens that have a mapping
    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE type CA c_token_types.

      " FIXME: this does not work if there are objects with different type but same name
      IF <token>-type = 'C'.
        " abapDoc comments
        READ TABLE map ASSIGNING FIELD-SYMBOL(<map>)
          WITH TABLE KEY old_object = get_object_name_from_abapdoc( <token>-str ).
        IF sy-subrc = 0.
          <token>-str = replace(
            val  = <token>-str
            sub  = <map>-old_object
            with = to_lower( <map>-new_object )
            case = abap_false ).
          INSERT <token> INTO TABLE tokens_checked.
        ENDIF.
      ELSE.
        " Other statements
        READ TABLE map ASSIGNING <map>
          WITH TABLE KEY old_object = get_class_name_from_token( <token>-str ).
        IF sy-subrc = 0.
          <token>-str = replace(
            val  = <token>-str
            sub  = <map>-old_object
            with = <map>-new_object
            case = abap_false ).
          INSERT <token> INTO TABLE tokens_checked.
        ENDIF.
        READ TABLE map ASSIGNING <map>
          WITH TABLE KEY old_object = get_interface_name_from_token( <token>-str ).
        IF sy-subrc = 0.
          <token>-str = replace(
            val  = <token>-str
            sub  = <map>-old_object
            with = <map>-new_object
            case = abap_false ).
          INSERT <token> INTO TABLE tokens_checked.
        ENDIF.
      ENDIF.

    ENDLOOP.

    " New tokens might be different in length. Sort tokens back to front
    " so source can be adjusted without problems
    SORT tokens_checked BY row DESCENDING col DESCENDING.

    LOOP AT tokens_checked ASSIGNING <token>.

      READ TABLE code ASSIGNING FIELD-SYMBOL(<source>) INDEX <token>-row.
      ASSERT sy-subrc = 0.

      <source> =
        substring( val = <source> len = <token>-col ) && <token>-str &&
        substring( val = <source> off = <token>-col + <token>-len1 ).

      IF strlen( <source> ) > 255.
        zcx_abappm_error=>raise( |Line length overflow in { program_name }, Line { <token>-row }| ).
      ENDIF.

    ENDLOOP.

    IF is_pretty = abap_true.
      CALL FUNCTION 'PRETTY_PRINTER'
        EXPORTING
          inctoo             = abap_false
        TABLES
          ntext              = result
          otext              = code
        EXCEPTIONS
          enqueue_table_full = 1
          include_enqueued   = 2
          include_readerror  = 3
          include_writeerror = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( |Error pretty printing code: { program_name }| ).
      ENDIF.
    ELSE.
      result = code.
    ENDIF.

  ENDMETHOD.


  METHOD read.

    DATA program TYPE progname.

    IF program_source IS INITIAL.
      SELECT SINGLE name FROM trdir INTO @program WHERE name = @program_name.
      IF sy-subrc = 0.
        READ REPORT program_name INTO result.
        IF sy-subrc <> 0.
          zcx_abappm_error=>raise( |Error reading program { program_name }| ).
        ENDIF.
      ENDIF.
    ELSE.
      result = program_source.
    ENDIF.

  ENDMETHOD.


  METHOD scan.

    DATA statements TYPE STANDARD TABLE OF sstmnt
      WITH KEY level struc from to number.

    DATA(source_code) = read(
      program_name    = program_name
      program_source  = program_source ).

    IF source_code IS INITIAL.
      RETURN.
      "zcx_abappm_error=>raise( |No code found in program { program_name }| )
    ENDIF.

    SCAN ABAP-SOURCE source_code
      TOKENS INTO result
      STATEMENTS INTO statements
      WITH ANALYSIS
      WITH COMMENTS
      WITHOUT TRMAC.

    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error scanning program { program_name }| ).
    ENDIF.

    IF statements IS INITIAL.
      zcx_abappm_error=>raise( |No statements found in program { program_name }| ).
    ENDIF.

    "   Process statements with
    "   - keywords (K)
    "   - short form of method call (A)
    "   - native SQL (E)
    "   - type-pools (T,V)
    "   - includes (I,J)
    "   - compute (C)
    "   - comments (P,S)
    "   - macros (R,D,M)
    "   - blank (N)
    "   - unknown (B,U)
    TRY.
        LOOP AT statements ASSIGNING FIELD-SYMBOL(<statement>) WHERE type CA 'KATC'.
          cl_abap_parser=>qualify_tokens(
            EXPORTING
              index_from     = <statement>-from
              index_to       = <statement>-to
              statement_type = <statement>-type
              simplified     = abap_false
            CHANGING
              stokesx_tab    = result ).
        ENDLOOP.
      CATCH cx_abap_parser INTO DATA(error).
        zcx_abappm_error=>raise( |Error parsing code: { error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
