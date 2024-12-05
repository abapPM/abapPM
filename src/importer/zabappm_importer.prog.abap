************************************************************************
* apm Importer
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* This standalone tool imports all modules of a given package.
* Currently, the individual modules must be installed as global classes.
* The global classes are then copied and renamed according to the
* IMPORT statements found in sub-packages. Dependencies between the
* imported modules are taken into consideration.
*
* The tool will eventually become the "Update Command" and will be
* integrated into the apm UI.
************************************************************************
REPORT zabappm_importer LINE-SIZE 255.

TABLES: tadir.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS:
    p_pack TYPE tadir-devclass DEFAULT '$ABAPPM'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
  SELECT-OPTIONS:
    so_type FOR tadir-object,
    so_name FOR tadir-obj_name.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
  PARAMETERS:
    p_defrul TYPE string DEFAULT '(?:\/.*\/|Y|Z)(..)(.*)',
    p_notest AS CHECKBOX DEFAULT 'X',
    p_pretty AS CHECKBOX DEFAULT '',
    p_log    AS CHECKBOX DEFAULT 'X',
    p_test   AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b3.

CONSTANTS c_width TYPE i VALUE 150.

TYPES:
  BEGIN OF ty_program,
    program      TYPE progname,
    package      TYPE devclass,
    package_from TYPE devclass,
  END OF ty_program,
  ty_programs TYPE STANDARD TABLE OF ty_program WITH KEY program,
  BEGIN OF ty_mapping,
    type        TYPE tadir-object,
    old_object  TYPE tadir-obj_name,
    new_object  TYPE tadir-obj_name,
    package_src TYPE devclass,
    package_tgt TYPE devclass,
    name        TYPE string,
    version     TYPE string,
  END OF ty_mapping,
  ty_mappings      TYPE STANDARD TABLE OF ty_mapping WITH KEY old_object,
  ty_objtype_range TYPE RANGE OF tadir-object,
  ty_objname_range TYPE RANGE OF tadir-obj_name.

CLASS lcl_code DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS scan
      IMPORTING
        !iv_program   TYPE progname
        !it_source    TYPE seop_source_string OPTIONAL
      RETURNING
        VALUE(result) TYPE stokesx_tab
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS import
      IMPORTING
        !iv_program   TYPE progname
        !it_source    TYPE seop_source_string OPTIONAL
        !it_map       TYPE ty_mappings
      RETURNING
        VALUE(result) TYPE seop_source_string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS read
      IMPORTING
        !iv_program   TYPE progname
        !it_source    TYPE seop_source_string OPTIONAL
      RETURNING
        VALUE(result) TYPE seop_source_string
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    CLASS-METHODS get_object_name_from_token
      IMPORTING
        !iv_token     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_object_name_from_abapdoc
      IMPORTING
        !iv_token     TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS lcl_code IMPLEMENTATION.

  METHOD read.

    DATA lv_program TYPE progname.

    IF it_source IS INITIAL.
      SELECT SINGLE name FROM trdir INTO lv_program WHERE name = iv_program.
      IF sy-subrc = 0.
        READ REPORT iv_program INTO result.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error reading program { iv_program }| ).
        ENDIF.
      ENDIF.
    ELSE.
      result = it_source.
    ENDIF.

  ENDMETHOD.

  METHOD scan.

    DATA:
      lt_source TYPE seop_source_string,
      lt_stmnt  TYPE TABLE OF sstmnt.

    lt_source = read(
      iv_program = iv_program
      it_source  = it_source ).

    IF lt_source IS INITIAL.
      RETURN.
      "zcx_abapgit_exception=>raise( |No code found in program { iv_program }| )
    ENDIF.

    SCAN ABAP-SOURCE lt_source
      TOKENS      INTO result
      STATEMENTS  INTO lt_stmnt
      WITH ANALYSIS
      WITH COMMENTS
      WITHOUT TRMAC.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error scanning code { iv_program }| ).
    ENDIF.

    IF lt_stmnt IS INITIAL.
      RETURN.
      "zcx_abapgit_exception=>raise( |No statements found in program { iv_program }| )
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
        LOOP AT lt_stmnt ASSIGNING FIELD-SYMBOL(<ls_stmnt>) WHERE type CA 'KATC'.
          cl_abap_parser=>qualify_tokens(
            EXPORTING
              index_from     = <ls_stmnt>-from
              index_to       = <ls_stmnt>-to
              statement_type = <ls_stmnt>-type
              simplified     = abap_false
            CHANGING
              stokesx_tab    = result ).
        ENDLOOP.
      CATCH cx_abap_parser INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( |Error parsing code: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD import.

    CONSTANTS:
      lc_token_types TYPE string VALUE 'HIlm34C'.

    DATA:
      lt_token    TYPE stokesx_tab,
      lt_token_ck TYPE stokesx_tab.

    result = read(
      iv_program = iv_program
      it_source  = it_source ).

    lt_token = scan(
      iv_program = iv_program
      it_source  = result ).

    IF lt_token IS INITIAL.
      RETURN.
    ENDIF.

    " Collect all tokens that have a mapping
    LOOP AT lt_token ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE type CA lc_token_types.

      IF <ls_token>-type = 'C'.
        " abapDoc comments
        READ TABLE it_map ASSIGNING FIELD-SYMBOL(<ls_map>)
          WITH TABLE KEY old_object = get_object_name_from_abapdoc( <ls_token>-str ).
        IF sy-subrc = 0.
          <ls_token>-str = replace(
            val  = <ls_token>-str
            sub  = <ls_map>-old_object
            with = to_lower( <ls_map>-new_object )
            case = abap_false ).
          INSERT <ls_token> INTO TABLE lt_token_ck.
        ENDIF.
      ELSE.
        " Other statements
        READ TABLE it_map ASSIGNING <ls_map>
          WITH TABLE KEY old_object = get_object_name_from_token( <ls_token>-str ).
        IF sy-subrc = 0.
          <ls_token>-str = replace(
            val  = <ls_token>-str
            sub  = <ls_map>-old_object
            with = <ls_map>-new_object
            case = abap_false ).
          INSERT <ls_token> INTO TABLE lt_token_ck.
        ENDIF.
      ENDIF.

    ENDLOOP.

    " New tokens might be different in length. Sort tokens back to front
    " so source can be adjusted without problems
    SORT lt_token_ck BY row DESCENDING col DESCENDING.

    LOOP AT lt_token_ck ASSIGNING <ls_token>.

      READ TABLE result ASSIGNING FIELD-SYMBOL(<lv_source>) INDEX <ls_token>-row.
      ASSERT sy-subrc = 0.

      <lv_source> =
        substring( val = <lv_source> len = <ls_token>-col ) && <ls_token>-str &&
        substring( val = <lv_source> off = <ls_token>-col + <ls_token>-len1 ).

      IF strlen( <lv_source> ) > 255.
        zcx_abapgit_exception=>raise( |Line length overflow in { iv_program }, Line { <ls_token>-row }| ).
      ENDIF.

    ENDLOOP.

    IF p_pretty = abap_true.
      CALL FUNCTION 'PRETTY_PRINTER'
        EXPORTING
          inctoo             = abap_false
        TABLES
          ntext              = result
          otext              = result
        EXCEPTIONS
          enqueue_table_full = 1
          include_enqueued   = 2
          include_readerror  = 3
          include_writeerror = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error pretty printing code: { iv_program }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_object_name_from_token.

    DATA lv_rest TYPE string.

    result = iv_token.

    IF result CS '~'.
      " zif_test~method
      SPLIT result AT '~' INTO result lv_rest.
    ENDIF.
    IF result CS '('.
      " zcl_test( )
      SPLIT result AT '(' INTO result lv_rest.
    ENDIF.

    IF result CS '=>'.
      " zcl_test=>method
      SPLIT result AT '=' INTO result lv_rest.
    ELSEIF result CS '->'.
      " zcl_test->method
      SPLIT result AT '>' INTO lv_rest result.
    ENDIF.

  ENDMETHOD.

  METHOD get_object_name_from_abapdoc.

    result = iv_token.

    " Example:
    " "! @raising zcx_test_error | Exception
    FIND REGEX '"! @raising (.*) \|' IN iv_token SUBMATCHES result.
    IF sy-subrc = 0.
      result = to_upper( result ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_mapping DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !is_program   TYPE ty_program
        !ir_objtype   TYPE ty_objtype_range
        !ir_objname   TYPE ty_objname_range
      RETURNING
        VALUE(result) TYPE ty_mappings
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_rule,
        old_object TYPE string,
        new_object TYPE string,
        name       TYPE string,
        version    TYPE string,
      END OF ty_rule,
      ty_rules TYPE STANDARD TABLE OF ty_rule WITH DEFAULT KEY.

    CLASS-METHODS get_import_rules
      IMPORTING
        !iv_program   TYPE progname
      RETURNING
        VALUE(result) TYPE ty_rules
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_tadir_objects
      IMPORTING
        !iv_from      TYPE devclass
        !ir_objtype   TYPE ty_objtype_range
        !ir_objname   TYPE ty_objname_range
      RETURNING
        VALUE(result) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_mapping IMPLEMENTATION.

  METHOD get.

    DATA:
      lv_found      TYPE abap_bool,
      lv_import_all TYPE abap_bool,
      ls_rule       TYPE ty_rule,
      lt_rules      TYPE ty_rules,
      ls_map        TYPE ty_mapping,
      lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS:
      <ls_rule>  LIKE LINE OF lt_rules,
      <ls_tadir> LIKE LINE OF lt_tadir.

    IF p_log = abap_true.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'Include:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      WRITE: / is_program-program.
      SKIP.
    ENDIF.

    " Get list of rules for importing objects
    lt_rules = get_import_rules( is_program-program ).

    READ TABLE lt_rules ASSIGNING <ls_rule> WITH KEY old_object = '*'.
    IF sy-subrc = 0.
      lv_import_all = abap_true.
      <ls_rule>-old_object = p_defrul.
    ENDIF.

    " Get list of original objects
    lt_tadir = get_tadir_objects(
      iv_from    = is_program-package_from
      ir_objtype = ir_objtype
      ir_objname = ir_objname ).

    " Process all objects and apply rules
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CLEAR ls_map.
      ls_map-package_src = is_program-package_from.
      ls_map-package_tgt = is_program-package.
      ls_map-type        = <ls_tadir>-object.
      ls_map-old_object  = <ls_tadir>-obj_name.

      " Collect first matching rule
      lv_found = abap_false.
      LOOP AT lt_rules INTO ls_rule.
        ls_map-new_object = <ls_tadir>-obj_name.
        IF ls_rule-old_object = <ls_tadir>-obj_name.
          " Direct mapping
          ls_map-new_object = ls_rule-new_object.
        ELSE.
          " Regex replacement
          ls_map-new_object = replace(
            val   = <ls_tadir>-obj_name
            regex = ls_rule-old_object
            with  = ls_rule-new_object ).
        ENDIF.
        IF ls_map-new_object <> <ls_tadir>-obj_name AND strlen( ls_map-new_object ) <= 30.
          lv_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_found = abap_true.
        INSERT ls_map INTO TABLE result.
      ELSEIF lv_import_all = abap_true.
        " With IMPORT '*' we expect all objects to be mapped
        " Most likely, the object name became too long requiring an additional rule
        zcx_abapgit_exception=>raise( |No mapping rule found for { <ls_tadir>-obj_name }| ).
      ENDIF.
    ENDLOOP.

    IF p_log = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Mapping:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<ls_map>).
        WRITE: / <ls_map>-package_src, <ls_map>-package_tgt, <ls_map>-old_object, <ls_map>-new_object.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.

  METHOD get_import_rules.

    DATA:
      lv_tabix TYPE sy-tabix,
      lv_pos   TYPE string,
      lt_token TYPE TABLE OF stokesx,
      ls_rule  TYPE ty_rule.

    FIELD-SYMBOLS:
      <ls_token> TYPE stokesx.

    lt_token = lcl_code=>scan( iv_program ).

    " Process all IMPORT statements
    "
    " Example 1:
    "
    " IMPORT zif_ajson TO zif_my_app_ajson FROM '@sbcgua/ajson'.
    "
    " IMPORT  c
    " ZIF_AJSON  r
    " TO  b
    " ZIF_MY_APP_AJSON  m
    " FROM  b
    " '@sbcgua/ajson'  !
    "
    " Example 2:
    "
    " IMPORT '*' TO 'z$1_my_app$2' FROM '@sbcgua/ajson'.
    "
    " IMPORT  c
    " '*'  !
    " TO  b
    " 'z$1_my_app$2'  m
    " FROM  b
    " '@sbcgua/ajson'  m
    LOOP AT lt_token TRANSPORTING NO FIELDS WHERE type = 'c' AND str = 'IMPORT'.
      lv_tabix = sy-tabix.
      CLEAR ls_rule.
      DO 5 TIMES.
        lv_pos = |Tabix { lv_tabix } Index { sy-index }|.
        READ TABLE lt_token ASSIGNING <ls_token> INDEX lv_tabix + sy-index.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error parsing IMPORT statement. { lv_pos }| ).
        ENDIF.

        CASE sy-index.
          WHEN 1.
            CASE <ls_token>-type.
              WHEN 'r'. " reference
                ls_rule-old_object = <ls_token>-str.
              WHEN '!' OR 'm'. " literal
                ls_rule-old_object = to_upper( replace(
                  val  = <ls_token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
              WHEN OTHERS.
                zcx_abapgit_exception=>raise( |Unknown identifier { <ls_token>-str }. { lv_pos }| ).
            ENDCASE.
          WHEN 2.
            IF <ls_token>-type <> 'b' OR <ls_token>-str <> 'TO'.
              zcx_abapgit_exception=>raise( |Error parsing IMPORT statement. Expecting "TO". { lv_pos }| ).
            ENDIF.
          WHEN 3.
            CASE <ls_token>-type.
              WHEN 'r'. " reference
                ls_rule-new_object = <ls_token>-str.
              WHEN '!' OR 'm'. " literal
                ls_rule-new_object = to_upper( replace(
                  val  = <ls_token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
              WHEN OTHERS.
                zcx_abapgit_exception=>raise( |Unknown identifier { <ls_token>-str }. { lv_pos }| ).
            ENDCASE.
          WHEN 4.
            IF <ls_token>-type <> 'b' OR <ls_token>-str <> 'FROM'.
              zcx_abapgit_exception=>raise( |Error parsing IMPORT statement. Expecting "FROM". { lv_pos }| ).
            ENDIF.
          WHEN 5.
            CASE <ls_token>-type.
              WHEN 'r'. " reference
                ls_rule-name = <ls_token>-str.
              WHEN '!' OR 'm'. " literal
                ls_rule-name = to_lower( replace(
                  val  = <ls_token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
              WHEN OTHERS.
                zcx_abapgit_exception=>raise( |Unknown identifier { <ls_token>-str }. { lv_pos }| ).
            ENDCASE.
            IF ls_rule-name+1(*) CS '@'.
              SPLIT ls_rule-name AT '@' INTO ls_rule-name ls_rule-version.
            ELSE.
              ls_rule-version = 'latest'.
            ENDIF.
        ENDCASE.
      ENDDO.

      INSERT ls_rule INTO TABLE result.
    ENDLOOP.

    IF p_log = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Rules:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<ls_rules>).
        WRITE: / <ls_rules>-old_object, AT 32 <ls_rules>-new_object, AT 64 <ls_rules>-name, <ls_rules>-version.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.

  METHOD get_tadir_objects.

    " Get list of original objects
    result = zcl_abapgit_factory=>get_tadir( )->read( iv_package = iv_from ).

    " Only classes and interfaces
    " FUTURE: support other object types
    DELETE result WHERE NOT ( object = 'CLAS' OR object = 'INTF' ).

    DELETE result WHERE NOT ( object IN ir_objtype AND obj_name IN ir_objname ).

    IF p_log = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Objects:' COLOR COL_NORMAL, AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<ls_tadir>).
        WRITE: / <ls_tadir>-object, <ls_tadir>-obj_name.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_class DEFINITION INHERITING FROM zcl_abapgit_oo_class.

  PUBLIC SECTION.

    METHODS import
      IMPORTING
        !iv_package TYPE devclass
        !iv_class   TYPE seoclsname
        !it_map     TYPE ty_mappings
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    METHODS source
      IMPORTING
        !iv_class     TYPE seoclsname
      RETURNING
        VALUE(result) TYPE seop_source_string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD import.

    DATA:
      lv_class_new             TYPE seoclsname,
      ls_clskey                TYPE seoclskey,
      ls_vseoclass             TYPE vseoclass,
      lv_program               TYPE program,
      lt_source                TYPE seop_source_string,
      lt_local_definitions     TYPE seop_source_string,
      lt_local_implementations TYPE seop_source_string,
      lt_local_macros          TYPE seop_source_string,
      lt_test_classes          TYPE seop_source_string.

    lv_class_new = it_map[ old_object = iv_class ]-new_object.
    ASSERT lv_class_new IS NOT INITIAL.

    IF iv_class = lv_class_new.
      zcx_abapgit_exception=>raise( 'New class name must be different' ).
    ENDIF.

    " Get old class
    ls_clskey-clsname = iv_class.
    ls_vseoclass      = zif_abapgit_oo_object_fnc~get_class_properties( ls_clskey ).

    IF ls_vseoclass IS INITIAL.
      zcx_abapgit_exception=>raise( 'Not found' ).
    ENDIF.

    " Rename and create new class
    ls_clskey-clsname    = lv_class_new.
    ls_vseoclass-clsname = lv_class_new.

    zif_abapgit_oo_object_fnc~create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseoclass ).

    " Import code and apply mapping of old to new names
    lt_source = source( iv_class ).

    lt_source = lcl_code=>import(
      iv_program = cl_oo_classname_service=>get_classpool_name( iv_class )
      it_source  = lt_source
      it_map     = it_map ).

    IF p_test IS INITIAL.
      zif_abapgit_oo_object_fnc~deserialize_source(
        iv_package = iv_package
        iv_version = ls_vseoclass-unicode
        is_key     = ls_clskey
        it_source  = lt_source ).
    ENDIF.

    lt_local_definitions = lcl_code=>import(
      iv_program = cl_oo_classname_service=>get_ccdef_name( iv_class )
      it_map     = it_map ).

    lt_local_implementations = lcl_code=>import(
      iv_program = cl_oo_classname_service=>get_ccimp_name( iv_class )
      it_map     = it_map ).

    lt_local_macros = lcl_code=>import(
      iv_program = cl_oo_classname_service=>get_ccmac_name( iv_class )
      it_map     = it_map ).

    IF p_notest IS INITIAL.
      lt_test_classes = lcl_code=>import(
        iv_program = cl_oo_classname_service=>get_ccau_name( iv_class )
        it_map     = it_map ).
    ENDIF.

    IF p_test IS INITIAL.
      zif_abapgit_oo_object_fnc~generate_locals(
        iv_package               = iv_package
        iv_version               = ls_vseoclass-unicode
        is_key                   = ls_clskey
        it_local_definitions     = lt_local_definitions
        it_local_implementations = lt_local_implementations
        it_local_macros          = lt_local_macros
        it_local_test_classes    = lt_test_classes ).
    ENDIF.

  ENDMETHOD.

  METHOD source.

    DATA:
      lo_source   TYPE REF TO if_oo_clif_source,
      lo_instance TYPE REF TO cl_oo_factory.

    TRY.
        CALL METHOD cl_oo_factory=>create_instance
          RECEIVING
            result = lo_instance.

        CALL METHOD lo_instance->create_clif_source
          EXPORTING
            clif_name = iv_class
            version   = 'A'
          RECEIVING
            result    = lo_source.

        CALL METHOD lo_source->get_source
          IMPORTING
            source = result.
      CATCH cx_root INTO DATA(lx_error).
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_interface DEFINITION INHERITING FROM zcl_abapgit_oo_interface.

  PUBLIC SECTION.

    METHODS import
      IMPORTING
        !iv_package   TYPE devclass
        !iv_interface TYPE seoclsname
        !it_map       TYPE ty_mappings
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_interface IMPLEMENTATION.

  METHOD import.

    DATA:
      lv_interface_new TYPE seoclsname,
      ls_clskey        TYPE seoclskey,
      ls_vseointerf    TYPE vseointerf,
      lv_program       TYPE program,
      lt_source        TYPE seop_source_string.

    lv_interface_new = it_map[ old_object = iv_interface ]-new_object.
    ASSERT lv_interface_new IS NOT INITIAL.

    IF iv_interface = lv_interface_new.
      zcx_abapgit_exception=>raise( 'New interface name must be different' ).
    ENDIF.

    " Get old interface
    ls_clskey-clsname = iv_interface.
    ls_vseointerf     = zif_abapgit_oo_object_fnc~get_interface_properties( ls_clskey ).

    IF ls_vseointerf IS INITIAL.
      zcx_abapgit_exception=>raise( 'Not found' ).
    ENDIF.

    " Rename and create new interface
    ls_clskey-clsname     = lv_interface_new.
    ls_vseointerf-clsname = lv_interface_new.

    zif_abapgit_oo_object_fnc~create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseointerf ).

    " Import code and apply mapping of old to new names
    lt_source = lcl_code=>import(
      iv_program = cl_oo_classname_service=>get_intfsec_name( iv_interface )
      it_map     = it_map ).

    IF p_test IS INITIAL.
      zif_abapgit_oo_object_fnc~deserialize_source(
        iv_package = iv_package
        iv_version = ls_vseointerf-unicode
        is_key     = ls_clskey
        it_source  = lt_source ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA:
    gt_programs  TYPE ty_programs,
    gt_map       TYPE ty_mappings,
    go_class     TYPE REF TO lcl_class,
    go_interface TYPE REF TO lcl_interface.

  DATA(lo_timer) = zcl_abapgit_timer=>create( 'apm Import' ).
  lo_timer->start( ).

  TRY.
      IF p_log = abap_true.
        FORMAT COLOR COL_HEADING.
        WRITE: / 'Searching for IMPORTs:', AT c_width space.
        SKIP.
        FORMAT COLOR OFF.
      ENDIF.

      DATA(lt_packages) = zcl_abapgit_factory=>get_sap_package( p_pack )->list_subpackages( ).

      LOOP AT lt_packages INTO DATA(lv_package) WHERE table_line <> p_pack.
        " Find INCLUDE containing IMPORT statements
        DATA lt_programs TYPE ty_programs.

        IF p_log = abap_true.
          WRITE: / 'PACKAGE', lv_package.
        ENDIF.

        CLEAR lt_programs.
        SELECT a~obj_name a~devclass INTO TABLE lt_programs FROM tadir AS a JOIN trdir AS b ON a~obj_name = b~name
          WHERE a~pgmid = 'R3TR' AND a~object = 'PROG' AND a~devclass = lv_package AND b~subc = 'I'.
        CHECK sy-subrc = 0 AND lt_programs IS NOT INITIAL.

        LOOP AT lt_programs ASSIGNING FIELD-SYMBOL(<ls_program>).
          IF p_log = abap_true.
            WRITE: AT /5 'INCLUDE', <ls_program>-program.
          ENDIF.

          DATA(lt_source) = lcl_code=>read( <ls_program>-program ).
          FIND FIRST OCCURRENCE OF REGEX |IMPORT.*TO.*FROM '(.*)'| IN TABLE lt_source SUBMATCHES DATA(lv_name).
          IF sy-subrc = 0.
            IF p_log = abap_true.
              WRITE: AT /10 'IMPORT', lv_name COLOR COL_POSITIVE.
            ENDIF.

            DATA(lv_apm_pack) = 'PACKAGE:%:PACKAGE_JSON'.
            DATA(lv_apm_name) = |"name": "{ lv_name }|.
            DATA ls_zabappm TYPE zabappm.
            DATA lv_found TYPE abap_bool.
            CLEAR lv_found.
            SELECT * FROM zabappm INTO ls_zabappm WHERE keys LIKE lv_apm_pack.
              IF ls_zabappm-value CS lv_apm_name.
                lv_found = abap_true.
                EXIT.
              ENDIF.
            ENDSELECT.

            IF lv_found = abap_true.
              SPLIT ls_zabappm-keys AT ':' INTO DATA(lv_1) DATA(lv_from) DATA(lv_2).
              ASSERT sy-subrc = 0.

              IF p_log = abap_true.
                WRITE: AT /15 'FOUND IN', lv_from COLOR COL_POSITIVE.
              ENDIF.

              <ls_program>-package_from = lv_from.
              INSERT <ls_program> INTO TABLE gt_programs.
            ELSE.
              IF p_log = abap_true.
                WRITE: AT /15 'NOT FOUND' COLOR COL_NEGATIVE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      LOOP AT gt_programs ASSIGNING <ls_program>.
        APPEND LINES OF lcl_mapping=>get(
          is_program = <ls_program>
          ir_objtype = so_type[]
          ir_objname = so_name[] ) TO gt_map.
      ENDLOOP.

      IF p_log = abap_true.
        FORMAT COLOR COL_HEADING.
        WRITE: / 'Importing:', AT c_width space.
        SKIP.
        FORMAT COLOR OFF.
      ENDIF.

      LOOP AT gt_map INTO DATA(gs_map).

        IF p_log = abap_true.
          WRITE: / 'IMPORT', gs_map-type, gs_map-old_object, 'TO', gs_map-new_object, 'IN PACKAGE', gs_map-package_tgt.
        ENDIF.

        CASE gs_map-type.
          WHEN 'CLAS'.
            CREATE OBJECT go_class.

            go_class->import(
              iv_package = gs_map-package_tgt
              iv_class   = |{ gs_map-old_object }|
              it_map     = gt_map ).

          WHEN 'INTF'.
            CREATE OBJECT go_interface.

            go_interface->import(
              iv_package   = gs_map-package_tgt
              iv_interface = |{ gs_map-old_object }|
              it_map       = gt_map ).

          WHEN OTHERS.
            zcx_abapgit_exception=>raise( |Unknow type { gs_map-type }| ).
        ENDCASE.

      ENDLOOP.

    CATCH zcx_abapgit_exception INTO DATA(gx_error).
      MESSAGE gx_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

  IF p_log = abap_true.
    SKIP.
    WRITE: / lo_timer->end( abap_true ).
  ENDIF.
