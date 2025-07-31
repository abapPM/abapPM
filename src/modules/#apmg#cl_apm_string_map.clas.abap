CLASS /apmg/cl_apm_string_map DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS version TYPE string VALUE 'v1.0.5'.
    CONSTANTS origin TYPE string VALUE 'https://github.com/sbcgua/abap-string-map'.
    CONSTANTS license TYPE string VALUE 'MIT'.

    TYPES:
      BEGIN OF ty_entry,
        k TYPE string,
        v TYPE string,
      END OF ty_entry.
    TYPES:
      tty_entries TYPE STANDARD TABLE OF ty_entry WITH KEY k.
    TYPES:
      tts_entries TYPE SORTED TABLE OF ty_entry WITH NON-UNIQUE KEY k.

    DATA mt_entries TYPE tts_entries READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        !iv_case_insensitive TYPE abap_bool DEFAULT abap_false
        !iv_list_mode        TYPE abap_bool DEFAULT abap_false " removes uniqueness requirement,
        " use with care: it is not the primary scenario
        !iv_from             TYPE any OPTIONAL
          PREFERRED PARAMETER iv_from
      RETURNING
        VALUE(ro_instance)   TYPE REF TO /apmg/cl_apm_string_map.
    METHODS constructor
      IMPORTING
        !iv_case_insensitive TYPE abap_bool DEFAULT abap_false
        !iv_list_mode        TYPE abap_bool DEFAULT abap_false " removes uniqueness requirement,
        " use with care: it is not the primary scenario
        !iv_from             TYPE any OPTIONAL.

    METHODS get
      IMPORTING
        !iv_key       TYPE clike
      RETURNING
        VALUE(rv_val) TYPE string.
    METHODS has
      IMPORTING
        !iv_key       TYPE clike
      RETURNING
        VALUE(rv_has) TYPE abap_bool.
    METHODS set
      IMPORTING
        !iv_key       TYPE clike
        !iv_val       TYPE clike
      RETURNING
        VALUE(ro_map) TYPE REF TO /apmg/cl_apm_string_map.
    METHODS setx
      IMPORTING
        !iv_str       TYPE csequence
      RETURNING
        VALUE(ro_map) TYPE REF TO /apmg/cl_apm_string_map.
    METHODS size
      RETURNING
        VALUE(rv_size) TYPE i.
    METHODS is_empty
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.
    METHODS delete
      IMPORTING
        !iv_key TYPE clike.
    METHODS keys
      RETURNING
        VALUE(rt_keys) TYPE string_table.
    METHODS values
      RETURNING
        VALUE(rt_values) TYPE string_table.
    METHODS clear.

    METHODS from_struc
      IMPORTING
        !is_container      TYPE any
      RETURNING
        VALUE(ro_instance) TYPE REF TO /apmg/cl_apm_string_map.
    METHODS from_entries
      IMPORTING
        !it_entries        TYPE ANY TABLE
      RETURNING
        VALUE(ro_instance) TYPE REF TO /apmg/cl_apm_string_map.
    METHODS from_string
      IMPORTING
        !iv_string_params  TYPE csequence
      RETURNING
        VALUE(ro_instance) TYPE REF TO /apmg/cl_apm_string_map.
    METHODS from_map
      IMPORTING
        !io_string_map     TYPE REF TO /apmg/cl_apm_string_map
      RETURNING
        VALUE(ro_instance) TYPE REF TO /apmg/cl_apm_string_map.
    METHODS merge
      IMPORTING
        !io_string_map     TYPE REF TO /apmg/cl_apm_string_map
      RETURNING
        VALUE(ro_instance) TYPE REF TO /apmg/cl_apm_string_map.

    METHODS to_struc
      CHANGING
        !cs_container TYPE any.
    METHODS to_string
      RETURNING
        VALUE(rv_string) TYPE string.
    METHODS to_entries
      CHANGING
        !ct_entries TYPE STANDARD TABLE.

    METHODS strict
      IMPORTING
        !iv_strict         TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_instance) TYPE REF TO /apmg/cl_apm_string_map.
    METHODS freeze.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_is_strict TYPE abap_bool.
    DATA mv_read_only TYPE abap_bool.
    DATA mv_case_insensitive TYPE abap_bool.
    DATA mv_list_mode TYPE abap_bool.
ENDCLASS.



CLASS /apmg/cl_apm_string_map IMPLEMENTATION.


  METHOD clear.

    IF mv_read_only = abap_true.
      lcx_error=>raise( 'String map is read only' ).
    ENDIF.

    CLEAR mt_entries.

  ENDMETHOD.


  METHOD constructor.
    mv_is_strict = abap_true.
    mv_case_insensitive = iv_case_insensitive.
    mv_list_mode = iv_list_mode.

    IF iv_from IS NOT INITIAL.
      DATA lo_type TYPE REF TO cl_abap_typedescr.
      lo_type = cl_abap_typedescr=>describe_by_data( iv_from ).

      CASE lo_type->type_kind.
        WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
          me->from_struc( iv_from ).

        WHEN cl_abap_typedescr=>typekind_oref.
          DATA lo_from TYPE REF TO /apmg/cl_apm_string_map.
          TRY.
              lo_from ?= iv_from.
            CATCH cx_sy_move_cast_error.
              lcx_error=>raise( 'Incorrect string map instance to copy from' ).
          ENDTRY.

          IF mt_entries IS INITIAL AND mv_case_insensitive = abap_false.
            me->mt_entries = lo_from->mt_entries. " shortcut, maybe remove for safety
          ELSE.
            me->from_map( lo_from ).
          ENDIF.

        WHEN cl_abap_typedescr=>typekind_table.
          me->from_entries( iv_from ).

        WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_char.
          me->from_string( iv_from ).

        WHEN OTHERS.
          lcx_error=>raise( |Incorrect input for string_map=>create, typekind { lo_type->type_kind }| ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_list_mode        = iv_list_mode
        iv_case_insensitive = iv_case_insensitive
        iv_from             = iv_from.
  ENDMETHOD.


  METHOD delete.

    IF mv_read_only = abap_true.
      lcx_error=>raise( 'String map is read only' ).
    ENDIF.

    DATA lv_key TYPE string.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    DELETE mt_entries WHERE k = lv_key.

  ENDMETHOD.


  METHOD freeze.
    mv_read_only = abap_true.
  ENDMETHOD.


  METHOD from_entries.

    FIELD-SYMBOLS <i> TYPE ty_entry.

    LOOP AT it_entries ASSIGNING <i> CASTING.
      set(
        iv_key = <i>-k
        iv_val = <i>-v ).
    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.


  METHOD from_map.

    from_entries( io_string_map->mt_entries ).
    ro_instance = me.

  ENDMETHOD.


  METHOD from_string.

    IF iv_string_params IS INITIAL.
      RETURN.
    ENDIF.

    DATA lt_lines TYPE string_table.
    FIELD-SYMBOLS <i> LIKE LINE OF lt_lines.
    SPLIT iv_string_params AT ',' INTO TABLE lt_lines.

    DATA lv_key TYPE string.
    DATA lv_val TYPE string.

    LOOP AT lt_lines ASSIGNING <i>.
      SPLIT <i> AT '=' INTO lv_key lv_val.
      SHIFT lv_key RIGHT DELETING TRAILING space.
      SHIFT lv_key LEFT DELETING LEADING space.
      SHIFT lv_val RIGHT DELETING TRAILING space.
      SHIFT lv_val LEFT DELETING LEADING space.
      IF lv_key IS INITIAL.
        lcx_error=>raise( 'Empty key in initialization string is not allowed' ).
        " value can be initial, even a,b,c is ok to create sets
      ENDIF.
      set(
        iv_key = lv_key
        iv_val = lv_val ).
    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.


  METHOD from_struc.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <c> LIKE LINE OF lo_struc->components.
    FIELD-SYMBOLS <val> TYPE any.

    lo_type = cl_abap_typedescr=>describe_by_data( is_container ).
    IF lo_type->type_kind <> cl_abap_typedescr=>typekind_struct1
      AND lo_type->type_kind <> cl_abap_typedescr=>typekind_struct2.
      lcx_error=>raise( 'Only structures supported' ).
    ENDIF.

    lo_struc ?= lo_type.
    LOOP AT lo_struc->components ASSIGNING <c>.
      CHECK <c>-type_kind CO 'bsI8PaeFCNgXyDT'. " values
      ASSIGN COMPONENT <c>-name OF STRUCTURE is_container TO <val>.
      ASSERT sy-subrc = 0.
      set(
        iv_key = |{ <c>-name }|
        iv_val = |{ <val> }| ).
    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.


  METHOD get.

    DATA lv_key TYPE string.
    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    READ TABLE mt_entries ASSIGNING <entry> WITH KEY k = lv_key.
    IF sy-subrc = 0.
      rv_val = <entry>-v.
    ENDIF.

  ENDMETHOD.


  METHOD has.

    DATA lv_key TYPE string.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    READ TABLE mt_entries TRANSPORTING NO FIELDS WITH KEY k = lv_key.
    rv_has = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_empty.
    rv_yes = boolc( lines( mt_entries ) = 0 ).
  ENDMETHOD.


  METHOD keys.

    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.
    LOOP AT mt_entries ASSIGNING <entry>.
      APPEND <entry>-k TO rt_keys.
    ENDLOOP.

  ENDMETHOD.


  METHOD merge.

    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.

    LOOP AT io_string_map->mt_entries ASSIGNING <entry>.
      set(
        iv_key = <entry>-k
        iv_val = <entry>-v ).
    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.


  METHOD set.

    DATA ls_entry LIKE LINE OF mt_entries.
    DATA lv_key TYPE string.
    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.

    IF mv_read_only = abap_true.
      lcx_error=>raise( 'String map is read only' ).
    ENDIF.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    IF mv_list_mode = abap_true.
      ls_entry-k = lv_key.
      ls_entry-v = iv_val.
      INSERT ls_entry INTO TABLE mt_entries.
    ELSE.
      READ TABLE mt_entries ASSIGNING <entry> WITH KEY k = lv_key.
      IF sy-subrc = 0.
        <entry>-v = iv_val.
      ELSE.
        ls_entry-k = lv_key.
        ls_entry-v = iv_val.
        INSERT ls_entry INTO TABLE mt_entries.
      ENDIF.
    ENDIF.

    ro_map = me.

  ENDMETHOD.


  METHOD setx.

    DATA lv_key TYPE string.
    DATA lv_val TYPE string.

    ro_map = me.

    IF iv_str IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT iv_str AT ':' INTO lv_key lv_val.
    CONDENSE lv_key.
    CONDENSE lv_val.

    IF lv_key IS INITIAL.
      RETURN.
    ENDIF.

    set(
      iv_key = lv_key
      iv_val = lv_val ).

  ENDMETHOD.


  METHOD size.

    rv_size = lines( mt_entries ).

  ENDMETHOD.


  METHOD strict.
    mv_is_strict = iv_strict.
    ro_instance = me.
  ENDMETHOD.


  METHOD to_entries.

    DATA lo_ttype TYPE REF TO cl_abap_tabledescr.
    DATA lo_dtype TYPE REF TO cl_abap_datadescr.
    DATA lo_stype TYPE REF TO cl_abap_structdescr.

    lo_ttype ?= cl_abap_typedescr=>describe_by_data( ct_entries ).
    lo_dtype = lo_ttype->get_table_line_type( ).

    IF lo_dtype->kind <> cl_abap_typedescr=>kind_struct.
      lcx_error=>raise( 'Unsupported table line type' ).
    ENDIF.

    lo_stype ?= lo_dtype.

    IF lines( lo_stype->components ) <> 2.
      lcx_error=>raise( 'Wrong number of fields in target table (must be 2)' ).
    ENDIF.

    FIELD-SYMBOLS <c> LIKE LINE OF lo_stype->components.
    LOOP AT lo_stype->components ASSIGNING <c>.
      IF NOT ( <c>-type_kind = cl_abap_typedescr=>typekind_char OR <c>-type_kind = cl_abap_typedescr=>typekind_string ).
        lcx_error=>raise( 'Wrong type of fields in target table (must be char or string)' ).
      ENDIF.
    ENDLOOP.

    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <to> TYPE any.
    FIELD-SYMBOLS <k> TYPE any.
    FIELD-SYMBOLS <v> TYPE any.
    LOOP AT mt_entries ASSIGNING <entry>.
      APPEND INITIAL LINE TO ct_entries ASSIGNING <to>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT 1 OF STRUCTURE <to> TO <k>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT 2 OF STRUCTURE <to> TO <v>.
      ASSERT sy-subrc = 0.
      <k> = <entry>-k.
      <v> = <entry>-v.
    ENDLOOP.

  ENDMETHOD.


  METHOD to_string.

    DATA lv_size TYPE i.
    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.

    lv_size = lines( mt_entries ).
    LOOP AT mt_entries ASSIGNING <entry>.
      rv_string = rv_string && <entry>-k && '=' && <entry>-v.
      IF sy-tabix < lv_size.
        rv_string = rv_string && ','.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD to_struc.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    DATA lv_field TYPE string.
    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <val> TYPE any.

    lo_type = cl_abap_typedescr=>describe_by_data( cs_container ).
    IF lo_type->type_kind <> cl_abap_typedescr=>typekind_struct1
      AND lo_type->type_kind <> cl_abap_typedescr=>typekind_struct2.
      lcx_error=>raise( 'Only structures supported' ).
    ENDIF.

    lo_struc ?= lo_type.
    LOOP AT mt_entries ASSIGNING <entry>.
      lv_field = to_upper( <entry>-k ).
      ASSIGN COMPONENT lv_field OF STRUCTURE cs_container TO <val>.
      IF sy-subrc = 0.
        " TODO check target type ?
        <val> = <entry>-v.
      ELSEIF mv_is_strict = abap_false.
        CONTINUE.
      ELSE.
        lcx_error=>raise( |Component { lv_field } not found in target| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD values.

    FIELD-SYMBOLS <entry> LIKE LINE OF mt_entries.
    LOOP AT mt_entries ASSIGNING <entry>.
      APPEND <entry>-v TO rt_values.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
