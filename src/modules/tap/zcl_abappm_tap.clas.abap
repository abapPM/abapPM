CLASS zcl_abappm_tap DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* TAP for ABAP
*
* https://testanything.org/tap-version-14-specification.html
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

  PUBLIC SECTION.

    CONSTANTS:
      version     TYPE string VALUE '1.0.0' ##NEEDED,
      tap_version TYPE string VALUE 'TAP version 14' ##NO_TEXT.

    DATA testdoc TYPE string_table READ-ONLY.

    DATA:
      BEGIN OF testplan READ-ONLY,
        counter TYPE i,
        reason  TYPE string,
      END OF testplan.

    DATA:
      BEGIN OF testpoint READ-ONLY,
        id          TYPE i,
        description TYPE string,
        failed      TYPE abap_bool,
      END OF testpoint.

    DATA:
      BEGIN OF subtest READ-ONLY,
        description TYPE string,
        tap         TYPE REF TO zcl_abappm_tap,
        failed      TYPE abap_bool,
      END OF subtest.

    DATA:
      BEGIN OF options READ-ONLY,
        logging   TYPE abap_bool,
        snapshot  TYPE abap_bool,
        tolerance TYPE f,
      END OF options.

    METHODS constructor
      IMPORTING
        logging      TYPE abap_bool DEFAULT abap_false
        snapshot     TYPE abap_bool DEFAULT abap_false
        snap_include TYPE progname OPTIONAL
        snap_package TYPE devclass OPTIONAL
        snap_title   TYPE sy-title OPTIONAL
        tolerance    TYPE f DEFAULT cl_abap_unit_assert=>rtol_default.

    METHODS plan
      IMPORTING
        count         TYPE i
        reason        TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS end
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS bailout
      IMPORTING
        reason        TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS passing
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS comment
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS pass
      IMPORTING
        msg           TYPE csequence OPTIONAL
        skip          TYPE abap_bool OPTIONAL
        todo          TYPE abap_bool OPTIONAL
          PREFERRED PARAMETER msg
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS fail
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS skip
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS todo
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    " Actual

    METHODS _
      IMPORTING
        t             TYPE any
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    " Asserts

    METHODS abort
      IMPORTING
        msg TYPE csequence OPTIONAL.

    METHODS true
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS ok "same as true
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS false
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS not_ok "same as false
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS equals
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS eq "same as equals
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS differs
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS ne "same as differs
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS equals_float
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS eq_f "same as equals_float
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS cp
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS np
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS cs
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS ns
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS error
      IMPORTING
        msg           TYPE csequence
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS initial
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS not_initial
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS bound
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS not_bound
      IMPORTING
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS contains
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap ##SHADOW[CONTAINS].

    METHODS not_contains
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS matches
      IMPORTING
        regex         TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap ##SHADOW[MATCHES].

    METHODS re "same as matches
      IMPORTING
        regex         TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS return_code
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS rc "same as return_code
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS subrc
      IMPORTING
        exp           TYPE sy-subrc DEFAULT 0
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS index
      IMPORTING
        exp           TYPE sy-index
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS tabix
      IMPORTING
        exp           TYPE sy-tabix
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS throws
      IMPORTING
        t   TYPE any ##NEEDED
        msg TYPE csequence OPTIONAL.

    METHODS does_not_throw
      IMPORTING
        t             TYPE any ##NEEDED
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS type
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS kind
      IMPORTING
        exp           TYPE any
        msg           TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    " Subtests

    METHODS test_begin
      IMPORTING
        description   TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    METHODS test_end
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tap.

    " Snapshots

    METHODS snap_begin
      IMPORTING
        id          TYPE csequence
        description TYPE csequence OPTIONAL.

    METHODS snap_write
      IMPORTING
        line TYPE string OPTIONAL
        list TYPE string_table OPTIONAL
          PREFERRED PARAMETER line.

    METHODS snap_end
      IMPORTING
        msg TYPE csequence OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA _subrc TYPE sy-subrc.
    DATA _index TYPE sy-index.
    DATA _tabix TYPE sy-tabix.

    DATA act TYPE REF TO data.

    DATA snapshot TYPE REF TO lcl_snapshot.

    METHODS snap_init
      IMPORTING
        include TYPE progname OPTIONAL
        package TYPE devclass OPTIONAL
        title   TYPE sy-title DEFAULT 'TAP Snapshot' ##NO_TEXT.

    METHODS esc
      IMPORTING
        msg           TYPE string
        prefix        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_abappm_tap IMPLEMENTATION.


  METHOD abort.
    cl_abap_unit_assert=>abort( msg = msg ).
  ENDMETHOD.


  METHOD bailout.
    APPEND |Bail out! { esc( reason ) }| TO testdoc ##NO_TEXT.
    abort( reason ).
  ENDMETHOD.


  METHOD bound.
    cl_abap_unit_assert=>assert_bound(
      act = act
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD comment.

    DATA warning TYPE REF TO if_aunit_info_warning.

    IF options-logging = abap_true.
      APPEND |# { msg }| TO testdoc.
    ELSE.
      " LOG is an enhancement to add comments to unit test output (ZTAP_INFO)
      TRY.
          CALL METHOD cl_aunit_warning_c=>('log')
            EXPORTING
              msg    = msg
            RECEIVING
              result = warning.

          cl_aunit_failure_handler=>raise_warning( warning ).
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD constructor.
    options-logging   = logging.
    options-snapshot  = snapshot.
    options-tolerance = tolerance.

    snap_init(
      include = snap_include
      package = snap_package
      title   = snap_title ).

    " New TAP testdoc begins with version
    APPEND tap_version TO testdoc.
  ENDMETHOD.


  METHOD contains.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_table_contains(
      table = <act>
      line  = exp
      msg   = msg ).

    result = me.
  ENDMETHOD.


  METHOD cp.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_char_cp(
      act = <act>
      exp = exp
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD cs.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( <act> CS exp )
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD differs.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    TRY.
        cl_abap_unit_assert=>assert_differs(
          act = <act>
          exp = exp
          msg = msg ).
      CATCH cx_sy_dyn_call_illegal_type.
        " Different types means pass the test!
    ENDTRY.

    result = me.
  ENDMETHOD.


  METHOD does_not_throw.
    " If we get here, the call didn't raise an exception. Therefore, pass the test
    result = _( t = t ).
  ENDMETHOD.


  METHOD end.
    plan( count = testplan-counter reason = testplan-reason ).
  ENDMETHOD.


  METHOD eq.
    result = equals( exp = exp msg = msg ).
  ENDMETHOD.


  METHOD equals.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = exp
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD equals_float.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_equals_float(
      act  = <act>
      exp  = exp
      rtol = options-tolerance
      msg  = msg ).

    result = me.
  ENDMETHOD.


  METHOD eq_f.
    result = equals_float( exp = exp msg = msg ).
  ENDMETHOD.


  METHOD error.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    IF <act> IS INSTANCE OF cx_root.
      cl_abap_unit_assert=>fail( msg = msg ).
    ENDIF.

    result = me.
  ENDMETHOD.


  METHOD esc.
    result = replace( val = replace( val = msg sub = '\' with = '\\' occ = 0 ) sub = '#' with = '\#' occ = 0 ).
    IF prefix IS NOT INITIAL.
      result = prefix && result.
    ENDIF.
  ENDMETHOD.


  METHOD fail.
    APPEND |not ok { testpoint-id }{ esc( msg = testpoint-description prefix = ` - ` ) }| TO testdoc ##NO_TEXT.
    CLEAR testpoint.
    cl_abap_unit_assert=>fail( msg = msg ).
    result = me.
  ENDMETHOD.


  METHOD false.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_false(
      act = <act>
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD index.
    cl_abap_unit_assert=>assert_equals(
      act = _index
      exp = exp
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD initial.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_initial(
      act = <act>
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD kind.
    DATA(type_descr) = cl_abap_typedescr=>describe_by_data_ref( act ).
    ASSERT type_descr IS BOUND.

    CASE to_upper( exp ).
      WHEN 'CLAS' OR 'CLASS'.
        DATA(exp_kind) = cl_abap_typedescr=>kind_class.
      WHEN 'ELEM' OR 'ELEMENTARY'.
        exp_kind = cl_abap_typedescr=>kind_elem.
      WHEN 'INTF' OR 'INTERFACE'.
        exp_kind = cl_abap_typedescr=>kind_intf.
      WHEN 'REF' OR 'REFERENCE'.
        exp_kind = cl_abap_typedescr=>kind_ref.
      WHEN 'STRU' OR 'STRUCTURE'.
        exp_kind = cl_abap_typedescr=>kind_struct.
      WHEN 'TABL' OR 'TABLE'.
        exp_kind = cl_abap_typedescr=>kind_table.
      WHEN OTHERS.
        exp_kind = exp.
    ENDCASE.

    cl_abap_unit_assert=>assert_equals(
      act = type_descr->kind
      exp = exp_kind
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD matches.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_text_matches(
      text    = <act>
      pattern = regex
      msg     = msg ).

    result = me.
  ENDMETHOD.


  METHOD ne.
    result = differs( exp = exp msg = msg ).
  ENDMETHOD.


  METHOD not_bound.
    cl_abap_unit_assert=>assert_not_bound(
      act = act
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD not_contains.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_table_not_contains(
      table = <act>
      line  = exp
      msg  = msg ).

    result = me.
  ENDMETHOD.


  METHOD not_initial.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_not_initial(
      act = <act>
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD not_ok.
    result = false( msg = msg ).
  ENDMETHOD.


  METHOD np.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_char_np(
      act = <act>
      exp = exp
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD ns.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( <act> NS exp )
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD ok.
    result = true( msg = msg ).
  ENDMETHOD.


  METHOD pass.
    DATA(directive) =
      COND #( WHEN skip = abap_true THEN ` # SKIP` ELSE
      COND #( WHEN todo = abap_true THEN ` # TODO` ) ) ##NO_TEXT.

    APPEND |ok { testpoint-id }{ esc( msg = testpoint-description prefix = ` - ` ) }{ directive }| TO testdoc ##NO_TEXT.
    CLEAR testpoint.
    result = me.
  ENDMETHOD.


  METHOD passing.
    result = xsdbool( testpoint-failed = abap_false ).
  ENDMETHOD.


  METHOD plan.
    APPEND |1..{ count }{ esc( msg = reason prefix = ` # ` ) }| TO testdoc.
    CLEAR testplan.
  ENDMETHOD.


  METHOD rc.
    result = return_code( exp = exp msg = msg ).
  ENDMETHOD.


  METHOD re.
    result = matches( regex = regex msg = msg ).
  ENDMETHOD.


  METHOD return_code.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_return_code(
      act = <act>
      exp = exp
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD skip.
    pass( msg = msg skip = abap_true ).
    cl_abap_unit_assert=>skip( msg = msg ).
    result = me.
  ENDMETHOD.


  METHOD snap_begin.
    ASSERT snapshot IS BOUND.

    snapshot->begin( id = id text = description ).
  ENDMETHOD.


  METHOD snap_end.
    ASSERT snapshot IS BOUND.

    IF options-snapshot = abap_true.
      " Create/update snapshot
      snapshot->end( ).
    ELSE.
      " Compare to snapshot
      cl_abap_unit_assert=>assert_equals(
        act = snapshot->actual( )
        exp = snapshot->expected( )
        msg = msg ).
    ENDIF.
  ENDMETHOD.


  METHOD snap_init.
    snapshot = NEW lcl_snapshot(
      include = include
      package = package
      title   = title
      new     = options-snapshot ).
  ENDMETHOD.


  METHOD snap_write.
    ASSERT snapshot IS BOUND.

    snapshot->write( line = line list = list ).
  ENDMETHOD.


  METHOD subrc.
    sy-subrc = _subrc.

    cl_abap_unit_assert=>assert_subrc(
      exp = exp
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD tabix.
    cl_abap_unit_assert=>assert_equals(
      act = _tabix
      exp = exp
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD test_begin.
    CLEAR subtest.
    subtest-description = description.
    subtest-tap = NEW #(
      logging   = options-logging
      snapshot  = abap_false
      tolerance = options-tolerance ).

    APPEND |# Subtest: { esc( msg = description ) }| TO testdoc ##NO_TEXT.

    result = subtest-tap.
  ENDMETHOD.


  METHOD test_end.
    ASSERT subtest-tap IS BOUND.

    LOOP AT subtest-tap->testdoc ASSIGNING FIELD-SYMBOL(<line>).
      APPEND |    { <line> }| TO testdoc. " indent 4
    ENDLOOP.

    IF subtest-tap->passing( ).
      pass( subtest-description ).
    ELSE.
      fail( subtest-description ).
    ENDIF.

    result = me.
  ENDMETHOD.


  METHOD throws.
    " If we get here, the call didn't raise an exception. Therefore, fail the test
    cl_abap_unit_assert=>fail( msg = msg ).
  ENDMETHOD.


  METHOD todo.
    pass( msg = msg todo = abap_true ).
    result = me.
  ENDMETHOD.


  METHOD true.
    ASSIGN act->* TO FIELD-SYMBOL(<act>).
    ASSERT <act> IS ASSIGNED.

    cl_abap_unit_assert=>assert_true(
      act = <act>
      msg = msg ).

    result = me.
  ENDMETHOD.


  METHOD type.
    DATA(type_descr) = cl_abap_typedescr=>describe_by_data_ref( act ).
    ASSERT type_descr IS BOUND.

    CASE to_upper( exp ).
      WHEN 'ANY'.
        DATA(exp_type) = cl_abap_typedescr=>typekind_any.
      WHEN 'C' OR 'CHAR'.
        exp_type = cl_abap_typedescr=>typekind_char.
      WHEN 'CLAS' OR 'CLASS'.
        exp_type = cl_abap_typedescr=>typekind_class.
      WHEN 'CLIKE'.
        "exp_type = cl_abap_typedescr=>typekind_clike
        exp_type = cl_abap_typedescr=>typekind_char && cl_abap_typedescr=>typekind_num
                && cl_abap_typedescr=>typekind_string && cl_abap_typedescr=>typekind_date
                && cl_abap_typedescr=>typekind_time.
      WHEN 'CSEQUENCE'.
        "exp_type = cl_abap_typedescr=>typekind_csequence
        exp_type = cl_abap_typedescr=>typekind_char && cl_abap_typedescr=>typekind_string.
      WHEN 'DATA'.
        "exp_type = cl_abap_typedescr=>typekind_data.
        exp_type = cl_abap_typedescr=>typekind_any.
      WHEN 'D' OR 'DATE' OR 'DATS' OR 'DATN'.
        exp_type = cl_abap_typedescr=>typekind_date.
      WHEN 'DECFLOAT'.
        "exp_type = cl_abap_typedescr=>typekind_decfloat
        exp_type = cl_abap_typedescr=>typekind_decfloat16 && cl_abap_typedescr=>typekind_decfloat34.
      WHEN 'DECFLOAT16'.
        exp_type = cl_abap_typedescr=>typekind_decfloat16.
      WHEN 'DECFLOAT34'.
        exp_type = cl_abap_typedescr=>typekind_decfloat34.
      WHEN 'DREF'.
        exp_type = cl_abap_typedescr=>typekind_dref.
      WHEN 'F' OR 'FLOAT' OR 'FLTP'.
        exp_type = cl_abap_typedescr=>typekind_float.
      WHEN 'X' OR 'HEX' OR 'RAW'.
        exp_type = cl_abap_typedescr=>typekind_hex.
      WHEN 'B' OR 'I1' OR 'INT1'.
        exp_type = cl_abap_typedescr=>typekind_int1.
      WHEN 'S' OR 'I2' OR 'INT2'.
        exp_type = cl_abap_typedescr=>typekind_int2.
      WHEN 'I' OR 'I4' OR 'INT' OR 'INT4' OR 'INTEGER'.
        exp_type = cl_abap_typedescr=>typekind_int.
      WHEN 'I8' OR 'INT8'.
        exp_type = cl_abap_typedescr=>typekind_int8.
      WHEN 'INTF' OR 'INTERFACE'.
        exp_type = cl_abap_typedescr=>typekind_intf.
      WHEN 'IREF'.
        exp_type = cl_abap_typedescr=>typekind_iref.
      WHEN 'N' OR 'NUMC'.
        exp_type = cl_abap_typedescr=>typekind_num.
      WHEN 'NUMERIC'.
        "exp_type = cl_abap_typedescr=>typekind_numeric
        exp_type = cl_abap_typedescr=>typekind_int1 && cl_abap_typedescr=>typekind_int2
                && cl_abap_typedescr=>typekind_int && cl_abap_typedescr=>typekind_int8
                && cl_abap_typedescr=>typekind_packed && cl_abap_typedescr=>typekind_float
                && cl_abap_typedescr=>typekind_decfloat16 && cl_abap_typedescr=>typekind_decfloat34.
      WHEN 'OREF' OR 'OBJ' OR 'OBJECT'.
        exp_type = cl_abap_typedescr=>typekind_oref.
      WHEN 'P' OR 'PACKED' OR 'DEC'.
        exp_type = cl_abap_typedescr=>typekind_packed.
      WHEN 'SIMPLE'.
        exp_type = cl_abap_typedescr=>typekind_simple.
      WHEN 'STR' OR 'STRG' OR 'STRING'.
        exp_type = cl_abap_typedescr=>typekind_string.
      WHEN 'STRU' OR 'STRUCT'.
        exp_type = cl_abap_typedescr=>typekind_struct1 && cl_abap_typedescr=>typekind_struct2.
      WHEN 'TAB' OR 'TABL' OR 'TABLE'.
        exp_type = cl_abap_typedescr=>typekind_table.
      WHEN 'T' OR 'TIME' OR 'TIMS' OR 'TIMN'.
        exp_type = cl_abap_typedescr=>typekind_time.
      WHEN 'UTC' OR 'UTCLONG'.
        exp_type = cl_abap_typedescr=>typekind_utclong.
      WHEN 'W' OR 'LCHR'.
        exp_type = cl_abap_typedescr=>typekind_w.
      WHEN 'XSEQUENCE'.
        "exp_type = cl_abap_typedescr=>typekind_xsequence
        exp_type = cl_abap_typedescr=>typekind_hex && cl_abap_typedescr=>typekind_xstring.
      WHEN 'XSTR' OR 'XSTRING' OR 'RSTR'.
        exp_type = cl_abap_typedescr=>typekind_xstring.
      WHEN 'BREF'.
        exp_type = cl_abap_typedescr=>typekind_bref.
      WHEN 'ENUM'.
        exp_type = cl_abap_typedescr=>typekind_enum.
      WHEN OTHERS.
        exp_type = exp.
    ENDCASE.

    IF exp_type <> cl_abap_typedescr=>typekind_any.
      cl_abap_unit_assert=>assert_true(
        act = xsdbool( type_descr->type_kind CA exp_type )
        msg = msg ).
    ENDIF.

    result = me.
  ENDMETHOD.


  METHOD _.
    _subrc = sy-subrc ##NEEDED.
    _index = sy-index ##NEEDED.
    _tabix = sy-tabix ##NEEDED.

    GET REFERENCE OF t INTO act.

    result = me.
  ENDMETHOD.
ENDCLASS.
