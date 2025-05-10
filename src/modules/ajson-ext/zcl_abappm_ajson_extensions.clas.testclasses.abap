CLASS ltcl_ajson_libs DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_test,
        a_b   TYPE i,
        _bb_c TYPE i,
        cc_dd TYPE i,
        _zz   TYPE i,
      END OF ty_test.

    DATA:
      abap_test TYPE ty_test,
      json_test TYPE string.

    METHODS:
      setup,
      to_camel_case_underscore FOR TESTING RAISING zcx_ajson_error,
      from_camel_case_underscore FOR TESTING RAISING zcx_ajson_error,
      empty_zero_null_simple FOR TESTING RAISING zcx_ajson_error,
      empty_zero_null_deep FOR TESTING RAISING zcx_ajson_error.

ENDCLASS.


CLASS ltcl_ajson_libs IMPLEMENTATION.

  METHOD setup.

    abap_test-a_b   = 1.
    abap_test-_bb_c = 2.
    abap_test-cc_dd = 3.
    abap_test-_zz   = 4.

    json_test = '{"aB":1,"_bbC":2,"ccDd":3,"_zz":4}'.

  ENDMETHOD.

  "
  " MAPPINGS
  "
  METHOD to_camel_case_underscore.

    DATA json_result TYPE string.

    json_result = zcl_ajson=>new(
      )->keep_item_order(
      )->set(
        iv_path = '/'
        iv_val  = abap_test
      )->map( zcl_ajson_extensions=>to_camel_case_underscore( )
      )->stringify( ).

    cl_abap_unit_assert=>assert_equals(
      act = json_result
      exp = json_test ).

  ENDMETHOD.

  METHOD from_camel_case_underscore.

    DATA abap_result TYPE ty_test.
    DATA ajson TYPE REF TO zif_ajson.

    ajson = zcl_ajson=>parse( json_test )->map( zcl_ajson_extensions=>from_camel_case_underscore( ) ).

    ajson->to_abap( IMPORTING ev_container = abap_result ).

    cl_abap_unit_assert=>assert_equals(
      act = abap_result
      exp = abap_test ).

  ENDMETHOD.

  "
  " FILTERS
  "
  METHOD empty_zero_null_simple.

    DATA:
      test TYPE REF TO zif_ajson,
      act  TYPE REF TO zif_ajson.

    test = zcl_ajson=>create_empty(
      )->set(
        iv_path = '/a'
        iv_val  = '1'
      )->set(
        iv_path = '/b'
        iv_val  = ''
      )->set_null( '/c'
      )->set(
        iv_path = '/d'
        iv_val  = 0 ).

    act = zcl_ajson=>create_from(
      ii_source_json = test
      ii_filter = zcl_ajson_extensions=>filter_empty_zero_null( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = act->stringify( )
      exp = '{"a":"1"}' ).

  ENDMETHOD.

  METHOD empty_zero_null_deep.

    DATA:
      test TYPE REF TO zif_ajson,
      act  TYPE REF TO zif_ajson.

    test = zcl_ajson=>create_empty(
      )->set(
        iv_path = '/a'
        iv_val  = '1'
      )->set(
        iv_path = '/b/c'
        iv_val  = ''
      )->set_null( '/b/d'
      )->set(
        iv_path = '/d/e'
        iv_val  = 0 ).

    act = zcl_ajson=>create_from(
      ii_source_json = test
      ii_filter = zcl_ajson_extensions=>filter_empty_zero_null( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = act->stringify( )
      exp = '{"a":"1"}' ).

  ENDMETHOD.

ENDCLASS.
