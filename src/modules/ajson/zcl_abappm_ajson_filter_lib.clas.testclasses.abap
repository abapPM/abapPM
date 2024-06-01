class ltcl_filters_test definition final
  for testing
  risk level harmless
  duration short.
  private section.
    methods empty_filter_simple for testing raising ZCX_ABAPPM_AJSON_ERROR.
    methods empty_filter_deep for testing raising ZCX_ABAPPM_AJSON_ERROR.
    methods path_filter for testing raising ZCX_ABAPPM_AJSON_ERROR.
    methods path_filter_string for testing raising ZCX_ABAPPM_AJSON_ERROR.
    methods path_filter_w_patterns for testing raising ZCX_ABAPPM_AJSON_ERROR.
    methods path_filter_deep for testing raising ZCX_ABAPPM_AJSON_ERROR.
    methods and_filter for testing raising ZCX_ABAPPM_AJSON_ERROR.
endclass.


class ltcl_filters_test implementation.

  method empty_filter_simple.

    data li_json type ref to ZIF_ABAPPM_AJSON.
    data li_json_filtered type ref to ZIF_ABAPPM_AJSON.

    li_json = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ).
    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b'
      iv_val  = '' ).
    li_json->set(
      iv_path = '/c'
      iv_val  = '3' ).
    li_json->set(
      iv_path = '/d'
      iv_val  = 0 ).

    li_json_filtered = ZCL_ABAPPM_AJSON=>CREATE_FROM(
      ii_source_json = li_json
      ii_filter = ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_EMPTY_FILTER( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json_filtered->stringify( )
      exp = '{"a":"1","c":"3"}' ).

  endmethod.

  method empty_filter_deep.

    data li_json type ref to ZIF_ABAPPM_AJSON.
    data li_json_filtered type ref to ZIF_ABAPPM_AJSON.

    li_json = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ).
    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '' ).
    li_json->set(
      iv_path = '/b/d'
      iv_val  = 0 ).
    li_json->set(
      iv_path = '/d/e'
      iv_val  = 0 ).

    li_json_filtered = ZCL_ABAPPM_AJSON=>CREATE_FROM(
      ii_source_json = li_json
      ii_filter = ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_EMPTY_FILTER( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json_filtered->stringify( )
      exp = '{"a":"1"}' ).

  endmethod.

  method path_filter.

    data li_json type ref to ZIF_ABAPPM_AJSON.
    data li_json_filtered type ref to ZIF_ABAPPM_AJSON.
    data lt_paths type string_table.

    append '/b/c' to lt_paths.

    li_json = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ).
    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '2' ).
    li_json->set(
      iv_path = '/c/d'
      iv_val  = '3' ).

    li_json_filtered = ZCL_ABAPPM_AJSON=>CREATE_FROM(
      ii_source_json = li_json
      ii_filter = ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER( it_skip_paths = lt_paths ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json_filtered->stringify( )
      exp = '{"a":"1","b":{},"c":{"d":"3"}}' ).

  endmethod.

  method path_filter_string.

    data li_json type ref to ZIF_ABAPPM_AJSON.
    data li_json_filtered type ref to ZIF_ABAPPM_AJSON.

    li_json = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ).
    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '2' ).
    li_json->set(
      iv_path = '/c/d'
      iv_val  = '3' ).

    li_json_filtered = ZCL_ABAPPM_AJSON=>CREATE_FROM(
      ii_source_json = li_json
      ii_filter = ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER( iv_skip_paths = '/b/c,/c/d' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json_filtered->stringify( )
      exp = '{"a":"1","b":{},"c":{}}' ).

  endmethod.

  method path_filter_w_patterns.

    data li_json type ref to ZIF_ABAPPM_AJSON.
    data li_json_filtered type ref to ZIF_ABAPPM_AJSON.

    li_json = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ).
    li_json->set(
      iv_path = '/@meta'
      iv_val  = 'meta' ).
    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '2' ).
    li_json->set(
      iv_path = '/c/d'
      iv_val  = '3' ).
    li_json->set(
      iv_path = '/c/@meta2'
      iv_val  = 'meta2' ).

    li_json_filtered = ZCL_ABAPPM_AJSON=>CREATE_FROM(
      ii_source_json = li_json
      ii_filter = ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER(
        iv_skip_paths = '/*/c,*/@*'
        iv_pattern_search = abap_true ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json_filtered->stringify( )
      exp = '{"a":"1","b":{},"c":{"d":"3"}}' ).

  endmethod.

  method path_filter_deep.

    data li_json type ref to ZIF_ABAPPM_AJSON.
    data li_json_filtered type ref to ZIF_ABAPPM_AJSON.
    data lt_paths type string_table.

    append '/b' to lt_paths.

    li_json = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ).
    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b/c'
      iv_val  = '2' ).
    li_json->set(
      iv_path = '/b/d'
      iv_val  = 'x' ).
    li_json->set(
      iv_path = '/c/d'
      iv_val  = '3' ).

    li_json_filtered = ZCL_ABAPPM_AJSON=>CREATE_FROM(
      ii_source_json = li_json
      ii_filter = ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER( it_skip_paths = lt_paths ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json_filtered->stringify( )
      exp = '{"a":"1","c":{"d":"3"}}' ).

  endmethod.

  method and_filter.

    data li_json type ref to ZIF_ABAPPM_AJSON.
    data li_json_filtered type ref to ZIF_ABAPPM_AJSON.
    data lt_filters type ZIF_ABAPPM_AJSON_FILTER=>TY_FILTER_TAB.

    append ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_EMPTY_FILTER( ) to lt_filters.
    append ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER( iv_skip_paths = '/c' ) to lt_filters.

    li_json = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ).
    li_json->set(
      iv_path = '/a'
      iv_val  = '1' ).
    li_json->set(
      iv_path = '/b'
      iv_val  = '' ).
    li_json->set(
      iv_path = '/c'
      iv_val  = '3' ).
    li_json->set(
      iv_path = '/d'
      iv_val  = 0 ).

    li_json_filtered = ZCL_ABAPPM_AJSON=>CREATE_FROM(
      ii_source_json = li_json
      ii_filter = ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_AND_FILTER( lt_filters ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json_filtered->stringify( )
      exp = '{"a":"1"}' ).

  endmethod.

endclass.
