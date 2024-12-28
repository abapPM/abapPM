class ltcl_test_mappers definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      from_json_to_json for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_abap for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_json for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_json_nested_struc for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_json_nested_table for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_json_first_lower for testing raising ZCX_ABAPPM_AJSON_ERROR.

    methods:
      to_snake for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_camel for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_camel_1st_upper for testing raising ZCX_ABAPPM_AJSON_ERROR,
      rename_by_attr for testing raising ZCX_ABAPPM_AJSON_ERROR,
      rename_by_path for testing raising ZCX_ABAPPM_AJSON_ERROR,
      rename_by_pattern for testing raising ZCX_ABAPPM_AJSON_ERROR,
      compound_mapper for testing raising ZCX_ABAPPM_AJSON_ERROR,
      test_to_upper for testing raising ZCX_ABAPPM_AJSON_ERROR,
      test_to_lower for testing raising ZCX_ABAPPM_AJSON_ERROR.

endclass.


class ltcl_test_mappers implementation.


  method from_json_to_json.

    data:
      lo_ajson type ref to ZCL_ABAPPM_AJSON.

    lo_ajson =
        ZCL_ABAPPM_AJSON=>PARSE(
            iv_json           = `{"fieldData":"field_value"}`
            ii_custom_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_CAMEL_CASE( iv_first_json_upper = abap_false ) ).

    lo_ajson->set_string( iv_path = `/fieldData`  iv_val = 'E' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"E"}' ).

  endmethod.


  method to_abap.

    data:
      lo_ajson   type ref to ZCL_ABAPPM_AJSON,
      li_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_CAMEL_CASE( ).

    lo_ajson = ZCL_ABAPPM_AJSON=>PARSE( iv_json = '{"FieldData":"field_value"}' ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( importing ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-field_data
      exp = 'field_value' ).

  endmethod.


  method to_json.

    data:
      lo_ajson   type ref to ZCL_ABAPPM_AJSON,
      li_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_CAMEL_CASE( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.

    lo_ajson = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value"}' ).

  endmethod.


  method to_json_nested_struc.

    data:
      lo_ajson   type ref to ZCL_ABAPPM_AJSON,
      li_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING.
    data:
      begin of ls_result,
        field_data type string,
        begin of struc_data,
          field_more type string,
        end of struc_data,
      end of ls_result.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_CAMEL_CASE( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.
    ls_result-struc_data-field_more = 'field_more'.

    lo_ajson = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value","strucData":{"fieldMore":"field_more"}}' ).

  endmethod.


  method to_json_nested_table.

    data:
      lo_ajson   type ref to ZCL_ABAPPM_AJSON,
      li_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING.
    data:
      lv_value type string,
      begin of ls_result,
        field_data type string,
        begin of struc_data,
          field_more type string_table,
        end of struc_data,
      end of ls_result.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_CAMEL_CASE( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.
    lv_value = 'field_more'.
    insert lv_value into table ls_result-struc_data-field_more.

    lo_ajson = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value","strucData":{"fieldMore":["field_more"]}}' ).

  endmethod.


  method to_json_first_lower.

    data:
      lo_ajson   type ref to ZCL_ABAPPM_AJSON,
      li_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_CAMEL_CASE( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"FieldData":"field_value"}' ).

  endmethod.


  method test_to_upper.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"a":1,"b":{"c":2}}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_UPPER_CASE( ) )->stringify( )
      exp = '{"A":1,"B":{"C":2}}' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>PARSE( '{"a":1,"b":{"c":2}}'
        )->map( ZCL_ABAPPM_AJSON_MAPPING=>CREATE_UPPER_CASE( )
        )->stringify( )
      exp = '{"A":1,"B":{"C":2}}' ).

  endmethod.

  method test_to_lower.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"A":1,"B":{"C":2}}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_LOWER_CASE( ) )->stringify( )
      exp = '{"a":1,"b":{"c":2}}' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>PARSE( '{"A":1,"B":{"C":2}}'
        )->map( ZCL_ABAPPM_AJSON_MAPPING=>CREATE_LOWER_CASE( )
        )->stringify( )
      exp = '{"a":1,"b":{"c":2}}' ).

  endmethod.

  method rename_by_attr.

    data lt_map type ZIF_ABAPPM_AJSON_MAPPING=>TTY_RENAME_MAP.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = 'a'.
    <i>-to   = 'x'.
    append initial line to lt_map assigning <i>.
    <i>-from = 'c'.
    <i>-to   = 'y'.
    append initial line to lt_map assigning <i>.
    <i>-from = 'd'.
    <i>-to   = 'z'.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"a":1,"b":{"c":2},"d":{"e":3}}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_RENAME( lt_map
        ) )->stringify( )
      exp = '{"b":{"y":2},"x":1,"z":{"e":3}}' ).

  endmethod.

  method rename_by_path.

    data lt_map type ZIF_ABAPPM_AJSON_MAPPING=>TTY_RENAME_MAP.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = '/b/a'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"a":1,"b":{"a":2},"c":{"a":3}}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_RENAME(
          it_rename_map = lt_map
          iv_rename_by  = ZCL_ABAPPM_AJSON_MAPPING=>RENAME_BY-FULL_PATH
        ) )->stringify( )
      exp = '{"a":1,"b":{"x":2},"c":{"a":3}}' ).

  endmethod.

  method rename_by_pattern.

    data lt_map type ZIF_ABAPPM_AJSON_MAPPING=>TTY_RENAME_MAP.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = '/*/this*'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"andthisnot":1,"b":{"thisone":2},"c":{"a":3}}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_RENAME(
          it_rename_map = lt_map
          iv_rename_by  = ZCL_ABAPPM_AJSON_MAPPING=>RENAME_BY-PATTERN
        ) )->stringify( )
      exp = '{"andthisnot":1,"b":{"x":2},"c":{"a":3}}' ).

  endmethod.

  method compound_mapper.

    data lt_map type ZIF_ABAPPM_AJSON_MAPPING=>TTY_RENAME_MAP.
    field-symbols <i> like line of lt_map.

    append initial line to lt_map assigning <i>.
    <i>-from = '/b/a'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"a":1,"b":{"a":2},"c":{"a":3}}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_COMPOUND_MAPPER(
          ii_mapper1 = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_RENAME(
            it_rename_map = lt_map
            iv_rename_by  = ZCL_ABAPPM_AJSON_MAPPING=>RENAME_BY-FULL_PATH )
          ii_mapper2 = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_UPPER_CASE( ) )
        )->stringify( )
      exp = '{"A":1,"B":{"X":2},"C":{"A":3}}' ).

  endmethod.

  method to_snake.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"aB":1,"BbC":2,"cD":{"xY":3},"ZZ":4}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_SNAKE_CASE( )
        )->stringify( )
      exp = '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}' ).

  endmethod.

  method to_camel.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( )
        )->stringify( )
      exp = '{"aB":1,"bbC":2,"cD":{"xY":3},"zz":4}' ).

    " Forced underscore
    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"a__b":1}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( )
        )->stringify( )
      exp = '{"a_b":1}' ).

  endmethod.

  method to_camel_1st_upper.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_AJSON=>CREATE_FROM(
        ii_source_json = ZCL_ABAPPM_AJSON=>PARSE( '{"aj_bc":1,"bb_c":2,"c_d":{"xq_yq":3},"zz":4}' )
        ii_mapper      = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( iv_first_json_upper = abap_true )
        )->stringify( )
      exp = '{"AjBc":1,"BbC":2,"CD":{"XqYq":3},"Zz":4}' ).

  endmethod.

endclass.



class ltcl_fields definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_json_without_path for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_json_with_path for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_abap for testing raising ZCX_ABAPPM_AJSON_ERROR,
      to_json importing iv_path type string returning value(rv_result) type string raising ZCX_ABAPPM_AJSON_ERROR.


endclass.


class ltcl_fields implementation.


  method to_abap.

    data:
      lo_ajson          type ref to ZCL_ABAPPM_AJSON,
      li_mapping        type ref to ZIF_ABAPPM_AJSON_MAPPING,
      lt_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING=>TY_MAPPING_FIELDS,
      ls_mapping_field  like line of lt_mapping_fields.
    data:
      begin of ls_result,
        abap_field type string,
        field      type string,
      end of ls_result.

    clear ls_mapping_field.
    ls_mapping_field-abap  = 'ABAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    insert ls_mapping_field into table lt_mapping_fields.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_FIELD_MAPPING( lt_mapping_fields ).

    lo_ajson =
        ZCL_ABAPPM_AJSON=>PARSE( iv_json = '{"field":"value","json.field":"field_value"}' ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( importing ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-abap_field
      exp = 'field_value' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-field
      exp = 'value' ).

  endmethod.


  method to_json_without_path.

    cl_abap_unit_assert=>assert_equals(
      act = to_json( `/` )
      exp = '{"field":"value","json.field":"field_value"}' ).

  endmethod.


  method to_json_with_path.

    cl_abap_unit_assert=>assert_equals(
      act = to_json( '/samplePath' )
      exp = '{"samplePath":{"field":"value","json.field":"field_value"}}' ).

  endmethod.


  method to_json.

    data:
      lo_ajson          type ref to ZCL_ABAPPM_AJSON,
      li_mapping        type ref to ZIF_ABAPPM_AJSON_MAPPING,
      lt_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING=>TY_MAPPING_FIELDS,
      ls_mapping_field  like line of lt_mapping_fields.
    data:
      begin of ls_result,
        abap_field type string,
        field      type string,
      end of ls_result.

    clear ls_mapping_field.
    ls_mapping_field-abap  = 'ABAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    insert ls_mapping_field into table lt_mapping_fields.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_FIELD_MAPPING( lt_mapping_fields ).

    ls_result-abap_field = 'field_value'.
    ls_result-field      = 'value'.

    lo_ajson = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = iv_path iv_val = ls_result ).

    rv_result = lo_ajson->stringify( ).

  endmethod.


endclass.



class ltcl_to_lower definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_json for testing raising ZCX_ABAPPM_AJSON_ERROR.
endclass.


class ltcl_to_lower implementation.


  method to_json.

    data:
      lo_ajson   type ref to ZCL_ABAPPM_AJSON,
      li_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_LOWER_CASE( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"field_data":"field_value"}' ).

  endmethod.


endclass.



class ltcl_to_upper definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      to_json for testing raising ZCX_ABAPPM_AJSON_ERROR.
endclass.


class ltcl_to_upper implementation.


  method to_json.

    data:
      lo_ajson   type ref to ZCL_ABAPPM_AJSON,
      li_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING.
    data:
      begin of ls_result,
        field_data type string,
      end of ls_result.

    li_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_UPPER_CASE( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = ZCL_ABAPPM_AJSON=>CREATE_EMPTY( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/' iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"FIELD_DATA":"field_value"}' ).

  endmethod.


endclass.