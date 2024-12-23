class ZCL_ABAPPM_AJSON_MAPPING definition
  public
  final
  create public.

  public section.

    constants:
      begin of rename_by,
        attr_name type i value 0,
        full_path type i value 1,
        pattern type i value 2,
        " regex type i value 3, " TODO add if needed in future
      end of rename_by.

    class-methods create_camel_case " DEPRECATED
      importing
        it_mapping_fields   type ZIF_ABAPPM_AJSON_MAPPING=>TY_MAPPING_FIELDS optional
        iv_first_json_upper type abap_bool default abap_true
      returning
        value(ri_mapping)   type ref to ZIF_ABAPPM_AJSON_MAPPING.

    class-methods create_upper_case
      importing
        it_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING=>TY_MAPPING_FIELDS optional
      returning
        value(ri_mapping) type ref to ZIF_ABAPPM_AJSON_MAPPING.

    class-methods create_lower_case
      importing
        it_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING=>TY_MAPPING_FIELDS optional
      returning
        value(ri_mapping) type ref to ZIF_ABAPPM_AJSON_MAPPING.

    class-methods create_field_mapping " DEPRECATED
      importing
        it_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING=>TY_MAPPING_FIELDS
      returning
        value(ri_mapping) type ref to ZIF_ABAPPM_AJSON_MAPPING.

    class-methods create_rename
      importing
        it_rename_map type ZIF_ABAPPM_AJSON_MAPPING=>TTY_RENAME_MAP
        iv_rename_by type i default rename_by-attr_name
      returning
        value(ri_mapping) type ref to ZIF_ABAPPM_AJSON_MAPPING.

    class-methods create_compound_mapper
      importing
        ii_mapper1 type ref to ZIF_ABAPPM_AJSON_MAPPING optional
        ii_mapper2 type ref to ZIF_ABAPPM_AJSON_MAPPING optional
        ii_mapper3 type ref to ZIF_ABAPPM_AJSON_MAPPING optional
        it_more type ZIF_ABAPPM_AJSON_MAPPING=>TY_TABLE_OF optional
      returning
        value(ri_mapping) type ref to ZIF_ABAPPM_AJSON_MAPPING.

    class-methods create_to_snake_case
      returning
        value(ri_mapping) type ref to ZIF_ABAPPM_AJSON_MAPPING.

    class-methods create_to_camel_case
      importing
        iv_first_json_upper type abap_bool default abap_false
      returning
        value(ri_mapping) type ref to ZIF_ABAPPM_AJSON_MAPPING.

  protected section.

  private section.

ENDCLASS.



CLASS ZCL_ABAPPM_AJSON_MAPPING IMPLEMENTATION.


  method create_camel_case.

    create object ri_mapping type lcl_mapping_camel
      exporting
        it_mapping_fields   = it_mapping_fields
        iv_first_json_upper = iv_first_json_upper.

  endmethod.


  method create_compound_mapper.

    data lt_queue type ZIF_ABAPPM_AJSON_MAPPING=>TY_TABLE_OF.

    append ii_mapper1 to lt_queue.
    append ii_mapper2 to lt_queue.
    append ii_mapper3 to lt_queue.
    append lines of it_more to lt_queue.
    delete lt_queue where table_line is initial.

    create object ri_mapping type lcl_compound_mapper
      exporting
        it_queue = lt_queue.

  endmethod.


  method create_field_mapping.

    create object ri_mapping type lcl_mapping_fields
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


  method create_lower_case.

    create object ri_mapping type lcl_mapping_to_lower
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.


  method create_rename.

    create object ri_mapping type lcl_rename
      exporting
        it_rename_map = it_rename_map
        iv_rename_by = iv_rename_by.

  endmethod.


  method create_to_camel_case.

    create object ri_mapping type lcl_to_camel
      exporting
        iv_first_json_upper = iv_first_json_upper.

  endmethod.


  method create_to_snake_case.

    create object ri_mapping type lcl_to_snake.

  endmethod.


  method create_upper_case.

    create object ri_mapping type lcl_mapping_to_upper
      exporting
        it_mapping_fields = it_mapping_fields.

  endmethod.
ENDCLASS.
