interface ZIF_ABAPPM_AJSON_MAPPING
  public.

  types:
    begin of ty_mapping_field, " deprecated, will be removed
      abap type string,
      json type string,
    end of ty_mapping_field,
    ty_mapping_fields type standard table of ty_mapping_field
      with unique sorted key abap components abap
      with unique sorted key json components json.

  types:
    begin of ty_rename,
      from type string,
      to type string,
    end of ty_rename,
    tty_rename_map type standard table of ty_rename
      with unique sorted key by_name components from.

  types:
    ty_table_of type standard table of ref to ZIF_ABAPPM_AJSON_MAPPING.

  methods to_abap " deprecated, will be removed
    importing
      !iv_path         type string
      !iv_name         type string
    returning
      value(rv_result) type string.

  methods to_json " deprecated, will be removed
    importing
      !iv_path         type string
      !iv_name         type string
    returning
      value(rv_result) type string.

  methods rename_node
    importing
      !is_node type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE
    changing
      !cv_name type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE-NAME.

endinterface.
