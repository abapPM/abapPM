class lcl_mapping_fields definition.

  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.

    methods constructor
      importing
        it_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING~TY_MAPPING_FIELDS optional.

  protected section.

  private section.
    data mt_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING~TY_MAPPING_FIELDS.

endclass.

class lcl_rename definition.

  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.

    methods constructor
      importing
        it_rename_map type ZIF_ABAPPM_AJSON_MAPPING~TTY_RENAME_MAP
        iv_rename_by type i.

  protected section.

  private section.
    data mt_rename_map type ZIF_ABAPPM_AJSON_MAPPING~TTY_RENAME_MAP.
    data mv_rename_by type i.

endclass.

class lcl_mapping_to_upper definition.

  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.

    methods constructor
      importing
        it_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING~TY_MAPPING_FIELDS optional.

  protected section.

  private section.
    data mi_mapping_fields type ref to ZIF_ABAPPM_AJSON_MAPPING.

endclass.


class lcl_mapping_to_lower definition.

  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.

    methods constructor
      importing
        it_mapping_fields type ZIF_ABAPPM_AJSON_MAPPING~TY_MAPPING_FIELDS optional.

  protected section.

  private section.
    data mi_mapping_fields type ref to ZIF_ABAPPM_AJSON_MAPPING.

endclass.


class lcl_mapping_camel definition.

  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.

    methods constructor
      importing
        it_mapping_fields   type ZIF_ABAPPM_AJSON_MAPPING~TY_MAPPING_FIELDS optional
        iv_first_json_upper type abap_bool default abap_true.

  protected section.

  private section.
    data mv_first_json_upper type abap_bool.
    data mi_mapping_fields type ref to ZIF_ABAPPM_AJSON_MAPPING.

endclass.

class lcl_compound_mapper definition.

  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.

    methods constructor
      importing
        it_queue type ZIF_ABAPPM_AJSON_MAPPING=>TY_TABLE_OF.

  protected section.

  private section.
    data mt_queue type ZIF_ABAPPM_AJSON_MAPPING=>TY_TABLE_OF.

endclass.

class lcl_to_snake definition.
  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.
endclass.

class lcl_to_camel definition.
  public section.
    interfaces ZIF_ABAPPM_AJSON_MAPPING.
    methods constructor
      importing
        iv_first_json_upper type abap_bool.
  private section.
    data mv_first_json_upper type abap_bool.
endclass.
