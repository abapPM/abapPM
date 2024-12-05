interface ZIF_ABAPPM_AJSON
  public.

  constants version type string value 'v1.1.10'. "#EC NOTEXT
  constants origin type string value 'https://github.com/sbcgua/ajson'. "#EC NOTEXT
  constants license type string value 'MIT'. "#EC NOTEXT

  types:
    begin of ty_opts,
      read_only type abap_bool,
      keep_item_order type abap_bool,
      format_datetime type abap_bool,
      to_abap_corresponding_only type abap_bool,
    end of ty_opts.

  " DATA

  data mt_json_tree type ZIF_ABAPPM_AJSON_TYPES=>TY_NODES_TS read-only.

  " CLONING

  methods clone
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.
  methods filter
    importing
      ii_filter type ref to ZIF_ABAPPM_AJSON_FILTER
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.
  methods map
    importing
      ii_mapper type ref to ZIF_ABAPPM_AJSON_MAPPING
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  " METHODS

  methods freeze.
  methods keep_item_order
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON.
  methods format_datetime
    importing
      iv_use_iso type abap_bool default abap_true
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON.
  methods to_abap_corresponding_only
    importing
      iv_enable type abap_bool default abap_true
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON.
  methods opts
    returning
      value(rs_opts) type ty_opts.

  " METHODS ex.reader

  methods is_empty
    returning
      value(rv_yes) type abap_bool.

  methods exists
    importing
      iv_path type string
    returning
      value(rv_exists) type abap_bool.

  methods members
    importing
      iv_path type string
    returning
      value(rt_members) type string_table.

  methods get
    importing
      iv_path type string
    returning
      value(rv_value) type string.

  methods get_node_type
    importing
      iv_path type string
    returning
      value(rv_node_type) type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE_TYPE.

  methods get_boolean
    importing
      iv_path type string
    returning
      value(rv_value) type abap_bool.

  methods get_integer
    importing
      iv_path type string
    returning
      value(rv_value) type i.

  methods get_number
    importing
      iv_path type string
    returning
      value(rv_value) type f.

  methods get_date
    importing
      iv_path type string
    returning
      value(rv_value) type d.

  methods get_timestamp
    importing
      iv_path type string
    returning
      value(rv_value) type timestamp.

  methods get_string
    importing
      iv_path type string
    returning
      value(rv_value) type string.

  methods slice
    importing
      iv_path type string
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON.

  methods to_abap
    importing
      iv_corresponding type abap_bool default abap_false
    exporting
      ev_container type any
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods array_to_string_table
    importing
      iv_path type string
    returning
      value(rt_string_table) type string_table
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  " METHODS ex.writer

  methods clear
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods set
    importing
      iv_path type string
      iv_val type any
      iv_ignore_empty type abap_bool default abap_true
      iv_node_type type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE_TYPE optional
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods setx
    importing
      iv_param type string
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods set_boolean
    importing
      iv_path type string
      iv_val type any
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods set_string
    importing
      iv_path type string
      iv_val type clike
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods set_integer
    importing
      iv_path type string
      iv_val type i
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods set_date
    importing
      iv_path type string
      iv_val type d
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods set_timestamp
    importing
      iv_path type string
      iv_val type timestamp
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods set_null
    importing
      iv_path type string
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods delete
    importing
      iv_path type string
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods touch_array
    importing
      iv_path type string
      iv_clear type abap_bool default abap_false
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods push
    importing
      iv_path type string
      iv_val type any
    returning
      value(ri_json) type ref to ZIF_ABAPPM_AJSON
    raising
      ZCX_ABAPPM_AJSON_ERROR.

  methods stringify
    importing
      iv_indent type i default 0
      iv_trailing_comma type abap_bool default abap_false
        preferred parameter iv_indent
    returning
      value(rv_json) type string
    raising
      ZCX_ABAPPM_AJSON_ERROR.

endinterface.
