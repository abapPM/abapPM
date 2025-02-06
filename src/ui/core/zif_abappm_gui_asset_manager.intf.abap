INTERFACE zif_abappm_gui_asset_manager PUBLIC.

  TYPES:
    BEGIN OF ty_web_asset,
      url          TYPE string,
      type         TYPE c LENGTH 50,
      subtype      TYPE c LENGTH 50,
      content      TYPE xstring,
      is_cacheable TYPE abap_bool,
    END OF ty_web_asset,
    ty_web_assets TYPE STANDARD TABLE OF ty_web_asset WITH DEFAULT KEY.

  METHODS get_all_assets
    RETURNING
      VALUE(rt_assets) TYPE ty_web_assets
    RAISING
      zcx_abappm_error.

  METHODS get_asset
    IMPORTING
      !iv_url         TYPE string
    RETURNING
      VALUE(rs_asset) TYPE ty_web_asset
    RAISING
      zcx_abappm_error.

  METHODS get_text_asset
    IMPORTING
      !iv_url            TYPE string
      !iv_assert_subtype TYPE string OPTIONAL
    RETURNING
      VALUE(rv_asset)    TYPE string
    RAISING
      zcx_abappm_error.

  METHODS register_asset
    IMPORTING
      !iv_url       TYPE string
      !iv_type      TYPE string
      !iv_cacheable TYPE abap_bool DEFAULT abap_true
      !iv_mime_name TYPE wwwdatatab-objid OPTIONAL
      !iv_base64    TYPE string OPTIONAL
      !iv_inline    TYPE string OPTIONAL
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
