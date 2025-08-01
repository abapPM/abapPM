INTERFACE /apmg/if_apm_gui_services PUBLIC.

  METHODS cache_asset
    IMPORTING
      !iv_text      TYPE string OPTIONAL
      !iv_xdata     TYPE xstring OPTIONAL
      !iv_url       TYPE string OPTIONAL
      !iv_type      TYPE c
      !iv_subtype   TYPE c
    RETURNING
      VALUE(rv_url) TYPE string
    RAISING
      /apmg/cx_apm_error.

  " Notes:
  " - page_asset is supposed to be not cacheable
  " - add mime64 if needed (supposedly won't be needed)
  METHODS register_page_asset
    IMPORTING
      !iv_url       TYPE string
      !iv_type      TYPE string
      !iv_mime_name TYPE wwwdatatab-objid OPTIONAL
      !iv_inline    TYPE string OPTIONAL
    RAISING
      /apmg/cx_apm_error.

  METHODS register_event_handler
    IMPORTING
      !ii_event_handler TYPE REF TO /apmg/if_apm_gui_event_handler.

  METHODS get_current_page_name
    RETURNING
      VALUE(rv_page_name) TYPE string.

  METHODS get_hotkeys_ctl
    RETURNING
      VALUE(ri_hotkey_ctl) TYPE REF TO /apmg/if_apm_gui_hotkey_ctl.

  METHODS get_html_parts
    RETURNING
      VALUE(ro_parts) TYPE REF TO /apmg/cl_apm_html_parts.

  METHODS get_log
    IMPORTING
      !iv_create_new TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(ri_log)  TYPE REF TO zif_abapgit_log.

ENDINTERFACE.
