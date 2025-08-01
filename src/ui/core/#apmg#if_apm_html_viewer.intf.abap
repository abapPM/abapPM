INTERFACE /apmg/if_apm_html_viewer PUBLIC.

  TYPES:
    ty_char256   TYPE c LENGTH 256,
    ty_post_data TYPE STANDARD TABLE OF ty_char256 WITH DEFAULT KEY,
    BEGIN OF ty_name_value,
      name  TYPE c LENGTH 30,
      value TYPE c LENGTH 250,
    END OF ty_name_value,
    ty_query_table TYPE STANDARD TABLE OF ty_name_value WITH DEFAULT KEY.

  CONSTANTS c_id_sapevent TYPE i VALUE 1 ##NO_TEXT.

  EVENTS sapevent
    EXPORTING
      VALUE(action) TYPE c OPTIONAL
      VALUE(frame) TYPE c OPTIONAL
      VALUE(getdata) TYPE c OPTIONAL
      VALUE(postdata) TYPE ty_post_data OPTIONAL
      VALUE(query_table) TYPE ty_query_table OPTIONAL.

  METHODS load_data
    IMPORTING
      !iv_url          TYPE string OPTIONAL
      !iv_type         TYPE c DEFAULT 'text'
      !iv_subtype      TYPE c DEFAULT 'html'
      !iv_size         TYPE i DEFAULT 0
    EXPORTING
      !ev_assigned_url TYPE string
    CHANGING
      !ct_data_table   TYPE STANDARD TABLE
    RAISING
      /apmg/cx_apm_error.

  METHODS set_registered_events
    IMPORTING
      !it_events TYPE cntl_simple_events
    RAISING
      /apmg/cx_apm_error.

  METHODS show_url
    IMPORTING
      !iv_url TYPE string
    RAISING
      /apmg/cx_apm_error.

  METHODS free.

  METHODS close_document.

  METHODS get_url
    RETURNING
      VALUE(rv_url) TYPE string.

  METHODS back.

  METHODS set_visiblity
    IMPORTING
      !iv_visible TYPE abap_bool.

  METHODS set_focus
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
