INTERFACE /apmg/if_apm_gui_event PUBLIC.

  DATA mv_action TYPE string READ-ONLY.
  DATA mv_getdata TYPE string READ-ONLY.
  DATA mt_postdata TYPE /apmg/if_apm_html_viewer=>ty_post_data READ-ONLY.
  DATA mi_gui_services TYPE REF TO /apmg/if_apm_gui_services READ-ONLY.
  DATA mv_current_page_name TYPE string.

  METHODS query
    RETURNING
      VALUE(ro_string_map) TYPE REF TO /apmg/cl_apm_string_map
    RAISING
      /apmg/cx_apm_error.

  METHODS form_data
    RETURNING
      VALUE(ro_string_map) TYPE REF TO /apmg/cl_apm_string_map
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
