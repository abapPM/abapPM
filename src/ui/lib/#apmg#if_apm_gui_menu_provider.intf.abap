INTERFACE /apmg/if_apm_gui_menu_provider PUBLIC.

  METHODS get_menu
    RETURNING
      VALUE(ro_toolbar) TYPE REF TO /apmg/cl_apm_html_toolbar
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
