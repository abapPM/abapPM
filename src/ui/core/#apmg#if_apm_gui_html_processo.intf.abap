INTERFACE /apmg/if_apm_gui_html_processo PUBLIC.

  METHODS process
    IMPORTING
      !iv_html         TYPE string
      !ii_gui_services TYPE REF TO /apmg/if_apm_gui_services
    RETURNING
      VALUE(rv_html)   TYPE string
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
