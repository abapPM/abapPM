INTERFACE zif_abappm_gui_html_processor PUBLIC.

  METHODS process
    IMPORTING
      !iv_html         TYPE string
      !ii_gui_services TYPE REF TO zif_abappm_gui_services
    RETURNING
      VALUE(rv_html)   TYPE string
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
