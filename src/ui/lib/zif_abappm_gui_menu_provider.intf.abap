INTERFACE zif_abappm_gui_menu_provider PUBLIC.

  METHODS get_menu
    RETURNING
      VALUE(ro_toolbar) TYPE REF TO zcl_abappm_html_toolbar
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
