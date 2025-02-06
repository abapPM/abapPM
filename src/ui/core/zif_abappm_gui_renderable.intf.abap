INTERFACE zif_abappm_gui_renderable PUBLIC.

  METHODS render
    RETURNING
      VALUE(ri_html) TYPE REF TO zif_abappm_html
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
