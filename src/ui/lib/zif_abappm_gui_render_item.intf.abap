INTERFACE zif_abappm_gui_render_item PUBLIC.

  METHODS render
    IMPORTING
      !iv_item       TYPE any
      !iv_index      TYPE i
    RETURNING
      VALUE(ri_html) TYPE REF TO zif_abappm_html
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
