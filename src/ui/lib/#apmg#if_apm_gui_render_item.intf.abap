INTERFACE /apmg/if_apm_gui_render_item PUBLIC.

  METHODS render
    IMPORTING
      !iv_item       TYPE any
      !iv_index      TYPE i
    RETURNING
      VALUE(ri_html) TYPE REF TO /apmg/if_apm_html
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
