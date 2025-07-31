INTERFACE /apmg/if_apm_gui_renderable PUBLIC.

  METHODS render
    RETURNING
      VALUE(ri_html) TYPE REF TO /apmg/if_apm_html
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
