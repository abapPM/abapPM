INTERFACE /apmg/if_apm_gui_error_handler PUBLIC.

  METHODS handle_error
    IMPORTING
      !ix_error         TYPE REF TO /apmg/cx_apm_error
    RETURNING
      VALUE(rv_handled) TYPE abap_bool.

ENDINTERFACE.
