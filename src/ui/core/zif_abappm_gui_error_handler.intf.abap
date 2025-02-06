INTERFACE zif_abappm_gui_error_handler PUBLIC.

  METHODS handle_error
    IMPORTING
      !ix_error         TYPE REF TO zcx_abappm_error
    RETURNING
      VALUE(rv_handled) TYPE abap_bool.

ENDINTERFACE.
