INTERFACE zif_abappm_gui_event_handler PUBLIC.

  TYPES:
    BEGIN OF ty_handling_result,
      page  TYPE REF TO zif_abappm_gui_renderable,
      state TYPE i,
    END OF ty_handling_result.

  METHODS on_event
    IMPORTING
      !ii_event         TYPE REF TO zif_abappm_gui_event
    RETURNING
      VALUE(rs_handled) TYPE ty_handling_result
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
