INTERFACE /apmg/if_apm_gui_event_handler PUBLIC.

  TYPES:
    BEGIN OF ty_handling_result,
      page  TYPE REF TO /apmg/if_apm_gui_renderable,
      state TYPE i,
    END OF ty_handling_result.

  METHODS on_event
    IMPORTING
      !ii_event         TYPE REF TO /apmg/if_apm_gui_event
    RETURNING
      VALUE(rs_handled) TYPE ty_handling_result
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
