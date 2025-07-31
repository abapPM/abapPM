CLASS /apmg/cl_apm_gui_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS is_renderable
      IMPORTING
        !io_obj       TYPE REF TO object
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.
    CLASS-METHODS is_event_handler
      IMPORTING
        !io_obj       TYPE REF TO object
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_gui_utils IMPLEMENTATION.


  METHOD is_event_handler.

    DATA li_event_handler TYPE REF TO /apmg/if_apm_gui_event_handler.

    TRY.
        li_event_handler ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error.
        rv_yes = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_renderable.

    DATA li_renderable TYPE REF TO /apmg/if_apm_gui_renderable.

    TRY.
        li_renderable ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error.
        rv_yes = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
