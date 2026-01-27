CLASS /apmg/cl_apm_progress_bar DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Progress Bar
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    INTERFACES /apmg/if_apm_progress_bar.

    CLASS-METHODS set_instance
      IMPORTING
        !instance TYPE REF TO /apmg/if_apm_progress_bar.

    CLASS-METHODS get_instance
      IMPORTING
        !total        TYPE i
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_progress_bar.

  PROTECTED SECTION.

    CLASS-DATA global_instance TYPE REF TO /apmg/if_apm_progress_bar.

    DATA total TYPE i.

    METHODS calculate_percentage
      IMPORTING
        !current      TYPE i
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.

    DATA time_next TYPE t.
    DATA date_next TYPE d.

ENDCLASS.



CLASS /apmg/cl_apm_progress_bar IMPLEMENTATION.


  METHOD /apmg/if_apm_progress_bar~off.

    " Clear the status bar
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

  ENDMETHOD.


  METHOD /apmg/if_apm_progress_bar~set_total.

    me->total = total.

    CLEAR: time_next, date_next.

  ENDMETHOD.


  METHOD /apmg/if_apm_progress_bar~show.

    CONSTANTS c_wait_secs TYPE i VALUE 2.

    GET TIME.

    DATA(current_time) = sy-uzeit.
    IF time_next IS INITIAL AND date_next IS INITIAL.
      time_next = current_time.
      date_next = sy-datum.
    ENDIF.

    " Only do a progress indication if enough time has passed
    IF current_time >= time_next AND sy-datum = date_next OR sy-datum > date_next.

      DATA(percentage) = calculate_percentage( current ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = percentage
          text       = text.

      time_next = current_time + c_wait_secs.

    ENDIF.

    IF sy-datum > date_next.
      date_next = sy-datum.
    ENDIF.
    IF time_next < current_time.
      date_next = sy-datum + 1.
    ENDIF.

  ENDMETHOD.


  METHOD calculate_percentage.

    TRY.
        result = current / total * 100.

        CASE result.
          WHEN 0.
            result = 1.
          WHEN 100.
            result = 99.
        ENDCASE.
      CATCH cx_sy_zerodivide.
        result = 0.
    ENDTRY.

  ENDMETHOD.


  METHOD get_instance.

    " Max one progress indicator at a time is supported
    IF global_instance IS INITIAL.
      global_instance = NEW /apmg/cl_apm_progress_bar( ).
    ENDIF.

    global_instance->set_total( total ).

    result = global_instance.

  ENDMETHOD.


  METHOD set_instance.

    global_instance = instance.

  ENDMETHOD.
ENDCLASS.
