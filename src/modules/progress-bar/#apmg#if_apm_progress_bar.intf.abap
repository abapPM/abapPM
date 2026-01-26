INTERFACE /apmg/if_apm_progress_bar PUBLIC.

  METHODS show
    IMPORTING
      !current TYPE i
      !text    TYPE csequence
    RAISING
      /apmg/cx_apm_error.

  METHODS set_total
    IMPORTING
      !total TYPE i.

  METHODS off
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
