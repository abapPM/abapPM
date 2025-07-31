CLASS /apmg/cl_apm_roadmap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Roadmap Placeholders
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS not_implemented
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS planned
      IMPORTING
        !message TYPE string OPTIONAL
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_roadmap IMPLEMENTATION.


  METHOD not_implemented.
    RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'The feature has not been implemented yet'.
  ENDMETHOD.


  METHOD planned.
    DATA(text) = message.
    IF text IS INITIAL.
      text = 'The feature has not been implemented but is on the roadmap'.
    ENDIF.
    RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = text.
  ENDMETHOD.
ENDCLASS.
