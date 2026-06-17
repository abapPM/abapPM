CLASS /apmg/cl_apm_command_whoami DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Whoami Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry     TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_response,
        username TYPE string,
      END OF ty_response.

    METHODS execute
      IMPORTING
        !registry     TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.
ENDCLASS.



CLASS /apmg/cl_apm_command_whoami IMPLEMENTATION.


  METHOD execute.

    /apmg/cl_apm_registry=>check_logged_in( registry ).

    DATA(response) = /apmg/cl_apm_registry=>fetch(
      command  = 'whoami'
      registry = registry
      url      = |{ registry }/-/whoami| ).

    DATA(message) = /apmg/cl_apm_registry=>check_response(
      response = response
      text     = 'Whoami error' ).

    DATA(whoami_response) = VALUE ty_response( ).

    /apmg/cl_apm_json=>to_abap(
      EXPORTING
        json   = response->cdata( )
      CHANGING
        result = whoami_response ).

    IF message IS INITIAL.
      result = whoami_response-username.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_whoami( ).

    result = command->execute( registry ).

  ENDMETHOD.
ENDCLASS.
