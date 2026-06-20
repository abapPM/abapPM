CLASS /apmg/cl_apm_command_ping DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Whoami Command
*
* Copyright 2026 apm.to Inc. <https://apm.to>
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

    METHODS execute
      IMPORTING
        !registry     TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_ping IMPLEMENTATION.


  METHOD execute.

    DATA(response) = /apmg/cl_apm_registry=>fetch(
      command  = 'ping'
      registry = registry
      url      = |{ registry }/-/ping| ).

    DATA(message) = /apmg/cl_apm_registry=>check_response(
      response = response
      text     = 'Ping error' ).

    DATA(status) = response->cdata( ).

    IF message IS INITIAL AND status CS '{}'.
      result = 'PONG'.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_ping( ).

    result = command->execute( /apmg/cl_apm_utils=>remove_trailing_slash( registry ) ).

  ENDMETHOD.
ENDCLASS.
