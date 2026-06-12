CLASS /apmg/cl_apm_command_logout DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Logout Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry TYPE string
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_response,
        ok TYPE string,
      END OF ty_response.

    METHODS execute
      IMPORTING
        !registry TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_logout IMPLEMENTATION.


  METHOD execute.

    " Send logout request
    DATA(response) = /apmg/cl_apm_registry=>fetch(
      command  = 'logout'
      registry = registry
      url      = |{ registry }/-/user/token/$apm$|
      method   = /apmg/if_apm_http_agent=>c_method-delete ).

    DATA(message) = /apmg/cl_apm_registry=>check_response(
      response = response
      text     = 'Logout error' ).

    DATA(logout_response) = VALUE ty_response( ).

    /apmg/cl_apm_json=>to_abap(
      EXPORTING
        json   = response->cdata( )
      CHANGING
        result = logout_response ).

    " Clear token
* TODO!
*    /apmg/cl_apm_http_logout_manage=>clear(
*      host  = registry
*      username = username
*      token = logout_response-token ).

    IF message IS INITIAL.
      MESSAGE logout_response-ok TYPE 'S'.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_logout( ).

    command->execute( registry ).

  ENDMETHOD.
ENDCLASS.
