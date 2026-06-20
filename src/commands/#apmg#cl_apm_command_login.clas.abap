CLASS /apmg/cl_apm_command_login DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Login Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* FUTURE: Enable web login (with optional 2fa)
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry  TYPE string
        !username  TYPE string
        !password  TYPE string
        !auth_type TYPE string DEFAULT 'legacy'
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_request,
        name     TYPE string,
        password TYPE string,
      END OF ty_request,
      BEGIN OF ty_response,
        ok    TYPE string,
        token TYPE string,
      END OF ty_response.

    METHODS execute
      IMPORTING
        !registry  TYPE string
        !username  TYPE string
        !password  TYPE string
        !auth_type TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_login IMPLEMENTATION.


  METHOD execute.

    /apmg/cl_apm_registry=>check_logged_out( registry ).

    DATA(login_request) = VALUE ty_request(
      name     = username
      password = password ).

    DATA(payload) = /apmg/cl_apm_json=>to_string( login_request ).

    DATA(response) = /apmg/cl_apm_registry=>fetch(
      command   = 'login'
      registry  = registry
      url       = |{ registry }/-/user/org.couchdb.user:{ username }|
      method    = /apmg/if_apm_http_agent=>c_method-put
      payload   = payload
      auth_type = auth_type
      username  = username
      password  = password ).

    DATA(message) = /apmg/cl_apm_registry=>check_response(
      response = response
      text     = 'Login error' ).

    DATA(login_response) = VALUE ty_response( ).

    /apmg/cl_apm_json=>to_abap(
      EXPORTING
        json   = response->cdata( )
      CHANGING
        result = login_response ).

    " Set token for subsequent requests (overwrites basic authentication)
    /apmg/cl_apm_http_login_manage=>set_token(
      host     = registry
      username = username
      token    = login_response-token ).

    IF message IS INITIAL.
      MESSAGE login_response-ok TYPE 'S'.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    IF auth_type <> 'legacy'.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Unsupported auth type'.
    ENDIF.

    DATA(command) = NEW /apmg/cl_apm_command_login( ).

    command->execute(
      registry  = /apmg/cl_apm_utils=>remove_trailing_slash( registry )
      username  = username
      password  = password
      auth_type = auth_type ).

  ENDMETHOD.
ENDCLASS.
