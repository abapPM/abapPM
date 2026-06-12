CLASS /apmg/cl_apm_http_login_manage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* HTTP Login Manager
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS get_auth
      IMPORTING
        !host         TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_username
      IMPORTING
        !host         TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_host
      IMPORTING
        !host         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS set_basic
      IMPORTING
        !host         TYPE csequence
        !username     TYPE csequence
        !password     TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS set_token
      IMPORTING
        !host         TYPE csequence
        !username     TYPE csequence
        !token        TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS save
      IMPORTING
        !host     TYPE csequence
        !auth     TYPE csequence
        !username TYPE csequence OPTIONAL.

    CLASS-METHODS clear
      IMPORTING
        !host TYPE csequence OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_auth,
        host TYPE string,
        auth TYPE string,
        user TYPE string,
      END OF ty_auth.

    CLASS-DATA auths TYPE TABLE OF ty_auth WITH DEFAULT KEY.

    CLASS-METHODS append
      IMPORTING
        !host TYPE string
        !auth TYPE string
        !user TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_http_login_manage IMPLEMENTATION.


  METHOD append.

    DATA(hostname) = get_host( host ).

    IF NOT line_exists( auths[ host = hostname ] ).
      APPEND INITIAL LINE TO auths ASSIGNING FIELD-SYMBOL(<auth>).
      <auth>-host = hostname.
      <auth>-auth = auth.
      <auth>-user = user.
    ENDIF.

  ENDMETHOD.


  METHOD clear.

    IF host IS INITIAL.
      CLEAR auths.
    ELSE.
      DELETE auths WHERE host = host ##SUBRC_OK.
    ENDIF.

  ENDMETHOD.


  METHOD get_auth.

    READ TABLE auths INTO DATA(auth) WITH KEY host = get_host( host ).
    IF sy-subrc = 0.
      result = auth-auth.
    ENDIF.

  ENDMETHOD.


  METHOD get_host.

    " If it's a URL, use host:port, otherwise just take the input
    TRY.
        DATA(url) = /apmg/cl_apm_url=>parse( host )->components.

        result = url-host.
        IF url-port IS NOT INITIAL.
          result = |{ result }:{ url-port }|.
        ENDIF.
      CATCH /apmg/cx_apm_error.
        result = host.
    ENDTRY.

  ENDMETHOD.


  METHOD get_username.

    READ TABLE auths INTO DATA(auth) WITH KEY host = get_host( host ).
    IF sy-subrc = 0.
      result = auth-user.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    IF auth IS NOT INITIAL.
      append( host = host
              auth = auth
              user = |{ username }| ).
    ENDIF.

  ENDMETHOD.


  METHOD set_basic.

    ASSERT host IS NOT INITIAL.

    IF username IS INITIAL OR password IS INITIAL.
      RETURN.
    ENDIF.

    result = |Basic { cl_http_utility=>encode_base64( |{ username }:{ password }| ) }|.

    append( host = host
            auth = result
            user = username ).

  ENDMETHOD.


  METHOD set_token.

    ASSERT host IS NOT INITIAL.

    IF token IS INITIAL.
      RETURN.
    ENDIF.

    result = |Bearer { token }|.

    append( host = host
            auth = result
            user = username ).

  ENDMETHOD.
ENDCLASS.
