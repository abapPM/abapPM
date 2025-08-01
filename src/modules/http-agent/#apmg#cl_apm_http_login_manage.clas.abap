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

    CLASS-METHODS get
      IMPORTING
        !host         TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS set
      IMPORTING
        !host         TYPE csequence
        !username     TYPE csequence
        !password     TYPE csequence
        !is_basic     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS save
      IMPORTING
        !host TYPE csequence
        !auth TYPE csequence.

    CLASS-METHODS clear.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_auth,
        host TYPE string,
        auth TYPE string,
      END OF ty_auth.

    CLASS-DATA auths TYPE TABLE OF ty_auth WITH DEFAULT KEY.

    CLASS-METHODS get_host
      IMPORTING
        !host         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS append
      IMPORTING
        !host TYPE string
        !auth TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_http_login_manage IMPLEMENTATION.


  METHOD append.

    DATA(hostname) = get_host( host ).

    IF NOT line_exists( auths[ host = hostname ] ).
      APPEND INITIAL LINE TO auths ASSIGNING FIELD-SYMBOL(<auth>).
      <auth>-host = hostname.
      <auth>-auth = auth.
    ENDIF.

  ENDMETHOD.


  METHOD clear.

    CLEAR auths.

  ENDMETHOD.


  METHOD get.

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


  METHOD save.

    IF auth IS NOT INITIAL.
      append( host = host
              auth = auth ).
    ENDIF.

  ENDMETHOD.


  METHOD set.

    ASSERT host IS NOT INITIAL.

    IF username IS INITIAL OR password IS INITIAL.
      RETURN.
    ENDIF.

    IF is_basic = abap_true.
      result = |Basic { cl_http_utility=>encode_base64( |{ username }:{ password }| ) }|.
    ELSE.
      result = |Bearer { password }|.
    ENDIF.

    append( host = host
            auth = result ).

  ENDMETHOD.
ENDCLASS.
