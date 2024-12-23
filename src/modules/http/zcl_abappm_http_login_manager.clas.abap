CLASS ZCL_ABAPPM_HTTP_LOGIN_MANAGER DEFINITION
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
        !host         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS set
      IMPORTING
        !host         TYPE string
        !username     TYPE string
        !password     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS save
      IMPORTING
        !host TYPE string
        !auth TYPE string.

    CLASS-METHODS clear.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_auth,
        host TYPE string,
        auth TYPE string,
      END OF ty_auth.

    CLASS-DATA auths TYPE TABLE OF ty_auth WITH DEFAULT KEY.

    CLASS-METHODS append
      IMPORTING
        !host TYPE string
        !auth TYPE string.

ENDCLASS.



CLASS ZCL_ABAPPM_HTTP_LOGIN_MANAGER IMPLEMENTATION.


  METHOD append.

    READ TABLE auths WITH KEY host = host TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO auths ASSIGNING FIELD-SYMBOL(<auth>).
      <auth>-host = host.
      <auth>-auth = auth.
    ENDIF.

  ENDMETHOD.


  METHOD clear.

    CLEAR auths.

  ENDMETHOD.


  METHOD get.

    READ TABLE auths INTO DATA(auth) WITH KEY host = host.
    IF sy-subrc = 0.
      result = auth-auth.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    IF NOT auth IS INITIAL.
      append( host = host
              auth = auth ).
    ENDIF.

  ENDMETHOD.


  METHOD set.

    ASSERT NOT host IS INITIAL.

    IF username IS INITIAL OR password IS INITIAL.
      RETURN.
    ENDIF.

    result = |Basic { cl_http_utility=>encode_base64( |{ username }:{ password }| ) }|.

    append( host = host
            auth = result ).

  ENDMETHOD.
ENDCLASS.
