CLASS zcl_abappm_http_login_manager DEFINITION
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

    CLASS-METHODS append
      IMPORTING
        !host TYPE string
        !auth TYPE string.

ENDCLASS.



CLASS zcl_abappm_http_login_manager IMPLEMENTATION.


  METHOD append.

    IF NOT line_exists( auths[ host = host ] ).
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

    result = cl_http_utility=>encode_base64( |{ username }:{ password }| ).

    IF is_basic = abap_true.
      result = |Basic { result }|.
    ELSE.
      result = |Bearer { result }|.
    ENDIF.

    append( host = host
            auth = result ).

  ENDMETHOD.
ENDCLASS.
