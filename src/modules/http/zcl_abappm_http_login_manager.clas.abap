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
        !iv_host       TYPE string
      RETURNING
        VALUE(rv_auth) TYPE string.

    CLASS-METHODS set
      IMPORTING
        !iv_host       TYPE string
        !iv_username   TYPE string
        !iv_password   TYPE string
      RETURNING
        VALUE(rv_auth) TYPE string.

    CLASS-METHODS save
      IMPORTING
        !iv_host TYPE string
        !iv_auth TYPE string.

    CLASS-METHODS clear.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_auth,
        host TYPE string,
        auth TYPE string,
      END OF ty_auth.

    CLASS-DATA gt_auth TYPE TABLE OF ty_auth WITH DEFAULT KEY.

    CLASS-METHODS append
      IMPORTING
        !iv_host TYPE string
        !iv_auth TYPE string.

ENDCLASS.



CLASS zcl_abappm_http_login_manager IMPLEMENTATION.


  METHOD append.

    FIELD-SYMBOLS <ls_auth> LIKE LINE OF gt_auth.

    READ TABLE gt_auth WITH KEY host = iv_host TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO gt_auth ASSIGNING <ls_auth>.
      <ls_auth>-host = iv_host.
      <ls_auth>-auth = iv_auth.
    ENDIF.

  ENDMETHOD.


  METHOD clear.
    CLEAR gt_auth.
  ENDMETHOD.


  METHOD get.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH KEY host = iv_host.
    IF sy-subrc = 0.
      rv_auth = ls_auth-auth.
    ENDIF.

  ENDMETHOD.


  METHOD save.
    IF NOT iv_auth IS INITIAL.
      append( iv_host = iv_host
              iv_auth = iv_auth ).
    ENDIF.
  ENDMETHOD.


  METHOD set.

    ASSERT NOT iv_host IS INITIAL.

    IF iv_username IS INITIAL OR iv_password IS INITIAL.
      RETURN.
    ENDIF.

    rv_auth = |Basic { cl_http_utility=>encode_base64( |{ iv_username }:{ iv_password }| ) }|.

    append( iv_host = iv_host
            iv_auth = rv_auth ).

  ENDMETHOD.
ENDCLASS.
