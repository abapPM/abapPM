INTERFACE ZIF_ABAPPM_HTTP_RESPONSE PUBLIC.

************************************************************************
* HTTP Response
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************

  METHODS data
    RETURNING
      VALUE(result) TYPE xstring.

  METHODS cdata
    RETURNING
      VALUE(result) TYPE string.

  METHODS json
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_AJSON
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS is_ok
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS code
    RETURNING
      VALUE(result) TYPE i.

  METHODS error
    RETURNING
      VALUE(result) TYPE string.

  METHODS headers
    RETURNING
      VALUE(result) TYPE REF TO ZCL_ABAPPM_STRING_MAP
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS close.

ENDINTERFACE.
