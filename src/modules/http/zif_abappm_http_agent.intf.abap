INTERFACE ZIF_ABAPPM_HTTP_AGENT PUBLIC.

************************************************************************
* HTTP Agent
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  CONSTANTS:
    BEGIN OF c_methods,
      get    TYPE string VALUE 'GET',
      post   TYPE string VALUE 'POST',
      put    TYPE string VALUE 'PUT',
      delete TYPE string VALUE 'DELETE',
      patch  TYPE string VALUE 'PATCH',
    END OF c_methods.

  METHODS global_headers
    RETURNING
      VALUE(result) TYPE REF TO ZCL_ABAPPM_STRING_MAP.

  METHODS request
    IMPORTING
      !url          TYPE string
      !ssl_id       TYPE ssfapplssl DEFAULT 'ANONYM'
      !method       TYPE string DEFAULT c_methods-get
      !query        TYPE REF TO ZCL_ABAPPM_STRING_MAP OPTIONAL
      !headers      TYPE REF TO ZCL_ABAPPM_STRING_MAP OPTIONAL
      !payload      TYPE any OPTIONAL " can be string, xstring
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_HTTP_RESPONSE
    RAISING
      ZCX_ABAPPM_ERROR.

ENDINTERFACE.
