INTERFACE zif_abappm_http_agent PUBLIC.

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
      VALUE(result) TYPE REF TO zcl_abappm_string_map.

  METHODS request
    IMPORTING
      !url          TYPE string
      !ssl_id       TYPE ssfapplssl DEFAULT 'ANONYM'
      !method       TYPE string DEFAULT c_methods-get
      !query        TYPE REF TO zcl_abappm_string_map OPTIONAL
      !headers      TYPE REF TO zcl_abappm_string_map OPTIONAL
      !payload      TYPE any OPTIONAL " can be string, xstring
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_http_response
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
