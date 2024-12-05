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
      VALUE(ro_global_headers) TYPE REF TO zcl_abap_string_map.

  METHODS request
    IMPORTING
      !iv_url            TYPE string
      !iv_ssl_id         TYPE ssfapplssl DEFAULT 'ANONYM'
      !iv_method         TYPE string DEFAULT c_methods-get
      !io_query          TYPE REF TO zcl_abap_string_map OPTIONAL
      !io_headers        TYPE REF TO zcl_abap_string_map OPTIONAL
      !iv_payload        TYPE any OPTIONAL " can be string, xstring
    RETURNING
      VALUE(ri_response) TYPE REF TO ZIF_ABAPPM_HTTP_RESPONSE
    RAISING
      ZCX_ABAPPM_ERROR.

ENDINTERFACE.
