INTERFACE zif_abappm_http_agent PUBLIC.

************************************************************************
* HTTP Agent
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.1' ##NEEDED.

  CONSTANTS:
    BEGIN OF c_method,
      get    TYPE string VALUE 'GET',
      post   TYPE string VALUE 'POST',
      put    TYPE string VALUE 'PUT',
      delete TYPE string VALUE 'DELETE',
      patch  TYPE string VALUE 'PATCH',
    END OF c_method,
    BEGIN OF c_header,
      accept        TYPE string VALUE 'accept',
      authorization TYPE string VALUE 'authorization',
      content_type  TYPE string VALUE 'content-type',
      user_agent    TYPE string VALUE 'user-agent',
      cookie        TYPE string VALUE 'cookie',
      set_cookie    TYPE string VALUE 'set-cookie',
      x_csrf_token  TYPE string VALUE 'x-csrf-token',
    END OF c_header,
    BEGIN OF c_content_type,
      json TYPE string VALUE 'application/json',
      text TYPE string VALUE 'application/text',
      xml  TYPE string VALUE 'application/xml',
      bin  TYPE string VALUE 'application/octet-stream',
    END OF c_content_type.

  METHODS global_headers
    RETURNING
      VALUE(result) TYPE REF TO zcl_abappm_string_map.

  METHODS request
    IMPORTING
      !url          TYPE string
      !ssl_id       TYPE ssfapplssl DEFAULT 'ANONYM'
      !method       TYPE string DEFAULT c_method-get
      !query        TYPE REF TO zcl_abappm_string_map OPTIONAL
      !headers      TYPE REF TO zcl_abappm_string_map OPTIONAL
      !payload      TYPE any OPTIONAL " can be char, string, xstring
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_http_response
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
