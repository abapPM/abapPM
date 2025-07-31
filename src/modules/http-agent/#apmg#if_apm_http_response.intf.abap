INTERFACE /apmg/if_apm_http_response PUBLIC.

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
      VALUE(result) TYPE REF TO /apmg/if_apm_ajson
    RAISING
      /apmg/cx_apm_error.

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
      VALUE(result) TYPE REF TO /apmg/cl_apm_string_map
    RAISING
      /apmg/cx_apm_error.

  METHODS close.

ENDINTERFACE.
