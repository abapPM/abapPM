INTERFACE zif_abappm_file_importer PUBLIC.

************************************************************************
* apm Files for Importer
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  METHODS get_abap
    IMPORTING
      !extra        TYPE string OPTIONAL
    RETURNING
      VALUE(result) TYPE string_table
    RAISING
      zcx_abappm_error.

  METHODS get_xml
    RETURNING
      VALUE(result) TYPE string
    RAISING
      zcx_abappm_error.

  METHODS get_json
    RETURNING
      VALUE(result) TYPE string
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
