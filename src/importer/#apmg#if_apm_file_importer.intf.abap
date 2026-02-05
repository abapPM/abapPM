INTERFACE /apmg/if_apm_file_importer PUBLIC.

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
      /apmg/cx_apm_error.

  METHODS get_xml
    RETURNING
      VALUE(result) TYPE string
    RAISING
      /apmg/cx_apm_error.

  METHODS get_xml_parsed
    RETURNING
      VALUE(result) TYPE REF TO zif_abapgit_xml_input
    RAISING
      /apmg/cx_apm_error.

  METHODS get_json
    RETURNING
      VALUE(result) TYPE string
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
