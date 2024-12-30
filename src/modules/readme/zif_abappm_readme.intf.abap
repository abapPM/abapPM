INTERFACE zif_abappm_readme PUBLIC.


************************************************************************
* Readme
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    BEGIN OF ty_readme,
      key      TYPE zif_abappm_persist_apm=>ty_key,
      markdown TYPE string,
      instance TYPE REF TO zif_abappm_readme,
    END OF ty_readme,
    ty_readmes TYPE STANDARD TABLE OF ty_readme WITH KEY key ##NEEDED.

  METHODS get
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !markdown     TYPE string
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_readme
    RAISING
      zcx_abappm_error.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_readme
    RAISING
      zcx_abappm_error.

  METHODS save
    RAISING
      zcx_abappm_error.

  METHODS delete
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
