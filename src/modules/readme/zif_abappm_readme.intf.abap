INTERFACE ZIF_ABAPPM_README PUBLIC.


************************************************************************
* Readme
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    BEGIN OF ty_readme,
      key      TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY,
      markdown TYPE string,
      instance TYPE REF TO ZIF_ABAPPM_README,
    END OF ty_readme.
  TYPES:
    ty_readmes TYPE STANDARD TABLE OF ty_readme WITH KEY key.

  METHODS get
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !iv_markdown  TYPE string
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_README
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_README
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS save
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS delete
    RAISING
      ZCX_ABAPPM_ERROR.

ENDINTERFACE.
