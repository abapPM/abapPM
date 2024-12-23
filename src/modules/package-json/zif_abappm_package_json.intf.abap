INTERFACE ZIF_ABAPPM_PACKAGE_JSON PUBLIC.

************************************************************************
* Package JSON
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Similar to @npmcli/package-json but with its own persistence
*
* https://www.npmjs.com/package/@npmcli/package-json
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  INTERFACES ZIF_ABAPPM_TYPES.

  TYPES:
    BEGIN OF ty_package,
      key             TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY,
      package         TYPE devclass,
      name            TYPE string,
      version         TYPE string,
      description     TYPE string,
      type            TYPE string,
      private         TYPE abap_bool,
      changed_by      TYPE as4user,
      changed_at      TYPE string,
      changed_at_raw  TYPE timestampl,
      favorite        TYPE abap_bool, " settings
      write_protected TYPE abap_bool, " settings
      labels          TYPE string_table, " settings
      instance        TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON,
    END OF ty_package,
    ty_packages TYPE STANDARD TABLE OF ty_package
      WITH NON-UNIQUE KEY primary_key COMPONENTS key
      WITH UNIQUE HASHED KEY package COMPONENTS package
      WITH NON-UNIQUE SORTED KEY name COMPONENTS name.

  METHODS get
    RETURNING
      VALUE(result) TYPE ZIF_ABAPPM_TYPES=>TY_PACKAGE_JSON.

  METHODS get_json
    IMPORTING
      !is_complete  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS set
    IMPORTING
      !package_json TYPE ZIF_ABAPPM_TYPES=>TY_PACKAGE_JSON
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS set_json
    IMPORTING
      !json         TYPE string
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS save
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS delete
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS is_valid
    RETURNING
      VALUE(result) TYPE abap_bool.

  " TODO: normalize
  " Intended for normalizing package.json in a modules tree.
  " https://www.npmjs.com/package/normalize-package-data

  " TODO: prepare
  " Like normalize but intended for preparing package.json for publish.

  " TODO: fix
  " Like normalize but intended for the apm pkg fix command.

  " TODO: update
  " Updates the contents of a package.json with the content provided.

ENDINTERFACE.
