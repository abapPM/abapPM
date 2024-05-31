INTERFACE ZIF_ABAPPM_PACKAGE_JSON PUBLIC.

************************************************************************
* Package JSON
*
* Copyright (c) Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
* Similar to @npmcli/package-json but with its own persistence
*
* https://www.npmjs.com/package/@npmcli/package-json
************************************************************************

  INTERFACES ZIF_ABAPPM_PACKAGE_JSON_TYPES.

  CONSTANTS:
    c_version TYPE string VALUE '1.0.0'.

  METHODS get
    RETURNING
      VALUE(result) TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON.

  METHODS get_json
    IMPORTING
      !iv_complete  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !is_json      TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
    RAISING
      ZCX_ABAPPM_PACKAGE_JSON.

  METHODS set_json
    IMPORTING
      !iv_json      TYPE string
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
    RAISING
      ZCX_ABAPPM_PACKAGE_JSON.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
    RAISING
      ZCX_ABAPPM_PACKAGE_JSON.

  METHODS save
    RAISING
      ZCX_ABAPPM_PACKAGE_JSON.

  METHODS delete
    RAISING
      ZCX_ABAPPM_PACKAGE_JSON.

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
