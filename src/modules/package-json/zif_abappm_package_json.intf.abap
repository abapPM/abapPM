INTERFACE zif_abappm_package_json PUBLIC.

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

  INTERFACES zif_abappm_package_json_types.

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  METHODS get
    RETURNING
      VALUE(result) TYPE zif_abappm_package_json_types=>ty_package_json.

  METHODS get_json
    IMPORTING
      !iv_complete  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !is_json      TYPE zif_abappm_package_json_types=>ty_package_json
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_package_json
    RAISING
      zcx_abappm_package_json.

  METHODS set_json
    IMPORTING
      !iv_json      TYPE string
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_package_json
    RAISING
      zcx_abappm_package_json.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_package_json
    RAISING
      zcx_abappm_package_json.

  METHODS save
    RAISING
      zcx_abappm_package_json.

  METHODS delete
    RAISING
      zcx_abappm_package_json.

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
