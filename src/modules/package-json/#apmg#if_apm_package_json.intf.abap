INTERFACE /apmg/if_apm_package_json PUBLIC.

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

  INTERFACES /apmg/if_apm_types.

  TYPES:
    BEGIN OF ty_package,
      key                   TYPE /apmg/if_apm_persist_apm=>ty_key,
      package               TYPE /apmg/if_apm_types=>ty_devclass,
      name                  TYPE /apmg/if_apm_types=>ty_package_json-name,
      version               TYPE /apmg/if_apm_types=>ty_package_json-version,
      description           TYPE /apmg/if_apm_types=>ty_package_json-description,
      type                  TYPE /apmg/if_apm_types=>ty_package_json-type,
      private               TYPE /apmg/if_apm_types=>ty_package_json-private,
      changed_by            TYPE as4user,
      changed_at            TYPE string,
      changed_at_raw        TYPE timestampl,
      bundle                TYPE abap_bool,
      parent                TYPE /apmg/if_apm_types=>ty_devclass,
      abap_language_version TYPE string,
      favorite              TYPE abap_bool,    " settings
      write_protected       TYPE abap_bool,    " settings
      labels                TYPE string_table, " settings
      instance              TYPE REF TO /apmg/if_apm_package_json,
    END OF ty_package,
    ty_packages TYPE STANDARD TABLE OF ty_package
      WITH NON-UNIQUE KEY primary_key COMPONENTS key
      WITH UNIQUE HASHED KEY package COMPONENTS package
      WITH NON-UNIQUE SORTED KEY name COMPONENTS name.

  METHODS get
    RETURNING
      VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json.

  METHODS get_json
    IMPORTING
      !is_complete  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string
    RAISING
      /apmg/cx_apm_error.

  METHODS set
    IMPORTING
      !package_json TYPE /apmg/if_apm_types=>ty_package_json
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_package_json
    RAISING
      /apmg/cx_apm_error.

  METHODS set_json
    IMPORTING
      !json         TYPE string
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_package_json
    RAISING
      /apmg/cx_apm_error.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_package_json
    RAISING
      /apmg/cx_apm_error.

  METHODS save
    RAISING
      /apmg/cx_apm_error.

  METHODS delete
    RAISING
      /apmg/cx_apm_error.

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
