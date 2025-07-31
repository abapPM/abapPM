INTERFACE /apmg/if_apm_persist_apm PUBLIC.

************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  " Maximum key length to allow transporting entries
  CONSTANTS c_max_key_len TYPE i VALUE 120.

  TYPES:
    ty_key   TYPE c LENGTH c_max_key_len,
    ty_value TYPE string,
    BEGIN OF ty_zabappm,
      keys      TYPE ty_key,
      value     TYPE ty_value,
      luser     TYPE as4user,
      timestamp TYPE timestampl,
    END OF ty_zabappm,
    BEGIN OF ty_list_item,
      keys      TYPE ty_key,
      key_type  TYPE string,
      key_name  TYPE string,
      key_extra TYPE string,
      value     TYPE ty_value,
      user      TYPE as4user,
      timestamp TYPE timestampl,
    END OF ty_list_item,
    ty_list TYPE SORTED TABLE OF ty_list_item WITH UNIQUE KEY keys.

  TYPES:
    BEGIN OF ty_explained,
      key_type     TYPE string,
      description  TYPE string,
      extra        TYPE string,
      content_type TYPE string,
    END OF ty_explained.

  CONSTANTS:
    c_zapm        TYPE tadir-object VALUE 'ZAPM',
    c_devclass    TYPE c LENGTH 30 VALUE '$TMP',
    c_transaction TYPE c LENGTH 30 VALUE 'ZAPM',
    c_tabname     TYPE c LENGTH 30 VALUE 'ZABAPPM',
    c_lock        TYPE c LENGTH 30 VALUE 'EZABAPPM',
    c_english     TYPE c LENGTH 1 VALUE 'E'.

  CONSTANTS:
    BEGIN OF c_key_type,
      package   TYPE ty_key VALUE 'PACKAGE',
      settings  TYPE ty_key VALUE 'SETTINGS',
      packument TYPE ty_key VALUE 'PACKUMENT',
    END OF c_key_type,
    BEGIN OF c_key_name,
      global_settings TYPE ty_key VALUE '$GLOBAL$',
      user_settings   TYPE ty_key VALUE '$USER$',
    END OF c_key_name,
    BEGIN OF c_key_extra,
      package_json    TYPE ty_key VALUE 'PACKAGE_JSON',
      package_readme  TYPE ty_key VALUE 'README',
      package_bundles TYPE ty_key VALUE 'BUNDLES',
    END OF c_key_extra.

  CONSTANTS:
    BEGIN OF c_content_type,
      json     TYPE string VALUE 'json',
      markdown TYPE string VALUE 'markdown',
      text     TYPE string VALUE 'text',
    END OF c_content_type.

  METHODS list
    IMPORTING
      !filter       TYPE ty_key OPTIONAL
      !from         TYPE timestampl DEFAULT 0
      !to           TYPE timestampl DEFAULT 99991231000000
        PREFERRED PARAMETER filter
    RETURNING
      VALUE(result) TYPE ty_list.

  METHODS load
    IMPORTING
      !key          TYPE ty_key
    RETURNING
      VALUE(result) TYPE ty_zabappm
    RAISING
      /apmg/cx_apm_error.

  METHODS save
    IMPORTING
      !key   TYPE ty_key
      !value TYPE ty_value
    RAISING
      /apmg/cx_apm_error.

  METHODS delete
    IMPORTING
      !key TYPE ty_key
    RAISING
      /apmg/cx_apm_error.

  METHODS lock
    IMPORTING
      !key  TYPE ty_key
      !mode TYPE enqmode DEFAULT 'E'
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
