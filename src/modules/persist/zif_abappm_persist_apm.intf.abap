INTERFACE zif_abappm_persist_apm PUBLIC.

************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  TYPES:
    ty_key TYPE c LENGTH 120,
    BEGIN OF ty_zabappm,
      keys      TYPE ty_key,
      value     TYPE string,
      luser     TYPE as4user,
      timestamp TYPE timestampl,
    END OF ty_zabappm,
    ty_list TYPE SORTED TABLE OF ty_zabappm WITH UNIQUE KEY keys.

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  CONSTANTS:
    c_zapm        TYPE tadir-object VALUE 'ZAPM',
    c_devclass    TYPE c LENGTH 30 VALUE '$TMP',
    c_transaction TYPE c LENGTH 30 VALUE 'ZAPM',
    c_tabname     TYPE c LENGTH 30 VALUE 'ZABAPPM',
    c_lock        TYPE c LENGTH 30 VALUE 'EZABAPPM',
    c_english     TYPE c LENGTH 1 VALUE 'E'.

  CONSTANTS:
    BEGIN OF c_key_type,
      package  TYPE ty_key VALUE 'PACKAGE',
      settings TYPE ty_key VALUE 'SETTINGS',
    END OF c_key_type.

  METHODS list
    IMPORTING
      !iv_filter    TYPE ty_key OPTIONAL
      !iv_from      TYPE timestampl DEFAULT 0
      !iv_to        TYPE timestampl DEFAULT 99991231000000
        PREFERRED PARAMETER iv_filter
    RETURNING
      VALUE(result) TYPE ty_list.

  METHODS load
    IMPORTING
      !iv_key       TYPE ty_key
    RETURNING
      VALUE(result) TYPE ty_zabappm
    RAISING
      zcx_abappm_persist_apm.

  METHODS save
    IMPORTING
      !iv_key   TYPE ty_key
      !iv_value TYPE ty_zabappm-value
    RAISING
      zcx_abappm_persist_apm.

  METHODS delete
    IMPORTING
      !iv_key TYPE ty_key
    RAISING
      zcx_abappm_persist_apm.

  METHODS lock
    IMPORTING
      !iv_key  TYPE ty_key
      !iv_mode TYPE enqmode DEFAULT 'E'
    RAISING
      zcx_abappm_persist_apm.

ENDINTERFACE.
