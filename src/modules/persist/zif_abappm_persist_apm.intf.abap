INTERFACE zif_abappm_persist_apm PUBLIC.

************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  TYPES:
    ty_key TYPE c LENGTH 255,
    BEGIN OF ty_zabappm,
      key   TYPE ty_key,
      value TYPE string,
      luser TYPE c LENGTH 12,
      ldate TYPE d,
      ltime TYPE t,
    END OF ty_zabappm,
    ty_list TYPE SORTED TABLE OF ty_zabappm WITH UNIQUE KEY key.

  CONSTANTS:
    c_zapm        TYPE tadir-object VALUE 'ZAPM',
    c_devclass    TYPE c LENGTH 30 VALUE '$TMP',
    c_transaction TYPE c LENGTH 30 VALUE 'ZAPM',
    c_tabname     TYPE c LENGTH 30 VALUE 'ZABAPPM',
    c_lock        TYPE c LENGTH 30 VALUE 'EZABAPPM',
    c_english     TYPE c LENGTH 1 VALUE 'E'.

  CONSTANTS:
    BEGIN OF c_type,
      package  TYPE ty_zabappm-key VALUE 'PACKAGE',
      json     TYPE ty_zabappm-key VALUE 'JSON',
      readme   TYPE ty_zabappm-key VALUE 'README',
      favicon  TYPE ty_zabappm-key VALUE 'FAVICON', " FUTURE
      user     TYPE ty_zabappm-key VALUE 'USER',
      settings TYPE ty_zabappm-key VALUE 'SETTINGS',
    END OF c_type.

  METHODS list
    IMPORTING
      !iv_key       TYPE zif_abappm_persist_apm=>ty_zabappm-key OPTIONAL
    RETURNING
      VALUE(result) TYPE zif_abappm_persist_apm=>ty_list
    RAISING
      zcx_abappm_persist_apm.

  METHODS load
    IMPORTING
      !iv_key       TYPE zif_abappm_persist_apm=>ty_zabappm-key
    RETURNING
      VALUE(result) TYPE zif_abappm_persist_apm=>ty_zabappm
    RAISING
      zcx_abappm_persist_apm.

  METHODS save
    IMPORTING
      !iv_key   TYPE zif_abappm_persist_apm=>ty_zabappm-key
      !iv_value TYPE zif_abappm_persist_apm=>ty_zabappm-value
    RAISING
      zcx_abappm_persist_apm.

  METHODS delete
    IMPORTING
      !iv_key TYPE zif_abappm_persist_apm=>ty_zabappm-key
    RAISING
      zcx_abappm_persist_apm.

ENDINTERFACE.
