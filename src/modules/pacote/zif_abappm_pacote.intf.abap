INTERFACE ZIF_ABAPPM_PACOTE PUBLIC.


************************************************************************
* Pacote
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    BEGIN OF ty_pacote,
      key       TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY,
      name      TYPE string,
      json      TYPE string,
      packument TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT,
      instance  TYPE REF TO ZIF_ABAPPM_PACOTE,
    END OF ty_pacote.
  TYPES:
    ty_pacotes TYPE STANDARD TABLE OF ty_pacote WITH KEY key.

  METHODS get
    RETURNING
      VALUE(result) TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT.

  METHODS get_json
    RETURNING
      VALUE(result) TYPE string.

  METHODS get_version
    IMPORTING
      !version      TYPE string
    RETURNING
      VALUE(result) TYPE ZIF_ABAPPM_TYPES=>TY_VERSION.

  METHODS set
    IMPORTING
      !packument    TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACOTE
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS set_json
    IMPORTING
      !json         TYPE string
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACOTE
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACOTE
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS save
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS delete
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS manifest
    IMPORTING
      version       TYPE string
      abbreviated   TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS packument
    RETURNING
      VALUE(result) TYPE string
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS tarball
    IMPORTING
      filename      TYPE string
    RETURNING
      VALUE(result) TYPE xstring
    RAISING
      ZCX_ABAPPM_ERROR.

ENDINTERFACE.
