INTERFACE ZIF_ABAPPM_PACOTE PUBLIC.


************************************************************************
* Pacote
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    BEGIN OF ty_version,
      key     TYPE string,
      version TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST,
    END OF ty_version.

  TYPES:
    BEGIN OF ty_attachment,
      key TYPE string,
      BEGIN OF tarball,
        content_type TYPE string,
        data         TYPE string,
        length       TYPE i,
      END OF tarball,
    END OF ty_attachment.

  TYPES:
    " Full packument (as fetched from registry)
    " Some fields are hoisted from latest version to root
    BEGIN OF ty_packument,
      name          TYPE string,
      description   TYPE string,
      dist_tags     TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_GENERIC WITH KEY key,
      time          TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_TIME WITH KEY key,
      versions      TYPE STANDARD TABLE OF ty_version WITH KEY key,
      maintainers   TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON WITH KEY name,
      readme        TYPE string,
      users         TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_USER WITH KEY name,
      homepage      TYPE string,
      BEGIN OF bugs,
        url   TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
        email TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_EMAIL,
      END OF bugs,
      license       TYPE string,
      keywords      TYPE string_table,
      author        TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON,
      BEGIN OF repository,
        type      TYPE string,
        url       TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
        directory TYPE string,
      END OF repository,
      __id          TYPE string,
      __rev         TYPE string,
      __attachments TYPE STANDARD TABLE OF ty_attachment WITH KEY key,
      access        TYPE string,
    END OF ty_packument.

  TYPES:
    BEGIN OF ty_pacote,
      key       TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY,
      name      TYPE string,
      json      TYPE string,
      packument TYPE ty_packument,
      instance  TYPE REF TO ZIF_ABAPPM_PACOTE,
    END OF ty_pacote.
  TYPES:
    ty_pacotes TYPE STANDARD TABLE OF ty_pacote WITH KEY key.

  METHODS get
    RETURNING
      VALUE(result) TYPE ty_packument.

  METHODS get_json
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !is_packument TYPE ty_packument
    RETURNING
      VALUE(result) TYPE REF TO ZIF_ABAPPM_PACOTE
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS set_json
    IMPORTING
      !iv_json      TYPE string
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
      iv_version     TYPE string
      iv_abbreviated TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result)  TYPE string
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS packument
    RETURNING
      VALUE(result) TYPE string
    RAISING
      ZCX_ABAPPM_ERROR.

  METHODS tarball
    IMPORTING
      iv_filename   TYPE string
    RETURNING
      VALUE(result) TYPE xstring
    RAISING
      ZCX_ABAPPM_ERROR.

ENDINTERFACE.
