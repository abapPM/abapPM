CLASS zcl_abappm_command_unpublish DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Unpublish Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry TYPE string
        !name     TYPE string
        !version  TYPE string
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_tarball
      IMPORTING
        !packument    TYPE zif_abappm_types=>ty_packument
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS remove_version
      IMPORTING
        !packument    TYPE zif_abappm_types=>ty_packument
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS delete_tarball
      IMPORTING
        !registry     TYPE string
        !packument    TYPE zif_abappm_types=>ty_packument
        !tarball      TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS unpublish_package
      IMPORTING
        !registry     TYPE string
        !packument    TYPE zif_abappm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_unpublish IMPLEMENTATION.


  METHOD delete_tarball.

    DATA(response) = zcl_abappm_command_utils=>get_agent( registry )->request(
      url     = |{ tarball }/rev/{ packument-_rev }|
      method  = zif_abappm_http_agent=>c_method-delete ).

    IF response->is_ok( ) = abap_false.
      result = |Error { response->code( ) } when deleting tarball: |
        && zcl_abappm_command_utils=>get_error( response->error( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_tarball.

    READ TABLE packument-versions ASSIGNING FIELD-SYMBOL(<version>) WITH KEY key = version.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Version { version } does not exist in package { packument-name }| ).
    ENDIF.

    result = <version>-version-dist-tarball.

  ENDMETHOD.


  METHOD remove_version.

    result = packument.

    DELETE result-versions WHERE key = version.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Version { version } does not exist in package { packument-name }| ).
    ENDIF.

    DELETE result-time WHERE key = version ##SUBRC_OK.

  ENDMETHOD.


  METHOD run.

    " 1. Get packument from registry
    DATA(packument) = zcl_abappm_command_utils=>get_packument_from_registry(
      registry = registry
      name     = name
      write    = abap_true ).

    " 2. Get tarball name
    DATA(tarball) = get_tarball(
      packument = packument
      version   = version ).

    " 3. Remove version from packument
    packument = remove_version(
      packument = packument
      version   = version ).

    " 4. Unpublish package from registry
    DATA(message) = unpublish_package(
      registry  = registry
      packument = packument ).

    IF message IS NOT INITIAL.
      zcx_abappm_error=>raise( message ).
    ENDIF.

    " 5. Delete tarball from registry
    message = delete_tarball(
      registry  = registry
      packument = packument
      tarball   = tarball ).

    IF message IS INITIAL.
      MESSAGE 'Package version successfully unpublished' TYPE 'S'.
    ELSE.
      zcx_abappm_error=>raise( message ).
    ENDIF.

  ENDMETHOD.


  METHOD unpublish_package.

    DATA(json) = zcl_abappm_pacote=>convert_packument_to_json( packument ).

    DATA(response) = zcl_abappm_command_utils=>get_agent( registry )->request(
      url     = |{ registry }/{ packument-name }/rev/{ packument-_rev }|
      method  = zif_abappm_http_agent=>c_method-put
      payload = json ).

    IF response->is_ok( ) = abap_false.
      result = |Error { response->code( ) } when unpublishing package: |
        && zcl_abappm_command_utils=>get_error( response->error( ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
