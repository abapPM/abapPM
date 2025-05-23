CLASS zcl_abappm_command_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Command Utilities
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS get_packument_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_manifest_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_manifest
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tarball_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !tarball      TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS install_package
      IMPORTING
        !registry      TYPE string
        !manifest      TYPE zif_abappm_types=>ty_manifest
        !package       TYPE devclass
        !name          TYPE string
        !version       TYPE string
        !is_production TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall_package
      IMPORTING
        !name    TYPE string
        !version TYPE string
        !package TYPE devclass
      RAISING
        zcx_abappm_error.

    CLASS-METHODS check_integrity
      IMPORTING
        !tarball TYPE xstring
        !dist    TYPE zif_abappm_types=>ty_dist
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_integrity
      IMPORTING
        !tarball      TYPE xstring
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_dist
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_abappm_command_utils IMPLEMENTATION.


  METHOD check_integrity.

    TRY.
        DATA(shasum) = zcl_abapgit_hash=>sha1_raw( tarball ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    IF shasum <> dist-shasum.
      zcx_abappm_error=>raise( 'Checksum error for tarball (sha1)' ).
    ENDIF.

    " TODO: check dist-integrity (sha512)
    " https://www.npmjs.com/package/ssri

  ENDMETHOD.


  METHOD get_integrity.

    TRY.
        DATA(shasum) = zcl_abapgit_hash=>sha1_raw( tarball ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    " TODO: determine integrity (sha512)
    " https://www.npmjs.com/package/ssri

    result = VALUE #(
      shasum        = shasum
      integrity     = '' ).

  ENDMETHOD.


  METHOD get_manifest_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(manifest) = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name )->manifest( version ).

    result = zcl_abappm_package_json=>convert_json_to_manifest( manifest ).

  ENDMETHOD.


  METHOD get_packument_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    result = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name )->get( ).

  ENDMETHOD.


  METHOD get_tarball_from_registry.

    result = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name )->tarball( tarball ).

  ENDMETHOD.


  METHOD install_package.

    " TODO: Currently hardcoded to local packages (no transport)
    DATA transport TYPE trkorr.

    DATA(tarball) = get_tarball_from_registry(
      registry = registry
      name     = name
      tarball  = manifest-dist-tarball ).

    check_integrity(
      tarball = tarball
      dist    = manifest-dist ).

    " FUTURE: Allow other folder logic than prefix
    zcl_abappm_installer=>install(
      name              = name
      version           = version
      data              = tarball
      package           = package
      transport         = transport
      enum_zip          = zcl_abappm_installer=>c_enum_zip-registry
      enum_folder_logic = zcl_abappm_installer=>c_enum_folder_logic-prefix
      is_production     = is_production ).

  ENDMETHOD.


  METHOD uninstall_package.

    zcl_abappm_installer=>uninstall(
      name    = name
      version = version
      package = package ).

  ENDMETHOD.
ENDCLASS.
