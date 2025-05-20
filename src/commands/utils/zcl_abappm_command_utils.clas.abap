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
        !write        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_manifest_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !version      TYPE string
        !write        TYPE abap_bool DEFAULT abap_false
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

    CLASS-METHODS get_error
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_agent
      IMPORTING
        !iv_url       TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_http_agent
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_abap_version
      RETURNING
        VALUE(result) TYPE string
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


  METHOD get_abap_version.

    TRY.
        DATA(semver) = NEW zcl_abappm_semver_sap( ).
        result = semver->sap_component_to_semver( 'SAP_BASIS' ).
      CATCH cx_abap_invalid_value INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_agent.

    result = zcl_abappm_http_agent=>create( ).

    " TODO: Do we need this for a PUT request?
    result->global_headers( )->set(
      iv_key = zif_abappm_http_agent=>c_header-accept
      iv_val = zif_abappm_http_agent=>c_content_type-json ).

    result->global_headers( )->set(
      iv_key = zif_abappm_http_agent=>c_header-content_type
      iv_val = zif_abappm_http_agent=>c_content_type-json ).

    result->global_headers( )->set(
      iv_key = zif_abappm_http_agent=>c_header-user_agent
      iv_val = |apm/{ zif_abappm_version=>c_version } abap/{ get_abap_version( ) }| ).

    DATA(urlc) = zcl_abappm_url=>parse( iv_url )->components.

    " Get/set auth token
    IF zcl_abappm_http_login_manager=>get( urlc-host ) IS NOT INITIAL.
      result->global_headers( )->set(
        iv_key = zif_abappm_http_agent=>c_header-authorization
        iv_val = zcl_abappm_http_login_manager=>get( urlc-host ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_error.

    TRY.
        result = zcl_abappm_ajson=>parse( json )->get_string( '/error' ).
        IF result IS NOT INITIAL.
          result = to_upper( result(1) ) && result+1.
        ENDIF.
      CATCH zcx_abappm_ajson_error.
        result = 'Unknown error'.
    ENDTRY.

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
      shasum    = shasum
      integrity = '' ).

  ENDMETHOD.


  METHOD get_manifest_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(manifest) = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name
    )->manifest(
      version = version
      write   = write ).

    result = zcl_abappm_package_json=>convert_json_to_manifest( manifest ).

  ENDMETHOD.


  METHOD get_packument_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(pacote) = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name ).

    pacote->packument( write ).

    result = pacote->get( ).

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
