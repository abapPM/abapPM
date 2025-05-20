CLASS zcl_abappm_command_publish DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Publish Command
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
        !package  TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_package
      IMPORTING
        !package TYPE devclass
      RAISING
        zcx_abappm_error.

    CLASS-METHODS check_packument
      IMPORTING
        !packument    TYPE zif_abappm_types=>ty_packument
        !package_json TYPE zif_abappm_types=>ty_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_package_json
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tar
      IMPORTING
        !package      TYPE devclass
        !package_json TYPE zif_abappm_types=>ty_package_json
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tar
      RAISING
        zcx_abappm_error.

    CLASS-METHODS init_package
      IMPORTING
        !packument    TYPE zif_abappm_types=>ty_packument
        !package_json TYPE zif_abappm_types=>ty_package_json
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS attach_tarball
      IMPORTING
        !version   TYPE string
        !tar       TYPE REF TO zcl_abappm_tar
      CHANGING
        !packument TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS publish_package
      IMPORTING
        !registry     TYPE string
        !packument    TYPE zif_abappm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_publish IMPLEMENTATION.


  METHOD attach_tarball.

    DATA(tarball) = tar->gzip( tar->save( ) ).

    DATA(dist) = zcl_abappm_command_utils=>get_integrity( tarball ).

    DATA(tarball_name) = packument-name.
    IF tarball_name(1) = '@'.
      SPLIT tarball_name AT '/' INTO DATA(rest) tarball_name ##NEEDED.
    ENDIF.

    dist-file_count    = tar->file_count( ).
    dist-unpacked_size = tar->unpacked_size( ).
    dist-tarball       = |{ tarball_name }-{ version }.tgz|.

    packument-versions[ key = version ]-version-dist = dist.

    DATA(attachment) = VALUE zif_abappm_types=>ty_attachment(
      key                  = dist-tarball
      tarball-content_type = zif_abappm_http_agent=>c_content_type-bin
      tarball-data         = cl_http_utility=>encode_x_base64( tarball )
      tarball-length       = xstrlen( tarball ) ).

    INSERT attachment INTO TABLE packument-_attachments.

  ENDMETHOD.


  METHOD check_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      zcx_abappm_error=>raise( |{ package } does not exist or is not initialized| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_packument.

    IF line_exists( packument-versions[ key = package_json-version ] ).
      zcx_abappm_error=>raise( |Version { package_json-version } already published| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_package_json.

    result = zcl_abappm_package_json=>factory( package )->load( )->get( ).
    result-readme = zcl_abappm_readme=>factory( package )->load( )->get( ).

  ENDMETHOD.


  METHOD get_tar.

    " TODO: Move this and all called methods to local part of class
    CONSTANTS c_null TYPE xstring VALUE ''.

    " 1. Serialize local objects
    TRY.
        DATA(logger) = NEW zcl_abapgit_log( ).

        DATA(local_settings) = VALUE zif_abapgit_persistence=>ty_local_settings(
          ignore_subpackages = abap_false
          only_local_objects = abap_false ).

        " TODO!: Hardcoded to prefix and /src/ starting folder
        DATA(dot_abapgit) = zcl_abapgit_dot_abapgit=>build_default( ).

        DATA(serializer) = NEW zcl_abappm_abapgit_serialize(
          io_dot_abapgit    = dot_abapgit
          is_local_settings = local_settings ).

        DATA(files) = serializer->files_local(
          iv_package = package
          ii_log     = logger ).

        SORT files BY file-path file-filename.
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    " 2. Tar and gzip files
    DATA(tar) = zcl_abappm_tar=>new( ).

    LOOP AT files ASSIGNING FIELD-SYMBOL(<file>).
      AT NEW file-path.
        IF <file>-file-path <> '/'.
          tar->append(
            name     = |{ <file>-file-path+1 }|
            content  = c_null
            typeflag = zcl_abappm_tar=>c_typeflag-directory ).
        ENDIF.
      ENDAT.
      IF <file>-file-path = '/'.
        DATA(name) = <file>-file-filename.
      ELSE.
        name = |{ <file>-file-path+1 }{ <file>-file-filename }|.
      ENDIF.
      tar->append(
        name    = name
        content = <file>-file-data ).
    ENDLOOP.

    " 3. Add package.json and readme
    DATA(manifest) = CORRESPONDING zif_abappm_types=>ty_manifest( package_json ).

    DATA(json) = zcl_abappm_package_json=>convert_manifest_to_json(
      manifest        = manifest
      is_package_json = abap_true ).

    TRY.
        tar->append(
          name    = CONV string( zif_abappm_types=>c_package_json_file )
          content = zcl_abapgit_convert=>string_to_xstring_utf8( json ) ).

        tar->append(
          name    = zif_abappm_types=>c_readme_file
          content = zcl_abapgit_convert=>string_to_xstring_utf8( package_json-readme ) ).
      CATCH zcx_abapgit_exception INTO error.
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    DATA(debug) = 'X'.
    IF debug = abap_true.
      TRY.
          zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
            EXPORTING
              iv_path = 'C:\Temp\test.tar'
              iv_xstr = tar->save( ) ).
        CATCH zcx_abapgit_exception.
          BREAK-POINT.
      ENDTRY.
    ENDIF.

    result = tar.

  ENDMETHOD.


  METHOD init_package.

    CONSTANTS c_latest TYPE string VALUE 'latest'.

    IF packument IS INITIAL.
      result        = CORRESPONDING #( package_json ).
      result-_id    = package_json-name.
      result-readme = ''. " sufficient to have it in version (below)
    ELSE.
      result = packument.
    ENDIF.

    " Update dist-tag
    " TODO: Allow publishing with other tags
    DATA(dist_tag) = VALUE zif_abappm_types=>ty_generic(
      key   = c_latest
      value = package_json-version ).

    DELETE result-dist_tags WHERE key = c_latest.
    INSERT dist_tag INTO TABLE result-dist_tags.

    " Add new version
    DATA(version) = VALUE zif_abappm_types=>ty_version( key = package_json-version ).

    version-version                = CORRESPONDING #( package_json ).
    version-version-_id           = |{ package_json-name }@{ package_json-version }|.
    version-version-_abap_version = zcl_abappm_command_utils=>get_abap_version( ).
    version-version-_apm_version  = zif_abappm_version=>c_version.

    INSERT version INTO TABLE result-versions.

  ENDMETHOD.


  METHOD publish_package.

    DATA(json) = zcl_abappm_pacote=>convert_packument_to_json( packument ).

    DATA(debug) = 'X'.
    IF debug = abap_true.
      TRY.
          zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
            EXPORTING
              iv_path = 'C:\Temp\test.json'
              iv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( json ) ).
        CATCH zcx_abapgit_exception.
          BREAK-POINT.
      ENDTRY.
    ENDIF.

    DATA(response) = zcl_abappm_command_utils=>get_agent( registry )->request(
      url     = |{ registry }/{ packument-name }|
      method  = zif_abappm_http_agent=>c_method-put
      payload = json ).

    IF response->is_ok( ) = abap_false.
      result = |Error { response->code( ) } when publishing package: |
        && zcl_abappm_command_utils=>get_error( response->error( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD run.

    " 1. Check if package exists and is initialized
    check_package( package ).

    " 2. Get package.abap.json and readme
    DATA(package_json) = get_package_json( package ).

    IF package_json-private = abap_true.
      zcx_abappm_error=>raise( 'Private packages can not be published' ).
    ENDIF.

    " 3. Get packument from registry
    TRY.
        DATA(packument) = zcl_abappm_command_utils=>get_packument_from_registry(
          registry = registry
          name     = package_json-name
          write    = abap_true ).
      CATCH zcx_abappm_error ##NO_HANDLER.
        " ignore if not found
    ENDTRY.

    " 4. Check if version already exist in registry
    check_packument(
      packument    = packument
      package_json = package_json ).

    " 5. Initialize packument for publishing
    DATA(packument_publish) = init_package(
      packument    = packument
      package_json = package_json ).

    " 6. Get tarball
    DATA(tar) = get_tar(
      package      = package
      package_json = package_json ).

    " 7. Attach tarball to packument
    attach_tarball(
      EXPORTING
        version   = package_json-version
        tar       = tar
      CHANGING
        packument = packument_publish ).

    " 8. Publish package to registry
    DATA(message) = publish_package(
      registry  = registry
      packument = packument_publish ).

    IF message IS INITIAL.
      MESSAGE 'Package successfully published' TYPE 'S'.
    ELSE.
      zcx_abappm_error=>raise( message ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
