CLASS /apmg/cl_apm_command_publish DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Publish Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry TYPE string
        !package  TYPE devclass
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS execute
      IMPORTING
        !registry TYPE string
        !package  TYPE devclass
      RAISING
        /apmg/cx_apm_error.

    METHODS check_package
      IMPORTING
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

    METHODS check_packument
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
        !package_json TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

    METHODS get_package_json
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

    METHODS serialize_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        /apmg/cx_apm_error.

    METHODS attach_object_list
      IMPORTING
        !files     TYPE zif_abapgit_definitions=>ty_files_item_tt
      CHANGING
        !packument TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    METHODS get_tar
      IMPORTING
        !package_json TYPE /apmg/if_apm_types=>ty_package_json
        !files        TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_tar
      RAISING
        /apmg/cx_apm_error.

    METHODS init_package
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
        !package_json TYPE /apmg/if_apm_types=>ty_package_json
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    METHODS attach_tarball
      IMPORTING
        !registry  TYPE string
        !version   TYPE string
        !tar       TYPE REF TO /apmg/cl_apm_tar
      CHANGING
        !packument TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    METHODS publish_package
      IMPORTING
        !registry     TYPE string
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_publish IMPLEMENTATION.


  METHOD attach_object_list.

    " Include a list of all objects which the registry will compare against the global object
    " directory (GTADIR) to avoid name conflicts with other packages
    LOOP AT files ASSIGNING FIELD-SYMBOL(<file>).
      DATA(item) = VALUE /apmg/if_apm_types=>ty_tadir_object(
        pgmid    = 'R3TR'
        object   = <file>-item-obj_type
        obj_name = <file>-item-obj_name ).

      COLLECT item INTO packument-_objects.
    ENDLOOP.

    SORT packument-_objects.

  ENDMETHOD.


  METHOD attach_tarball.

    DATA(tarball) = tar->gzip( tar->save( ) ).

    DATA(dist) = /apmg/cl_apm_command_utils=>get_integrity( tarball ).

    DATA(name) = packument-name.
    IF name(1) = '@'.
      SPLIT name AT '/' INTO DATA(rest) name ##NEEDED.
    ENDIF.
    DATA(filename) = |{ name }-{ version }.tgz|.

    dist-file_count    = tar->file_count( ).
    dist-unpacked_size = tar->unpacked_size( ).
    dist-tarball       = |{ registry }/{ name }/-/{ filename }|.

    packument-versions[ key = version ]-version-dist = dist.

    DATA(attachment) = VALUE /apmg/if_apm_types=>ty_attachment(
      key                  = filename
      tarball-content_type = /apmg/if_apm_http_agent=>c_content_type-bin
      tarball-data         = cl_http_utility=>encode_x_base64( tarball )
      tarball-length       = xstrlen( tarball ) ).

    INSERT attachment INTO TABLE packument-_attachments.

  ENDMETHOD.


  METHOD check_package.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |{ package } does not exist or is not initialized|.
    ENDIF.

  ENDMETHOD.


  METHOD check_packument.

    IF line_exists( packument-versions[ key = package_json-version ] ).
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Version { package_json-version } already published|.
    ENDIF.

  ENDMETHOD.


  METHOD execute.

    " 1. Check if package exists and is initialized
    check_package( package ).

    " 2. Get package.abap.json and readme
    DATA(package_json) = get_package_json( package ).

    IF package_json-private = abap_true.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Private packages can not be published'.
    ENDIF.

    " 3. Get packument from registry
    TRY.
        DATA(packument) = /apmg/cl_apm_command_utils=>get_packument_from_registry(
          registry = registry
          name     = package_json-name
          write    = abap_true ).
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
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

    " 6. Serialize all objects of package
    DATA(files) = serialize_package( package ).

    " 7. Attach object list to packument
    attach_object_list(
      EXPORTING
        files     = files
      CHANGING
        packument = packument_publish ).

    " 8. Get tarball
    DATA(tar) = get_tar(
      package_json = package_json
      files        = files ).

    " 9. Attach tarball to packument
    attach_tarball(
      EXPORTING
        registry  = registry
        version   = package_json-version
        tar       = tar
      CHANGING
        packument = packument_publish ).

    " 10. Publish package to registry
    DATA(message) = publish_package(
      registry  = registry
      packument = packument_publish ).

    IF message IS INITIAL.
      MESSAGE 'Package successfully published' TYPE 'S'.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_json.

    result = /apmg/cl_apm_package_json=>factory( package )->load( )->get( ).
    result-readme = /apmg/cl_apm_readme=>factory( package )->load( )->get( ).

  ENDMETHOD.


  METHOD get_tar.

    " TODO: Move this and all called methods to local part of class
    CONSTANTS c_null TYPE xstring VALUE ''.

    " 2. Tar and gzip files
    DATA(tar) = /apmg/cl_apm_tar=>new( ).

    LOOP AT files ASSIGNING FIELD-SYMBOL(<file>).
      AT NEW file-path.
        IF <file>-file-path <> '/'.
          tar->append(
            name     = |{ <file>-file-path+1 }|
            content  = c_null
            typeflag = /apmg/cl_apm_tar=>c_typeflag-directory ).
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
    DATA(manifest) = CORRESPONDING /apmg/if_apm_types=>ty_manifest( package_json ).

    DATA(json) = /apmg/cl_apm_package_json=>convert_manifest_to_json(
      manifest        = manifest
      is_package_json = abap_true ).

    TRY.
        tar->append(
          name    = CONV string( /apmg/if_apm_types=>c_package_json_file )
          content = zcl_abapgit_convert=>string_to_xstring_utf8( json ) ).

        tar->append(
          name    = /apmg/if_apm_types=>c_readme_file
          content = zcl_abapgit_convert=>string_to_xstring_utf8( package_json-readme ) ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

    result = tar.

  ENDMETHOD.


  METHOD init_package.

    CONSTANTS c_latest TYPE string VALUE 'latest'.

    IF packument IS INITIAL.
      result        = CORRESPONDING #( package_json ).
      result-_id    = package_json-name.
    ELSE.
      result = packument.
    ENDIF.

    CLEAR:
      result-_rev,
      result-author,
      result-bugs,
      result-dist_tags,
      result-homepage,
      result-icon,
      result-keywords,
      result-license,
      result-maintainers,
      result-readme,
      result-repository,
      result-time,
      result-users,
      result-versions.

    " Update LATEST dist-tag
    " TODO: Allow publishing with other tags
    DATA(dist_tag) = VALUE /apmg/if_apm_types=>ty_generic(
      key   = c_latest
      value = package_json-version ).

    INSERT dist_tag INTO TABLE result-dist_tags.

    " Add new version
    DATA(version) = VALUE /apmg/if_apm_types=>ty_version( key = package_json-version ).

    version-version               = CORRESPONDING #( package_json ).
    version-version-_id           = |{ package_json-name }@{ package_json-version }|.
    version-version-_abap_version = /apmg/cl_apm_command_utils=>get_abap_version( ).
    version-version-_apm_version  = /apmg/if_apm_version=>c_version.

    INSERT version INTO TABLE result-versions.

  ENDMETHOD.


  METHOD publish_package.

    DATA(json) = /apmg/cl_apm_pacote=>convert_packument_to_json( packument ).

    DATA(response) = /apmg/cl_apm_command_utils=>fetch_registry(
      registry = registry
      url      = |{ registry }/{ packument-name }|
      method   = /apmg/if_apm_http_agent=>c_method-put
      payload  = json ).

    result = /apmg/cl_apm_command_utils=>check_response(
      response = response
      text     = 'Error publishing package' ).

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_publish( ).

    command->execute(
      registry = registry
      package  = package ).

  ENDMETHOD.


  METHOD serialize_package.

    TRY.
        DATA(logger) = NEW zcl_abapgit_log( ).

        DATA(local_settings) = VALUE zif_abapgit_persistence=>ty_local_settings(
          ignore_subpackages = abap_false
          only_local_objects = abap_false ).

        " Hardcoded to prefix folder logic and /src/ starting folder
        " TODO!: Support full and mixed folder logic
        DATA(dot_abapgit) = zcl_abapgit_dot_abapgit=>build_default( ).

        DATA(serializer) = NEW /apmg/cl_apm_abapgit_serialize(
          io_dot_abapgit    = dot_abapgit
          is_local_settings = local_settings ).

        result = serializer->files_local(
          iv_package = package
          ii_log     = logger ).

        SORT result BY file-path file-filename.
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
