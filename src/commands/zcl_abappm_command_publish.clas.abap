CLASS zcl_abappm_command_publish DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
        !packument    TYPE zif_abappm_pacote=>ty_packument
        !package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_agent
      IMPORTING
        !iv_url       TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_http_agent
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_package_json
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_packument_from_registry
      IMPORTING
        !registry     TYPE string
        !package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result) TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tarball
      IMPORTING
        !package      TYPE devclass
        !package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS init_package
      IMPORTING
        !packument    TYPE zif_abappm_pacote=>ty_packument
        !package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result) TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS attach_package
      IMPORTING
        !version    TYPE string
        !tarball    TYPE xstring
      CHANGING
        !cs_publish TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS publish_package
      IMPORTING
        !registry     TYPE string
        !packument    TYPE zif_abappm_pacote=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_abap_version
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_publish IMPLEMENTATION.


  METHOD attach_package.

    DATA(attachment) = VALUE zif_abappm_pacote=>ty_attachment(
      key                  = |{ cs_publish-name }-{ version }.tgz|
      tarball-content_type = 'application/octet-stream'
      tarball-data         = cl_http_utility=>encode_x_base64( tarball )
      tarball-length       = xstrlen( tarball ) ).

    INSERT attachment INTO TABLE cs_publish-__attachments.

  ENDMETHOD.


  METHOD check_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      zcx_abappm_error=>raise( |{ package } does not exist or is not initialized| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_packument.

    READ TABLE packument-versions TRANSPORTING NO FIELDS
      WITH KEY key = package_json-version.
    IF sy-subrc = 0.
      zcx_abappm_error=>raise( |Version { package_json-version } already published| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_abap_version.

    TRY.
        DATA(semver) = NEW zcl_abappm_semver_sap( ).
        result = semver->sap_component_to_semver( 'SAP_BASIS' ).
      CATCH cx_root INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_agent.

    TRY.
        result = zcl_abappm_http_agent=>create( ).

        " TODO: Do we need this for a PUT request?
        result->global_headers( )->set(
          iv_key = 'accept'
          iv_val = 'application/json' ).

        result->global_headers( )->set(
          iv_key = 'content-type'
          iv_val = 'application/json' ).

        result->global_headers( )->set(
          iv_key = 'user-agent'
          iv_val = |apm/{ zif_abappm_version=>c_version } abap/{ get_abap_version( ) }| ).

        " Login manager requires git-like url so we add some dummy repo
        DATA(git_url) = iv_url && '/apm/apm.git'.

        " Get auth token
        IF zcl_abappm_http_login_manager=>get( git_url ) IS NOT INITIAL.
          result->global_headers( )->set(
            iv_key = 'authorization'
            iv_val = zcl_abappm_http_login_manager=>get( git_url ) ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_package_json.

    result = zcl_abappm_package_json=>factory( package )->load( )->get( ).
    result-readme = zcl_abappm_readme=>factory( package )->load( )->get( ).

  ENDMETHOD.


  METHOD get_packument_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(packument) = zcl_abappm_pacote=>factory(
      iv_registry = registry
      iv_name     = package_json-name )->packument( ).

    TRY.
        " TODO: ...
        " zcl_abappm_ajson=>parse( packument )->to_abap_corresponding_only( )->to_abap( IMPORTING ev_container = result ).

      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tarball.

    " TODO: Move this and all called methods to local part of class
    CONSTANTS c_null TYPE xstring VALUE ''.

    " 1. Serialize local objects
    TRY.
        DATA(logger) = NEW zcl_abapgit_log( ).

        DATA(local_settings) = VALUE zif_abapgit_persistence=>ty_local_settings(
          ignore_subpackages = abap_false
          only_local_objects = abap_false ).

        " TODO: Hardcoded to prefix
        DATA(dot_abapgit) = zcl_abapgit_dot_abapgit=>build_default( ).
        dot_abapgit->set_folder_logic( zif_abapgit_dot_abapgit=>c_folder_logic-prefix ).
        dot_abapgit->set_starting_folder( 'src' ).

        DATA(serializer) = NEW zcl_abapgit_serialize(
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
            iv_name     = <file>-file-path
            iv_content  = c_null
            iv_typeflag = zcl_abappm_tar=>c_typeflag-directory ).
        ENDIF.
      ENDAT.
      IF <file>-file-path = '/'.
        DATA(name) = <file>-file-filename.
      ELSE.
        name = |{ <file>-file-path }/{ <file>-file-filename }|.
      ENDIF.
      tar->append(
        iv_name    = name
        iv_content = <file>-file-data ).
    ENDLOOP.

    " 3. Add package.json and readme
    DATA(manifest) = CORRESPONDING zif_abappm_package_json_types=>ty_manifest( package_json ).

    DATA(json) = zcl_abappm_package_json=>convert_manifest_to_json(
      is_manifest     = manifest
      iv_package_json = abap_true ).

    TRY.
        tar->append(
          iv_name    = 'package.json'
          iv_content = zcl_abapgit_convert=>string_to_xstring_utf8( json ) ).

        tar->append(
          iv_name    = 'README.md'
          iv_content = zcl_abapgit_convert=>string_to_xstring_utf8( package_json-readme ) ).
      CATCH zcx_abapgit_exception INTO error.
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    result = tar->gzip( tar->save( ) ).

  ENDMETHOD.


  METHOD init_package.

    MOVE-CORRESPONDING package_json TO result.
    result-__id = package_json-name.

    " TODO: Allow publishing with other tag
    DATA(dist_tag) = VALUE zif_abappm_package_json_types=>ty_generic(
      key   = 'latest'
      value = package_json-version ).

    INSERT dist_tag INTO TABLE result-dist_tags.

    DATA(version) = VALUE zif_abappm_pacote=>ty_version( key = package_json-version ).

    MOVE-CORRESPONDING package_json TO version-version.
    version-version-__id           = |{ package_json-name }@{ package_json-version }|.
    version-version-__abap_version = get_abap_version( ).
    version-version-__apm_version  = zif_abappm_version=>c_version.

    INSERT version INTO TABLE result-versions.

  ENDMETHOD.


  METHOD publish_package.

    DATA(json) = zcl_abappm_pacote=>convert_packument_to_json( packument ).

    DATA(response) = get_agent( registry )->request(
      iv_url     = |{ registry }/{ packument-name }|
      iv_method  = 'PUT'
      iv_payload = json ).

    IF response->is_ok( ) = abap_false.
      result = |Error { response->code( ) } when publishing package|.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    " 1. Check if package exists and is initialized
    check_package( package ).

    " 2. Get package.abap.json and readme
    DATA(package_json) = get_package_json( package ).

    IF package_json-private = abap_true.
      zcx_abappm_error=>raise( 'This is a private package and can not be published' ).
    ENDIF.

    " 3. Get packument from registry
    " TODO: This should include request parameter for writing to the registry (only if not anonymous?)
    TRY.
        DATA(packument) = get_packument_from_registry(
          registry     = registry
          package_json = package_json ).
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
    DATA(tarball) = get_tarball(
      package      = package
      package_json = package_json ).

    " 7. Attach tarball to packument
    attach_package(
      EXPORTING
        version = package_json-version
        tarball = tarball
      CHANGING
        cs_publish = packument_publish ).

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
