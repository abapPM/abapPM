CLASS zcl_abappm_command_publish DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_registry TYPE string
        !iv_package  TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_package
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abappm_error.

    CLASS-METHODS check_packument
      IMPORTING
        !is_packument    TYPE zif_abappm_pacote=>ty_packument
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
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
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_packument_from_registry
      IMPORTING
        !iv_registry     TYPE string
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result)    TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tarball
      IMPORTING
        !iv_package      TYPE devclass
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result)    TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS init_package
      IMPORTING
        !is_packument    TYPE zif_abappm_pacote=>ty_packument
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result)    TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS attach_package
      IMPORTING
        !iv_version TYPE string
        !iv_tarball TYPE xstring
      CHANGING
        !cs_publish TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS publish_package
      IMPORTING
        !iv_registry  TYPE string
        !is_packument TYPE zif_abappm_pacote=>ty_packument
      RETURNING
        VALUE(result) TYPE abap_bool
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

    DATA ls_attachment TYPE LINE OF zif_abappm_pacote=>ty_packument-__attachments.

    ls_attachment-key                  = |{ cs_publish-name }-{ iv_version }.tgz|.
    ls_attachment-tarball-content_type = 'application/octet-stream'.
    ls_attachment-tarball-data         = cl_http_utility=>encode_x_base64( iv_tarball ).
    ls_attachment-tarball-length       = xstrlen( iv_tarball ).
    INSERT ls_attachment INTO TABLE cs_publish-__attachments.

  ENDMETHOD.


  METHOD check_package.

    DATA:
      lv_name         TYPE string,
      li_package_json TYPE REF TO zif_abappm_package_json.

    li_package_json = zcl_abappm_package_json=>factory( iv_package ).

    IF li_package_json->exists( ) = abap_false.
      zcx_abappm_error=>raise( |{ iv_package } does not exist or is not initialized| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_packument.

    READ TABLE is_packument-versions TRANSPORTING NO FIELDS
      WITH KEY key = is_package_json-version.
    IF sy-subrc = 0.
      zcx_abappm_error=>raise( |Version { is_package_json-version } already published| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_abap_version.

    DATA lo_semver TYPE REF TO zcl_abappm_semver_sap.
    DATA lx_error TYPE REF TO cx_root.

    TRY.
        result = lo_semver->sap_component_to_semver( 'SAP_BASIS' ).
      CATCH cx_root INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_agent.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception,
      lv_url   TYPE string.

    TRY.
        result = zcl_abappm_http_agent=>create( ).

        " TODO: Do we need this for a PUT request?
        result->global_headers( )->set(
          iv_key = 'Accept'
          iv_val = 'application/json' ).

        " Login manager requires git-like url so we add some dummy repo
        lv_url = iv_url && '/apm/apm.git'.

        " Get auth token
        IF zcl_abappm_http_login_manager=>get( lv_url ) IS NOT INITIAL.
          result->global_headers( )->set(
            iv_key = 'Authorization'
            iv_val = zcl_abappm_http_login_manager=>get( lv_url ) ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_package_json.
    result = zcl_abappm_package_json=>factory( iv_package )->load( )->get( ).
    result-readme = zcl_abappm_readme=>factory( iv_package )->load( )->get( ).
  ENDMETHOD.


  METHOD get_packument_from_registry.

    DATA:
      lx_ajson_error TYPE REF TO zcx_abappm_ajson_error,
      li_json        TYPE REF TO zif_abappm_ajson,
      ls_dependency  TYPE zif_abappm_package_json_types=>ty_dependency,
      lv_packument   TYPE string.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    lv_packument = zcl_abappm_pacote=>factory(
      iv_registry = iv_registry
      iv_name     = is_package_json-name )->packument( ).

    TRY.
        " zcl_abappm_ajson=>parse( lv_packument )->to_abap_corresponding_only( )->to_abap( IMPORTING ev_container = result ).

      CATCH zcx_abappm_ajson_error INTO lx_ajson_error.
        zcx_abappm_error=>raise_with_text( lx_ajson_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tarball.

    " TODO: Move this and all called methods to local part of class
    CONSTANTS lc_null TYPE xstring VALUE ''.

    DATA:
      ls_local_settings TYPE zif_abapgit_persistence=>ty_local_settings,
      lo_dot_abapgit    TYPE REF TO zcl_abapgit_dot_abapgit,
      lt_files          TYPE zif_abapgit_definitions=>ty_files_item_tt,
      lo_serialize      TYPE REF TO zcl_abapgit_serialize,
      lo_tar            TYPE REF TO zcl_abappm_tar,
      ls_manifest       TYPE zif_abappm_package_json_types=>ty_manifest,
      lv_json           TYPE string,
      lv_name           TYPE string,
      lv_data           TYPE xstring,
      li_log            TYPE REF TO zif_abapgit_log,
      lx_error          TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS:
      <ls_file> LIKE LINE OF lt_files.


    " 1. Serialize local objects
    TRY.
        CREATE OBJECT li_log TYPE zcl_abapgit_log.

        ls_local_settings-ignore_subpackages = abap_false.
        ls_local_settings-only_local_objects = abap_false.

        " TODO: Hardcoded to prefix
        lo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
        lo_dot_abapgit->set_folder_logic( zif_abapgit_dot_abapgit=>c_folder_logic-prefix ).
        lo_dot_abapgit->set_starting_folder( 'src' ).

        CREATE OBJECT lo_serialize
          EXPORTING
            io_dot_abapgit    = lo_dot_abapgit
            is_local_settings = ls_local_settings.

        lt_files = lo_serialize->files_local(
          iv_package = iv_package
          ii_log     = li_log ).

        SORT lt_files BY file-path file-filename.
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    " 2. Tar and gzip files
    lo_tar = zcl_abappm_tar=>new( ).

    LOOP AT lt_files ASSIGNING <ls_file>.
      AT NEW file-path.
        IF <ls_file>-file-path <> '/'.
          lo_tar->append(
            iv_name     = <ls_file>-file-path
            iv_content  = lc_null
            iv_typeflag = zcl_abappm_tar=>c_typeflag-directory ).
        ENDIF.
      ENDAT.
      IF <ls_file>-file-path = '/'.
        lv_name = <ls_file>-file-filename.
      ELSE.
        lv_name = |{ <ls_file>-file-path }/{ <ls_file>-file-filename }|.
      ENDIF.
      lo_tar->append(
        iv_name    = lv_name
        iv_content = <ls_file>-file-data ).
    ENDLOOP.

    " 3. Add package.json and readme
    MOVE-CORRESPONDING is_package_json TO ls_manifest.
    lv_json = zcl_abappm_package_json=>convert_manifest_to_json(
      is_manifest     = ls_manifest
      iv_package_json = abap_true ).

    TRY.
        lo_tar->append(
          iv_name    = 'package.json'
          iv_content = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json ) ).

        lo_tar->append(
          iv_name    = 'README.md'
          iv_content = zcl_abapgit_convert=>string_to_xstring_utf8( is_package_json-readme ) ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    lv_data = lo_tar->save( ).
    result = lo_tar->gzip( lv_data ).

  ENDMETHOD.


  METHOD init_package.

    DATA:
      ls_dist_tag TYPE LINE OF zif_abappm_pacote=>ty_packument-dist_tags,
      ls_version  TYPE LINE OF zif_abappm_pacote=>ty_packument-versions.

    result-__id        = is_package_json-name.
    result-name        = is_package_json-name.
    result-description = is_package_json-description.

    " TODO: Publish with other tag
    ls_dist_tag-key   = 'latest'.
    ls_dist_tag-value = is_package_json-version.
    INSERT ls_dist_tag INTO TABLE result-dist_tags.

    ls_version-key     = is_package_json-version.
    MOVE-CORRESPONDING is_package_json TO ls_version-version.
    ls_version-version-__id           = |{ is_package_json-name }@{ is_package_json-version }|.
    ls_version-version-__abap_version = get_abap_version( ).
    ls_version-version-__apm_version  = zif_abappm_version=>c_version.
    INSERT ls_version INTO TABLE result-versions.

  ENDMETHOD.


  METHOD publish_package.

    DATA lv_json TYPE string.

    lv_json = zcl_abappm_pacote=>convert_packument_to_json( is_packument ).

    result = get_agent( iv_registry )->request(
      iv_url     = |{ iv_registry }/{ is_packument-name }|
      iv_method  = 'PUT'
      iv_payload = lv_json )->is_ok( ).

  ENDMETHOD.


  METHOD run.

    DATA:
      ls_package_json TYPE zif_abappm_package_json_types=>ty_package_json,
      ls_packument    TYPE zif_abappm_pacote=>ty_packument,
      ls_publish      TYPE zif_abappm_pacote=>ty_packument,
      lv_success      TYPE abap_bool,
      lv_tarball      TYPE xstring.

    " 1. Check if package exists and is initialized
    check_package( iv_package ).

    " 2. Get package.abap.json and readme
    ls_package_json = get_package_json( iv_package ).

    " 3. Get packument from registry
    " TODO: This should include request parameter for writing to the registry (only if not anonymous?)
    TRY.
        ls_packument = get_packument_from_registry(
          iv_registry     = iv_registry
          is_package_json = ls_package_json ).
      CATCH zcx_abappm_error ##NO_HANDLER.
        " ignore if not found
    ENDTRY.

    " 4. Check if version already exist in registry
    check_packument(
      is_packument    = ls_packument
      is_package_json = ls_package_json ).

    " 5. Initialize packument for publishing
    ls_publish = init_package(
      is_packument    = ls_packument
      is_package_json = ls_package_json ).

    " 6. Get tarball
    lv_tarball = get_tarball(
      iv_package      = iv_package
      is_package_json = ls_package_json ).

    " 7. Attach tarball to packument
    attach_package(
      EXPORTING
        iv_version = ls_package_json-version
        iv_tarball = lv_tarball
      CHANGING
        cs_publish = ls_publish ).

    " 8. Publish package to registry
    lv_success = publish_package(
      iv_registry  = iv_registry
      is_packument = ls_publish ).

    MESSAGE 'Package successfully published' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
