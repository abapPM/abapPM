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
        iv_tarball TYPE xstring
      CHANGING
        cs_publish TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_agent
      IMPORTING
        !iv_url       TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_http_agent
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_package_json
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_readme
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE string
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

    CLASS-METHODS publish_package
      IMPORTING
        !iv_registry  TYPE string
        !is_packument TYPE zif_abappm_pacote=>ty_packument
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_publish IMPLEMENTATION.


  METHOD attach_package.

    DATA ls_attachment TYPE LINE OF zif_abappm_pacote=>ty_packument-__attachments.

    ls_attachment-tarball-content_type = 'application/octet-stream'.
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


  METHOD get_agent.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception,
      lv_url   TYPE string.

    TRY.
        result = zcl_abapgit_factory=>get_http_agent( ).

        " TODO: Do we need this for a PUT request?
        result->global_headers( )->set(
          iv_key = 'Accept'
          iv_val = 'application/json' ).

        " Login manager requires git-like url so we add some dummy repo
        lv_url = iv_url && '/apm/apm.git'.

        " Get auth token from repo
        IF zcl_abapgit_login_manager=>get( lv_url ) IS NOT INITIAL.
          result->global_headers( )->set(
            iv_key = 'Authorization'
            iv_val = zcl_abapgit_login_manager=>get( lv_url ) ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_package_json.
    result = zcl_abappm_package_json=>factory( iv_package )->load( )->get( ).
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


  METHOD get_readme.
    result = zcl_abappm_readme=>factory( iv_package )->load( )->get( ).
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
    ls_version-version-__abap_version = sy-saprl.
    ls_version-version-__apm_version  = zif_abappm_version=>c_version.
    INSERT ls_version INTO TABLE result-versions.

  ENDMETHOD.


  METHOD publish_package.

    DATA:
      lv_json  TYPE string,
      lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        result = get_agent( iv_registry )->request(
          iv_url     = |{ iv_registry }/{ is_packument-name }|
          iv_method  = 'PUT'
          iv_payload = lv_json )->is_ok( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

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


    " 7. Attach tarball to packument
    attach_package(
      EXPORTING
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
