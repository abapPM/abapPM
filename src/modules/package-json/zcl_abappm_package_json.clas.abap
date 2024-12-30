CLASS zcl_abappm_package_json DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Package JSON
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abappm_package_json.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !package      TYPE devclass
        !name         TYPE string OPTIONAL
        !version      TYPE string OPTIONAL
        !private      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS injector
      IMPORTING
        !package TYPE devclass
        !mock    TYPE REF TO zif_abappm_package_json.

    METHODS constructor
      IMPORTING
        !package TYPE devclass
        !name    TYPE string OPTIONAL
        !version TYPE string OPTIONAL
        !private TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS list
      IMPORTING
        !filter       TYPE string OPTIONAL
        !instanciate  TYPE abap_bool DEFAULT abap_false
        !is_bundle    TYPE abap_bool DEFAULT abap_undefined
      RETURNING
        VALUE(result) TYPE zif_abappm_package_json=>ty_packages.

    CLASS-METHODS get_package_key
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_persist_apm=>ty_key.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !key          TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE devclass.

    CLASS-METHODS convert_json_to_manifest
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_manifest
      RAISING
        zcx_abappm_error.

    CLASS-METHODS convert_json_to_manifest_abbr
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_manifest_abbreviated
      RAISING
        zcx_abappm_error.

    CLASS-METHODS convert_manifest_to_json
      IMPORTING
        !manifest        TYPE zif_abappm_types=>ty_manifest
        !is_package_json TYPE abap_bool DEFAULT abap_false
        !is_complete     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE string
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO zif_abappm_package_json,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      db_persist TYPE REF TO zif_abappm_persist_apm,
      instances  TYPE ty_instances.

    DATA:
      key      TYPE zif_abappm_persist_apm=>ty_key,
      package  TYPE devclass,
      manifest TYPE zif_abappm_types=>ty_manifest.

    CLASS-METHODS check_manifest
      IMPORTING
        !manifest TYPE zif_abappm_types=>ty_manifest
      RAISING
        zcx_abappm_error.

    CLASS-METHODS sort_manifest
      IMPORTING
        !manifest     TYPE zif_abappm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_manifest.

ENDCLASS.



CLASS zcl_abappm_package_json IMPLEMENTATION.


  METHOD check_manifest.

    DATA(issues) = zcl_abappm_package_json_valid=>check( manifest ).

    IF issues IS NOT INITIAL.
      zcx_abappm_error=>raise( |Invalid package json:\n{ concat_lines_of( table = issues sep = |\n| ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.

    db_persist = zcl_abappm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    IF zcl_abappm_package_json_valid=>is_valid_sap_package( package ) = abap_false.
      zcx_abappm_error=>raise( |Invalid package: { package }| ).
    ENDIF.

    me->package      = package.
    manifest-name    = name.
    manifest-version = version.
    manifest-private = private.

    key = get_package_key( package ).

    TRY.
        zif_abappm_package_json~load( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD convert_json_to_manifest.

    " TODO: AJSON does not allow for mapping of ABAP to JSON objects like { "user1", "user2", ... }
    " A table would map to an array [ "user1", "user2", ... ]

    TYPES:
      " Copy of schema but without dependencies (instead of array)
      BEGIN OF ty_package_json_partial,
        name                TYPE string,
        version             TYPE string,
        description         TYPE string,
        keywords            TYPE string_table,
        homepage            TYPE string,
        BEGIN OF bugs,
          url   TYPE zif_abappm_types=>ty_uri,
          email TYPE zif_abappm_types=>ty_email,
        END OF bugs,
        license             TYPE string,
        author              TYPE zif_abappm_types=>ty_person,
        contributors        TYPE STANDARD TABLE OF zif_abappm_types=>ty_person WITH KEY name,
        maintainers         TYPE STANDARD TABLE OF zif_abappm_types=>ty_person WITH KEY name,
        main                TYPE string,
        man                 TYPE string_table,
        type                TYPE string,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE zif_abappm_types=>ty_uri,
          directory TYPE string,
        END OF repository,
        BEGIN OF funding,
          type TYPE string,
          url  TYPE zif_abappm_types=>ty_uri,
        END OF funding,
        bundle_dependencies TYPE string_table,
        os                  TYPE string_table,
        cpu                 TYPE string_table,
        db                  TYPE string_table,
        private             TYPE abap_bool,
        deprecated          TYPE abap_bool,
        BEGIN OF dist,
          file_count    TYPE i,
          integrity     TYPE string,
          shasum        TYPE string,
          signatures    TYPE STANDARD TABLE OF zif_abappm_types=>ty_signature WITH KEY keyid,
          tarball       TYPE string,
          unpacked_size TYPE i,
        END OF dist,
        readme              TYPE string,
      END OF ty_package_json_partial.

    DATA:
      json_partial TYPE ty_package_json_partial,
      dependency   TYPE zif_abappm_types=>ty_dependency.

    TRY.
        DATA(ajson) = zcl_abappm_ajson=>parse( json )->to_abap_corresponding_only( ).

        ajson->to_abap( IMPORTING ev_container = json_partial ).

        DATA(manifest) = CORRESPONDING zif_abappm_types=>ty_manifest( json_partial ).

        " Transpose dependencies
        LOOP AT ajson->members( '/dependencies' ) INTO dependency-name.
          dependency-range = ajson->get( '/dependencies/' && dependency-name ).
          INSERT dependency INTO TABLE manifest-dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/devDependencies' ) INTO dependency-name.
          dependency-range = ajson->get( '/devDependencies/' && dependency-name ).
          INSERT dependency INTO TABLE manifest-dev_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/optionalDependencies' ) INTO dependency-name.
          dependency-range = ajson->get( '/optionalDependencies/' && dependency-name ).
          INSERT dependency INTO TABLE manifest-optional_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/peerDependencies' ) INTO dependency-name.
          dependency-range = ajson->get( '/peerDependencies/' && dependency-name ).
          INSERT dependency INTO TABLE manifest-peer_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/bundleDependencies' ) INTO dependency-name.
          dependency-range = ajson->get( '/bundleDependencies/' && dependency-name ).
          " store just the range, which is the name of the bundle dependency
          INSERT dependency-range INTO TABLE manifest-bundle_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/engines' ) INTO dependency-name.
          dependency-range = ajson->get( '/engines/' && dependency-name ).
          INSERT dependency INTO TABLE manifest-engines.
        ENDLOOP.

        manifest-dist-file_count    = ajson->get( '/dist/fileCount' ).
        manifest-dist-unpacked_size = ajson->get( '/dist/unpackageSize' ).
        manifest-__id               = ajson->get( '_id' ).
        manifest-__abap_version     = ajson->get( '_abapVersion' ).
        manifest-__apm_version      = ajson->get( '_apmVersion' ).

        check_manifest( manifest ).

        result = sort_manifest( manifest ).

      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_json_to_manifest_abbr.

    DATA(manifest) = convert_json_to_manifest( json ).
    result = CORRESPONDING #( manifest ).

  ENDMETHOD.


  METHOD convert_manifest_to_json.

    TRY.
        DATA(ajson) = zcl_abappm_ajson=>new(
          )->keep_item_order(
          )->set(
            iv_path = '/'
            iv_val  = manifest
          )->map( zcl_abappm_ajson_mapping=>create_to_camel_case( ) ).

        " Transpose dependencies
        ajson->setx( '/dependencies:{ }' ).
        LOOP AT manifest-dependencies INTO DATA(dependency).
          ajson->set(
            iv_path = '/dependencies/' && dependency-name
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( '/devDependencies:{ }' ).
        LOOP AT manifest-dev_dependencies INTO dependency.
          ajson->set(
            iv_path = '/devDependencies/' && dependency-name
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( '/optionalDependencies:{ }' ).
        LOOP AT manifest-optional_dependencies INTO dependency.
          ajson->set(
            iv_path = '/optionalDependencies/' && dependency-name
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( 'peerDependencies:{ }' ).
        LOOP AT manifest-peer_dependencies INTO dependency.
          ajson->set(
            iv_path = '/peerDependencies/' && dependency-name
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( '/engines:{ }' ).
        LOOP AT manifest-engines INTO dependency.
          ajson->set(
            iv_path = '/engines/' && dependency-name
            iv_val  = dependency-range ).
        ENDLOOP.

        IF is_complete = abap_false.
          ajson = ajson->filter( lcl_ajson_filters=>create_empty_filter( ) ).
          IF manifest-private = abap_false.
            ajson = ajson->filter( zcl_abappm_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/private' ) ).
          ENDIF.
        ENDIF.

        IF is_package_json = abap_true.
          " Remove the manifest fields that are not in package.json
          ajson = ajson->filter( zcl_abappm_ajson_filter_lib=>create_path_filter(
            iv_skip_paths = '/dist,/deprecated,/_id,/_abapVersion,/_apmVersion' ) ).
        ENDIF.

        result = ajson->stringify( 2 ).
      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>)
      WITH TABLE KEY package = package.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      result = NEW zcl_abappm_package_json(
        package = package
        name    = name
        version = version
        private = private ).

      DATA(instance) = VALUE ty_instance(
        package  = package
        instance = result ).
      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_from_key.

    SPLIT key AT ':' INTO DATA(prefix) result DATA(suffix) ##NEEDED.
    result = to_upper( result ).

  ENDMETHOD.


  METHOD get_package_key.

    result = |{ zif_abappm_persist_apm=>c_key_type-package }:{ package }:|
          && |{ zif_abappm_persist_apm=>c_key_extra-package_json }|.

  ENDMETHOD.


  METHOD injector.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>) WITH TABLE KEY package = package.
    IF sy-subrc = 0.
      <instance>-instance = mock.
    ELSE.
      DATA(instance) = VALUE ty_instance(
        package  = package
        instance = mock ).
      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD list.

    DATA(list) = db_persist->list( zif_abappm_persist_apm=>c_key_type-package && |:{ filter }%:|
      && zif_abappm_persist_apm=>c_key_extra-package_json ).

    LOOP AT list ASSIGNING FIELD-SYMBOL(<list>).
      DATA(result_item) = VALUE zif_abappm_package_json=>ty_package(
        key            = <list>-keys
        package        = get_package_from_key( <list>-keys )
        changed_by     = <list>-user
        changed_at_raw = <list>-timestamp
        changed_at     = zcl_abapgit_gui_chunk_lib=>render_timestamp( <list>-timestamp ) ).

      IF instanciate = abap_true.
        TRY.
            result_item-instance    = factory( result_item-package )->load( ).
            DATA(package_json)      = result_item-instance->get( ).
            result_item-name        = package_json-name.
            result_item-version     = package_json-version.
            result_item-description = package_json-description.
            result_item-type        = package_json-type.
            result_item-private     = package_json-private.
          CATCH zcx_abappm_error ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      INSERT result_item INTO TABLE result.
    ENDLOOP.

    " Check package hierarchy to determine which packages are bundled
    LOOP AT result ASSIGNING FIELD-SYMBOL(<result_item>).
      TRY.
          DATA(super_packages) = zcl_abapgit_factory=>get_sap_package( <result_item>-package )->list_superpackages( ).

          LOOP AT super_packages ASSIGNING FIELD-SYMBOL(<super_package>) WHERE table_line <> <result_item>-package.
            IF line_exists( result[ KEY package COMPONENTS package = <super_package> ] ).
              <result_item>-bundle = abap_true.
              <result_item>-parent = <super_package>.
              EXIT.
            ENDIF.
          ENDLOOP.

        CATCH zcx_abapgit_exception ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    CASE is_bundle.
      WHEN abap_true.
        DELETE result WHERE bundle = abap_false.
      WHEN abap_false.
        DELETE result WHERE bundle = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD sort_manifest.

    result = manifest.

    " Keeping things in order avoid unnecessary diffs
    SORT:
      result-dependencies BY name,
      result-dev_dependencies BY name,
      result-optional_dependencies BY name,
      result-peer_dependencies BY name,
      result-bundle_dependencies,
      result-engines BY name,
      result-contributors BY name,
      result-maintainers BY name,
      result-keywords,
      result-man,
      result-os,
      result-cpu,
      result-db.

  ENDMETHOD.


  METHOD zif_abappm_package_json~delete.

    db_persist->delete( key ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~exists.

    TRY.
        db_persist->load( key ).
        result = abap_true.
      CATCH zcx_abappm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_package_json~get.

    result = CORRESPONDING #( manifest ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~get_json.

    result = convert_manifest_to_json(
      manifest        = manifest
      is_package_json = abap_true
      is_complete     = is_complete ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid.

    TRY.
        result = xsdbool( zcl_abappm_package_json_valid=>check( manifest ) IS INITIAL ).
      CATCH zcx_abappm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_package_json~load.

    zif_abappm_package_json~set_json( db_persist->load( key )-value ).
    result = me.

  ENDMETHOD.


  METHOD zif_abappm_package_json~save.

    check_manifest( manifest ).
    db_persist->save(
      key   = key
      value = zif_abappm_package_json~get_json( ) ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~set.

    manifest = CORRESPONDING #( package_json ).
    check_manifest( manifest ).
    manifest = sort_manifest( manifest ).
    result   = me.

  ENDMETHOD.


  METHOD zif_abappm_package_json~set_json.

    manifest = convert_json_to_manifest( json ).
    result   = me.

  ENDMETHOD.
ENDCLASS.
