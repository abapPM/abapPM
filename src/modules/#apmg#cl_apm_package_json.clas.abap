CLASS /apmg/cl_apm_package_json DEFINITION
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

    INTERFACES /apmg/if_apm_package_json.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !package      TYPE devclass
        !name         TYPE string OPTIONAL
        !version      TYPE string OPTIONAL
        !private      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_package_json
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS injector
      IMPORTING
        !package TYPE devclass
        !mock    TYPE REF TO /apmg/if_apm_package_json.

    METHODS constructor
      IMPORTING
        !package TYPE devclass
        !name    TYPE string OPTIONAL
        !version TYPE string OPTIONAL
        !private TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS list
      IMPORTING
        !filter       TYPE string OPTIONAL
        !instanciate  TYPE abap_bool DEFAULT abap_false
        !is_bundle    TYPE abap_bool DEFAULT abap_undefined
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_package_json=>ty_packages.

    CLASS-METHODS get_package_key
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_persist_apm=>ty_key.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !key          TYPE /apmg/if_apm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE devclass.

    CLASS-METHODS convert_json_to_manifest
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS convert_json_to_manifest_abbr
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_manifest_abbreviated
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS convert_manifest_to_json
      IMPORTING
        !manifest        TYPE /apmg/if_apm_types=>ty_manifest
        !is_package_json TYPE abap_bool DEFAULT abap_false
        !is_complete     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE string
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO /apmg/if_apm_package_json,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    TYPES ty_package_list TYPE STANDARD TABLE OF devclass WITH KEY table_line.

    CLASS-DATA:
      db_persist TYPE REF TO /apmg/if_apm_persist_apm,
      instances  TYPE ty_instances.

    DATA:
      key      TYPE /apmg/if_apm_persist_apm=>ty_key,
      package  TYPE devclass,
      manifest TYPE /apmg/if_apm_types=>ty_manifest.

    CLASS-METHODS check_manifest
      IMPORTING
        !manifest TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS sort_manifest
      IMPORTING
        !manifest     TYPE /apmg/if_apm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_manifest.

    CLASS-METHODS replace_slash
      IMPORTING
        !value        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_super_packages
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE ty_package_list.

ENDCLASS.



CLASS /apmg/cl_apm_package_json IMPLEMENTATION.


  METHOD /apmg/if_apm_package_json~delete.

    db_persist->delete( key ).

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~exists.

    TRY.
        db_persist->load( key ).
        result = abap_true.
      CATCH /apmg/cx_apm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~get.

    result = CORRESPONDING #( manifest ).

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~get_json.

    result = convert_manifest_to_json(
      manifest        = manifest
      is_package_json = abap_true
      is_complete     = is_complete ).

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~is_valid.

    TRY.
        result = xsdbool( /apmg/cl_apm_package_json_vali=>check( manifest ) IS INITIAL ).
      CATCH /apmg/cx_apm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~load.

    /apmg/if_apm_package_json~set_json( db_persist->load( key )-value ).
    result = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~save.

    check_manifest( manifest ).
    db_persist->save(
      key   = key
      value = /apmg/if_apm_package_json~get_json( ) ).

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~set.

    manifest = CORRESPONDING #( package_json ).
    check_manifest( manifest ).
    manifest = sort_manifest( manifest ).
    result   = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_package_json~set_json.

    manifest = convert_json_to_manifest( json ).
    result   = me.

  ENDMETHOD.


  METHOD check_manifest.

    DATA(issues) = /apmg/cl_apm_package_json_vali=>check( manifest ).

    IF issues IS NOT INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Invalid package json:\n{ concat_lines_of( table = issues sep = |\n| ) }|.
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.

    db_persist = /apmg/cl_apm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    IF /apmg/cl_apm_package_json_vali=>is_valid_sap_package( package ) = abap_false.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = |Invalid package: { package }|.
    ENDIF.

    me->package      = package.
    manifest-name    = name.
    manifest-version = version.
    manifest-private = private.

    key = get_package_key( package ).

    TRY.
        /apmg/if_apm_package_json~load( ).
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD convert_json_to_manifest.

    " TODO: AJSON does not allow for mapping of ABAP to JSON objects like { "user1", "user2", ... }
    " A table would map to an array [ "user1", "user2", ... ]

    TYPES:
      " Copy of schema but without dependencies (instead of array)
      BEGIN OF ty_manifest_partial,
        name          TYPE string,
        version       TYPE string,
        description   TYPE string,
        keywords      TYPE string_table,
        homepage      TYPE string,
        icon          TYPE string,
        BEGIN OF bugs,
          url   TYPE /apmg/if_apm_types=>ty_uri,
          email TYPE /apmg/if_apm_types=>ty_email,
        END OF bugs,
        license       TYPE string,
        author        TYPE /apmg/if_apm_types=>ty_person,
        contributors  TYPE STANDARD TABLE OF /apmg/if_apm_types=>ty_person WITH KEY name,
        maintainers   TYPE STANDARD TABLE OF /apmg/if_apm_types=>ty_person WITH KEY name,
        main          TYPE string,
        man           TYPE string_table,
        type          TYPE string,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE /apmg/if_apm_types=>ty_uri,
          directory TYPE string,
        END OF repository,
        BEGIN OF funding,
          type TYPE string,
          url  TYPE /apmg/if_apm_types=>ty_uri,
        END OF funding,
        os            TYPE string_table,
        cpu           TYPE string_table,
        db            TYPE string_table,
        private       TYPE abap_bool,
        deprecated    TYPE abap_bool,
        dist          TYPE /apmg/if_apm_types=>ty_dist,
        readme        TYPE string,
        _id           TYPE string,
        _abap_version TYPE string,
        _apm_version  TYPE string,
      END OF ty_manifest_partial.

    DATA:
      manifest_partial TYPE ty_manifest_partial,
      dependency       TYPE /apmg/if_apm_types=>ty_dependency.

    TRY.
        DATA(ajson) = /apmg/cl_apm_ajson=>parse( json
          )->to_abap_corresponding_only(
          )->map( /apmg/cl_apm_ajson_extensions=>from_camel_case_underscore( ) ).

        ajson->to_abap( IMPORTING ev_container = manifest_partial ).

        DATA(manifest) = CORRESPONDING /apmg/if_apm_types=>ty_manifest( manifest_partial ).

        " Transpose dependencies
        LOOP AT ajson->members( '/dependencies' ) INTO dependency-key.
          dependency-range = ajson->get( '/dependencies/' && replace_slash( dependency-key ) ).
          INSERT dependency INTO TABLE manifest-dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/dev_Dependencies' ) INTO dependency-key.
          dependency-range = ajson->get( '/dev_Dependencies/' && replace_slash( dependency-key ) ).
          INSERT dependency INTO TABLE manifest-dev_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/optional_Dependencies' ) INTO dependency-key.
          dependency-range = ajson->get( '/optional_Dependencies/' && replace_slash( dependency-key ) ).
          INSERT dependency INTO TABLE manifest-optional_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/peer_Dependencies' ) INTO dependency-key.
          dependency-range = ajson->get( '/peer_Dependencies/' && replace_slash( dependency-key ) ).
          INSERT dependency INTO TABLE manifest-peer_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/bundle_Dependencies' ) INTO dependency-key.
          dependency-range = ajson->get( '/bundle_Dependencies/' && replace_slash( dependency-key ) ).
          " store just the range, which is the name of the bundle dependency
          INSERT dependency-range INTO TABLE manifest-bundle_dependencies.
        ENDLOOP.
        LOOP AT ajson->members( '/engines' ) INTO dependency-key.
          dependency-range = ajson->get( '/engines/' && replace_slash( dependency-key ) ).
          INSERT dependency INTO TABLE manifest-engines.
        ENDLOOP.

        check_manifest( manifest ).

        result = sort_manifest( manifest ).

      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD convert_json_to_manifest_abbr.

    DATA(full_manifest) = convert_json_to_manifest( json ).
    result = CORRESPONDING #( full_manifest ).

  ENDMETHOD.


  METHOD convert_manifest_to_json.

    DATA skip_paths TYPE string_table.

    TRY.
        DATA(ajson) = /apmg/cl_apm_ajson=>new(
          )->keep_item_order(
          )->set(
            iv_path = '/'
            iv_val  = manifest
          )->map( /apmg/cl_apm_ajson_extensions=>to_camel_case_underscore( ) ).

        " Transpose dependencies
        ajson->setx( '/dependencies:{ }' ).
        LOOP AT manifest-dependencies INTO DATA(dependency).
          ajson->set(
            iv_path = '/dependencies/' && dependency-key
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( '/devDependencies:{ }' ).
        LOOP AT manifest-dev_dependencies INTO dependency.
          ajson->set(
            iv_path = '/devDependencies/' && dependency-key
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( '/optionalDependencies:{ }' ).
        LOOP AT manifest-optional_dependencies INTO dependency.
          ajson->set(
            iv_path = '/optionalDependencies/' && dependency-key
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( 'peerDependencies:{ }' ).
        LOOP AT manifest-peer_dependencies INTO dependency.
          ajson->set(
            iv_path = '/peerDependencies/' && dependency-key
            iv_val  = dependency-range ).
        ENDLOOP.

        ajson->setx( '/engines:{ }' ).
        LOOP AT manifest-engines INTO dependency.
          ajson->set(
            iv_path = '/engines/' && dependency-key
            iv_val  = dependency-range ).
        ENDLOOP.

        IF is_complete = abap_false.
          ajson = ajson->filter( /apmg/cl_apm_ajson_extensions=>filter_empty_zero_null( ) ).
          IF manifest-private = abap_false.
            INSERT `/private` INTO TABLE skip_paths.
          ENDIF.
          IF manifest-deprecated = abap_false.
            INSERT `/deprecated` INTO TABLE skip_paths.
          ENDIF.
        ENDIF.

        IF is_package_json = abap_true.
          " Remove the manifest fields that are not in package.json
          INSERT `/deprecated` INTO TABLE skip_paths.
          INSERT `/dist` INTO TABLE skip_paths.
          INSERT `/_id` INTO TABLE skip_paths.
          INSERT `/_abapVersion` INTO TABLE skip_paths.
          INSERT `/_apmVersion` INTO TABLE skip_paths.
        ENDIF.

        IF skip_paths IS NOT INITIAL.
          DATA(skip_path) = concat_lines_of(
            table = skip_paths
            sep   = ',' ).
          ajson = ajson->filter( /apmg/cl_apm_ajson_filter_lib=>create_path_filter( iv_skip_paths = skip_path ) ).
        ENDIF.

        result = ajson->stringify( 2 ).
      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>)
      WITH TABLE KEY package = package.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      result = NEW /apmg/cl_apm_package_json(
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

    result = |{ /apmg/if_apm_persist_apm=>c_key_type-package }:{ package }:|
          && |{ /apmg/if_apm_persist_apm=>c_key_extra-package_json }|.

  ENDMETHOD.


  METHOD get_super_packages.

    DATA(devclass) = package.
    DO.
      INSERT devclass INTO TABLE result.
      SELECT SINGLE parentcl FROM tdevc INTO @DATA(parent) WHERE devclass = @devclass.
      IF sy-subrc <> 0 OR parent IS INITIAL.
        EXIT.
      ENDIF.
      devclass = parent.
    ENDDO.

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

    DATA(list) = db_persist->list( /apmg/if_apm_persist_apm=>c_key_type-package && |:{ filter }%:|
      && /apmg/if_apm_persist_apm=>c_key_extra-package_json ).

    LOOP AT list ASSIGNING FIELD-SYMBOL(<list>).
      CONVERT TIME STAMP <list>-timestamp
        TIME ZONE 'UTC'
        INTO DATE DATA(date)
        TIME DATA(time).

      DATA(changed_at) = |{ date DATE = ISO } { time TIME = ISO }|.

      DATA(result_item) = VALUE /apmg/if_apm_package_json=>ty_package(
        key            = <list>-keys
        package        = get_package_from_key( <list>-keys )
        changed_by     = <list>-user
        changed_at_raw = <list>-timestamp
        changed_at     = changed_at ).

      IF instanciate = abap_true.
        TRY.
            result_item-instance    = factory( result_item-package )->load( ).
            DATA(package_json)      = result_item-instance->get( ).
            result_item-name        = package_json-name.
            result_item-version     = package_json-version.
            result_item-description = package_json-description.
            result_item-type        = package_json-type.
            result_item-private     = package_json-private.
          CATCH /apmg/cx_apm_error ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      INSERT result_item INTO TABLE result.
    ENDLOOP.

    " Check package hierarchy to determine which packages are bundled
    LOOP AT result ASSIGNING FIELD-SYMBOL(<result_item>).
      DATA(super_packages) = get_super_packages( <result_item>-package ).

      LOOP AT super_packages ASSIGNING FIELD-SYMBOL(<super_package>) WHERE table_line <> <result_item>-package.
        IF line_exists( result[ KEY package COMPONENTS package = <super_package> ] ).
          <result_item>-bundle = abap_true.
          <result_item>-parent = <super_package>.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    CASE is_bundle.
      WHEN abap_true.
        DELETE result WHERE bundle = abap_false.
      WHEN abap_false.
        DELETE result WHERE bundle = abap_true.
    ENDCASE.

  ENDMETHOD.


  METHOD replace_slash.

    result = replace(
      val  = value
      sub  = '/'
      with = cl_abap_char_utilities=>horizontal_tab
      occ  = 0 ).

  ENDMETHOD.


  METHOD sort_manifest.

    result = manifest.

    " Keeping things in order avoid unnecessary diffs
    SORT:
      result-dependencies BY key,
      result-dev_dependencies BY key,
      result-optional_dependencies BY key,
      result-peer_dependencies BY key,
      result-bundle_dependencies,
      result-engines BY key,
      result-contributors BY name,
      result-maintainers BY name,
      result-keywords,
      result-man,
      result-os,
      result-cpu,
      result-db.

  ENDMETHOD.
ENDCLASS.
