CLASS ZCL_ABAPPM_PACKAGE_JSON DEFINITION
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

    INTERFACES ZIF_ABAPPM_PACKAGE_JSON.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_package   TYPE devclass
        !iv_name      TYPE string OPTIONAL
        !iv_version   TYPE string OPTIONAL
        !iv_private   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS injector
      IMPORTING
        !iv_package TYPE devclass
        !ii_mock    TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON.

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass
        !iv_name    TYPE string OPTIONAL
        !iv_version TYPE string OPTIONAL
        !iv_private TYPE abap_bool DEFAULT abap_false
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS list
      IMPORTING
        !iv_filter      TYPE string OPTIONAL
        !iv_instanciate TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE ZIF_ABAPPM_PACKAGE_JSON=>TY_PACKAGES.

    CLASS-METHODS get_package_key
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !iv_key       TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY
      RETURNING
        VALUE(result) TYPE devclass.

    CLASS-METHODS convert_json_to_manifest
      IMPORTING
        !iv_json      TYPE string
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS convert_manifest_to_json
      IMPORTING
        !is_manifest     TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST
        !iv_package_json TYPE abap_bool DEFAULT abap_false
        !iv_complete     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE string
      RAISING
        ZCX_ABAPPM_ERROR.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gi_persist   TYPE REF TO ZIF_ABAPPM_PERSIST_APM,
      gt_instances TYPE ty_instances.

    DATA:
      mv_key      TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY,
      mv_package  TYPE devclass,
      ms_manifest TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST.

    CLASS-METHODS check_manifest
      IMPORTING
        !is_manifest TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS sort_manifest
      IMPORTING
        !is_manifest  TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST.

ENDCLASS.



CLASS ZCL_ABAPPM_PACKAGE_JSON IMPLEMENTATION.


  METHOD check_manifest.

    DATA lt_issues TYPE string_table.

    lt_issues = ZCL_ABAPPM_PACKAGE_JSON_VALID=>CHECK( is_manifest ).

    IF lt_issues IS NOT INITIAL.
      ZCX_ABAPPM_ERROR=>RAISE( |Invalid package json:\n{ concat_lines_of( table = lt_issues sep = |\n| ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
    gi_persist = ZCL_ABAPPM_PERSIST_APM=>GET_INSTANCE( ).
  ENDMETHOD.


  METHOD constructor.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_SAP_PACKAGE( iv_package ) = abap_false.
      ZCX_ABAPPM_ERROR=>RAISE( |Invalid package: { iv_package }| ).
    ENDIF.

    mv_package          = iv_package.
    ms_manifest-name    = iv_name.
    ms_manifest-version = iv_version.
    ms_manifest-private = iv_private.

    mv_key = get_package_key( mv_package ).

    TRY.
        ZIF_ABAPPM_PACKAGE_JSON~LOAD( ).
      CATCH ZCX_ABAPPM_ERROR ##NO_HANDLER.
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
          url   TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
          email TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_EMAIL,
        END OF bugs,
        license             TYPE string,
        author              TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON,
        contributors        TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON WITH KEY name,
        maintainers         TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON WITH KEY name,
        main                TYPE string,
        man                 TYPE string_table,
        type                TYPE string,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
          directory TYPE string,
        END OF repository,
        BEGIN OF funding,
          type TYPE string,
          url  TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
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
          signatures    TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_SIGNATURE WITH DEFAULT KEY,
          tarball       TYPE string,
          unpacked_size TYPE i,
        END OF dist,
        readme              TYPE string,
      END OF ty_package_json_partial.

    DATA:
      li_json         TYPE REF TO ZIF_ABAPPM_AJSON,
      ls_json_partial TYPE ty_package_json_partial,
      ls_dependency   TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_DEPENDENCY,
      ls_json         TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST,
      ls_package_json TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON,
      lt_issues       TYPE string_table,
      lx_error        TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        li_json = ZCL_ABAPPM_AJSON=>PARSE( iv_json ).
        li_json->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = ls_json_partial ).

        MOVE-CORRESPONDING ls_json_partial TO ls_json.

        " Transpose dependencies
        LOOP AT li_json->members( '/dependencies' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/dependencies/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-dependencies.
        ENDLOOP.
        LOOP AT li_json->members( '/devDependencies' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/devDependencies/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-dev_dependencies.
        ENDLOOP.
        LOOP AT li_json->members( '/optDependencies' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/optDependencies/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-optional_dependencies.
        ENDLOOP.
        LOOP AT li_json->members( '/engines' ) INTO ls_dependency-name.
          ls_dependency-range = li_json->get( '/engines/' && ls_dependency-name ).
          INSERT ls_dependency INTO TABLE ls_json-engines.
        ENDLOOP.

        check_manifest( ls_json ).

        result = sort_manifest( ls_json ).

      CATCH ZCX_ABAPPM_AJSON_ERROR INTO lx_error.
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_manifest_to_json.

    DATA:
      li_json       TYPE REF TO ZIF_ABAPPM_AJSON,
      ls_dependency TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_DEPENDENCY,
      lx_error      TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        li_json = ZCL_ABAPPM_AJSON=>NEW( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = is_manifest ).

        li_json = li_json->map( ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( ) ).

        " Transpose dependencies
        li_json->setx( '/dependencies:{ }' ).
        LOOP AT is_manifest-dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/dependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.

        li_json->setx( '/devDependencies:{ }' ).
        LOOP AT is_manifest-dev_dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/devDependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.

        li_json->setx( '/optionalDependencies:{ }' ).
        LOOP AT is_manifest-optional_dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/optionalDependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.

        li_json->setx( '/engines:{ }' ).
        LOOP AT is_manifest-engines INTO ls_dependency.
          li_json->set(
            iv_path = '/engines/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.

        IF iv_complete = abap_false.
          li_json = li_json->filter( lcl_ajson_filters=>create_empty_filter( ) ).
          IF is_manifest-private = abap_false.
            li_json = li_json->filter( ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER( iv_skip_paths = '/private' ) ).
          ENDIF.
        ENDIF.

        IF iv_package_json = abap_true.
          " Remove the manifest fields that are not in package.json
          li_json = li_json->filter( ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER(
            iv_skip_paths = '/dist,/deprecated,/_id,/_abapVersion,/_apmVersion' ) ).
        ENDIF.

        result = li_json->stringify( 2 ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO lx_error.
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE ZCL_ABAPPM_PACKAGE_JSON
        EXPORTING
          iv_package = iv_package
          iv_name    = iv_name
          iv_version = iv_version
          iv_private = iv_private.

      ls_instance-package  = iv_package.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_from_key.

    DATA:
      lv_prefix TYPE string,
      lv_suffix TYPE string.

    SPLIT iv_key AT ':' INTO lv_prefix result lv_suffix.
    result = to_upper( result ).

  ENDMETHOD.


  METHOD get_package_key.
    result = |{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-PACKAGE }:{ iv_package }:{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_EXTRA-PACKAGE_JSON }|.
  ENDMETHOD.


  METHOD injector.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      <ls_instance>-instance = ii_mock.
    ELSE.
      ls_instance-package  = iv_package.
      ls_instance-instance = ii_mock.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD list.

    DATA:
      lt_list   TYPE ZIF_ABAPPM_PERSIST_APM=>TY_LIST,
      ls_result LIKE LINE OF result.

    FIELD-SYMBOLS <ls_list> LIKE LINE OF lt_list.

    lt_list = gi_persist->list( ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-PACKAGE && |:{ iv_filter }%:|
      && ZIF_ABAPPM_PERSIST_APM=>C_KEY_EXTRA-PACKAGE_JSON ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      CLEAR ls_result.
      ls_result-key            = <ls_list>-keys.
      ls_result-package        = get_package_from_key( <ls_list>-keys ).
      ls_result-changed_by     = <ls_list>-user.
      ls_result-changed_at_raw = <ls_list>-timestamp.
      ls_result-changed_at     = zcl_abapgit_gui_chunk_lib=>render_timestamp( <ls_list>-timestamp ).

      IF iv_instanciate = abap_true.
        TRY.
            ls_result-instance    = factory( ls_result-package )->load( ).
            ls_result-name        = ls_result-instance->get( )-name.
            ls_result-version     = ls_result-instance->get( )-version.
            ls_result-description = ls_result-instance->get( )-description.
            ls_result-type        = ls_result-instance->get( )-type.
            ls_result-private     = ls_result-instance->get( )-private.
          CATCH ZCX_ABAPPM_ERROR ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      INSERT ls_result INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD sort_manifest.
    result = is_manifest.
    SORT result-dependencies BY name.
    SORT result-dev_dependencies BY name.
    SORT result-optional_dependencies BY name.
    SORT result-engines BY name.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~DELETE.
    gi_persist->delete( mv_key ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~EXISTS.
    TRY.
        gi_persist->load( mv_key ).
        result = abap_true.
      CATCH ZCX_ABAPPM_ERROR.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~GET.
    MOVE-CORRESPONDING ms_manifest TO result.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~GET_JSON.
    result = convert_manifest_to_json(
      is_manifest     = ms_manifest
      iv_package_json = abap_true
      iv_complete     = iv_complete ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~IS_VALID.
    TRY.
        result = boolc( ZCL_ABAPPM_PACKAGE_JSON_VALID=>CHECK( ms_manifest ) IS INITIAL ).
      CATCH ZCX_ABAPPM_ERROR.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~LOAD.
    ZIF_ABAPPM_PACKAGE_JSON~SET_JSON( gi_persist->load( mv_key )-value ).
    result = me.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~SAVE.
    check_manifest( ms_manifest ).
    gi_persist->save(
      iv_key   = mv_key
      iv_value = ZIF_ABAPPM_PACKAGE_JSON~GET_JSON( ) ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~SET.
    MOVE-CORRESPONDING is_json TO ms_manifest.
    check_manifest( ms_manifest ).
    ms_manifest = sort_manifest( ms_manifest ).
    result = me.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~SET_JSON.
    ms_manifest = convert_json_to_manifest( iv_json ).
    result = me.
  ENDMETHOD.
ENDCLASS.
