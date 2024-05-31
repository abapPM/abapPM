CLASS ZCL_ABAPPM_PACKAGE_JSON DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Package JSON
*
* Copyright (c) Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES ZIF_ABAPPM_PACKAGE_JSON.

    CLASS-METHODS factory
      IMPORTING
        !iv_package   TYPE devclass
        !iv_name      TYPE string OPTIONAL
        !iv_version   TYPE string OPTIONAL
        !iv_private   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

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
        ZCX_ABAPPM_PACKAGE_JSON.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO ZIF_ABAPPM_PACKAGE_JSON,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gt_instances TYPE ty_instances.

    DATA:
      mv_package      TYPE devclass,
      ms_package_json TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON,
      mo_persist      TYPE REF TO ZCL_ABAPPM_PACKAGE_JSON_DB.

    CLASS-METHODS sort_dependencies
      IMPORTING
        !is_package_json TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON
      RETURNING
        VALUE(result)    TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON.

ENDCLASS.



CLASS ZCL_ABAPPM_PACKAGE_JSON IMPLEMENTATION.


  METHOD constructor.

    DATA:
      ls_json  TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON,
      li_json  TYPE REF TO ZIF_ABAPPM_AJSON,
      lx_error TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_SAP_PACKAGE( iv_package ) = abap_false.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE( |Invalid package: { iv_package }| ).
    ENDIF.

    mv_package              = iv_package.
    ms_package_json-name    = iv_name.
    ms_package_json-version = iv_version.
    ms_package_json-private = iv_private.

    CREATE OBJECT mo_persist
      EXPORTING
        iv_package = mv_package.

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


  METHOD sort_dependencies.
    result = is_package_json.
    SORT result-dependencies BY name.
    SORT result-dev_dependencies BY name.
    SORT result-optional_dependencies BY name.
    SORT result-engines BY name.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~DELETE.
    mo_persist->delete( ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~GET.
    result = ms_package_json.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~GET_JSON.

    DATA:
      li_json       TYPE REF TO ZIF_ABAPPM_AJSON,
      ls_dependency TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_DEPENDENCY,
      lx_error      TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        li_json = ZCL_ABAPPM_AJSON=>NEW( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = ms_package_json ).

        li_json = li_json->map( ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( ) ).

        " Transpose dependencies
        li_json->setx( '/dependencies:{ }' ).
        LOOP AT ms_package_json-dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/dependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.
        li_json->setx( '/devDependencies:{ }' ).
        LOOP AT ms_package_json-dev_dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/devDependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.
        li_json->setx( '/optionalDependencies:{ }' ).
        LOOP AT ms_package_json-optional_dependencies INTO ls_dependency.
          li_json->set(
            iv_path = '/optionalDependencies/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.
        li_json->setx( '/engines:{ }' ).
        LOOP AT ms_package_json-engines INTO ls_dependency.
          li_json->set(
            iv_path = '/engines/' && ls_dependency-name
            iv_val  = ls_dependency-range ).
        ENDLOOP.

        IF iv_complete = abap_false.
          li_json = li_json->filter( lcl_ajson_filters=>create_empty_filter( ) ).
          IF ms_package_json-private = abap_false.
            li_json = li_json->filter( ZCL_ABAPPM_AJSON_FILTER_LIB=>CREATE_PATH_FILTER( iv_skip_paths = '/private' ) ).
          ENDIF.
        ENDIF.

        result = li_json->stringify( 2 ).
      CATCH ZCX_ABAPPM_AJSON_ERROR.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~IS_VALID.

    DATA lt_errors TYPE string_table.

    TRY.
        lt_errors = ZCL_ABAPPM_PACKAGE_JSON_VALID=>CHECK( ms_package_json ).
        result = boolc( lt_errors IS INITIAL ).
      CATCH ZCX_ABAPPM_PACKAGE_JSON.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~LOAD.
    ZIF_ABAPPM_PACKAGE_JSON~SET_JSON( mo_persist->load( )-data ).
    result = me.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~SAVE.
    ZCL_ABAPPM_PACKAGE_JSON_VALID=>CHECK( ms_package_json ).
    mo_persist->save(  ZIF_ABAPPM_PACKAGE_JSON~GET_JSON( ) ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~SET.
    ZCL_ABAPPM_PACKAGE_JSON_VALID=>CHECK( is_json ).
    ms_package_json = sort_dependencies( is_json ).
    result = me.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACKAGE_JSON~SET_JSON.

    TYPES:
      " Copy of schema but without dependencies (instead of array)
      BEGIN OF ty_package_json_wo_deps,
        name                 TYPE string,
        version              TYPE string,
        description          TYPE string,
        keywords             TYPE string_table,
        homepage             TYPE string,
        BEGIN OF bugs,
          url   TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
          email TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_EMAIL,
        END OF bugs,
        license              TYPE string,
        author               TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON,
        contributors         TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON WITH KEY name,
        maintainers          TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PERSON WITH KEY name,
        main                 TYPE string,
        man                  TYPE string_table,
        type                 TYPE string,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
          directory TYPE string,
        END OF repository,
        BEGIN OF funding,
          type TYPE string,
          url  TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_URI,
        END OF funding,
        bundled_dependencies TYPE string_table,
        os                   TYPE string_table,
        cpu                  TYPE string_table,
        db                   TYPE string_table,
        private              TYPE abap_bool,
        BEGIN OF dist,
          file_count    TYPE i,
          integrity     TYPE string,
          shasum        TYPE string,
          signatures    TYPE STANDARD TABLE OF ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_SIGNATURE WITH DEFAULT KEY,
          tarball       TYPE string,
          unpacked_size TYPE i,
        END OF dist,
        readme               TYPE string,
      END OF ty_package_json_wo_deps.

    DATA:
      li_json         TYPE REF TO ZIF_ABAPPM_AJSON,
      ls_json_wo_deps TYPE ty_package_json_wo_deps,
      ls_dependency   TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_DEPENDENCY,
      ls_json         TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_PACKAGE_JSON,
      lx_error        TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        li_json = ZCL_ABAPPM_AJSON=>PARSE( iv_json ).
        li_json->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = ls_json_wo_deps ).

        MOVE-CORRESPONDING ls_json_wo_deps TO ls_json.

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

        ZCL_ABAPPM_PACKAGE_JSON_VALID=>CHECK( ls_json ).

        ms_package_json = sort_dependencies( ls_json ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO lx_error.
        ZCX_ABAPPM_PACKAGE_JSON=>RAISE_WITH_TEXT( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.
ENDCLASS.
