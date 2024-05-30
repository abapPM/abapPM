CLASS zcl_abappm_package_json DEFINITION
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

    INTERFACES zif_abappm_package_json.

    CLASS-METHODS factory
      IMPORTING
        !iv_package   TYPE devclass
        !iv_name      TYPE string OPTIONAL
        !iv_version   TYPE string OPTIONAL
        !iv_private   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_package_json
      RAISING
        zcx_abappm_package_json.

    CLASS-METHODS injector
      IMPORTING
        !iv_package TYPE devclass
        !ii_mock    TYPE REF TO zif_abappm_package_json.

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass
        !iv_name    TYPE string OPTIONAL
        !iv_version TYPE string OPTIONAL
        !iv_private TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_package_json.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO zif_abappm_package_json,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gt_instances TYPE ty_instances.

    DATA:
      mv_package      TYPE devclass,
      ms_package_json TYPE zif_abappm_package_json=>ty_package_json,
      mo_persist      TYPE REF TO zcl_abappm_package_json_db.

    CLASS-METHODS sort_dependencies
      IMPORTING
        !is_package_json TYPE zif_abappm_package_json=>ty_package_json
      RETURNING
        VALUE(result)    TYPE zif_abappm_package_json=>ty_package_json.

ENDCLASS.



CLASS zcl_abappm_package_json IMPLEMENTATION.


  METHOD constructor.

    DATA:
      ls_json  TYPE zif_abappm_package_json=>ty_package_json,
      li_json  TYPE REF TO zif_abappm_ajson,
      lx_error TYPE REF TO zcx_abappm_ajson_error.

    IF zif_abappm_package_json~is_valid_sap_package( iv_package ) = abap_false.
      zcx_abappm_package_json=>raise( |Invalid package: { iv_package }| ).
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
      CREATE OBJECT result TYPE zcl_abappm_package_json
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


  METHOD zif_abappm_package_json~delete.
    mo_persist->delete( ).
  ENDMETHOD.


  METHOD zif_abappm_package_json~get.
    result = ms_package_json.
  ENDMETHOD.


  METHOD zif_abappm_package_json~get_json.

    DATA:
      li_json       TYPE REF TO zif_abappm_ajson,
      ls_dependency TYPE zif_abappm_package_json=>ty_dependency,
      lx_error      TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        li_json = zcl_abappm_ajson=>new( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = ms_package_json ).

        li_json = li_json->map( zcl_abappm_ajson_mapping=>create_to_camel_case( ) ).

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
          li_json = li_json->filter( zcl_abappm_ajson_filter_lib=>create_empty_filter( ) ).
          IF ms_package_json-private = abap_false.
            li_json = li_json->filter( zcl_abappm_ajson_filter_lib=>create_path_filter( iv_skip_paths = '/private' ) ).
          ENDIF.
        ENDIF.

        result = li_json->stringify( 2 ).
      CATCH zcx_abappm_ajson_error.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid.

    DATA lt_errors TYPE string_table.

    TRY.
        lt_errors = zif_abappm_package_json~validate( ms_package_json ).
        result = boolc( lt_errors IS INITIAL ).
      CATCH zcx_abappm_package_json.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_cpu.

    result = boolc(
      iv_cpu IS INITIAL OR
      iv_cpu = zif_abappm_package_json~c_cpu-x86_64 OR
      iv_cpu = zif_abappm_package_json~c_cpu-power_pc OR
      iv_cpu = zif_abappm_package_json~c_cpu-sparc ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_db.

    result = boolc(
      iv_db IS INITIAL OR
      iv_db = zif_abappm_package_json~c_db-adabas_d OR
      iv_db = zif_abappm_package_json~c_db-db2 OR
      iv_db = zif_abappm_package_json~c_db-db400 OR
      iv_db = zif_abappm_package_json~c_db-db6 OR
      iv_db = zif_abappm_package_json~c_db-hdb OR
      iv_db = zif_abappm_package_json~c_db-informix OR
      iv_db = zif_abappm_package_json~c_db-mssql OR
      iv_db = zif_abappm_package_json~c_db-oracle OR
      iv_db = zif_abappm_package_json~c_db-sap_db OR
      iv_db = zif_abappm_package_json~c_db-sybase ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_email.

    " Email address validation (RFC 5322)
    CONSTANTS lc_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF iv_email IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX lc_email_regex IN iv_email.
      result = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_engine.

    result = boolc(
      iv_engine IS INITIAL OR
      iv_engine = zif_abappm_package_json~c_engine-abap OR
      iv_engine = zif_abappm_package_json~c_engine-apm ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_name.
    IF strlen( iv_name )
      BETWEEN zif_abappm_package_json~c_package_name-min_length
          AND zif_abappm_package_json~c_package_name-max_length.

      FIND REGEX zif_abappm_package_json~c_package_name-regex IN iv_name.
      result = boolc( sy-subrc =  0 ).
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_os.

    result = boolc(
      iv_os IS INITIAL OR
      iv_os = zif_abappm_package_json~c_os-aix OR
      iv_os = zif_abappm_package_json~c_os-hp_ux OR
      iv_os = zif_abappm_package_json~c_os-linux OR
      iv_os = zif_abappm_package_json~c_os-ms_windows OR
      iv_os = zif_abappm_package_json~c_os-os_390 OR
      iv_os = zif_abappm_package_json~c_os-os_400 OR
      iv_os = zif_abappm_package_json~c_os-solaris ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_package_type.

    result = boolc(
      iv_type IS INITIAL OR
      iv_type = zif_abappm_package_json~c_package_type-commonabap OR
      iv_type = zif_abappm_package_json~c_package_type-module ).

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_sap_package.

    DATA lv_package_type TYPE c LENGTH 1.

    " Limit to local, customer, namespaced, and partner packages (see type-pool TPAK)
    CALL METHOD cl_package_helper=>check_package_name
      EXPORTING
        i_package_name = iv_package
      IMPORTING
        e_package_type = lv_package_type
      EXCEPTIONS
        OTHERS         = 1.

    result = boolc( sy-subrc =  0 AND lv_package_type CA '$ZNJ' ).

    " Workaround for missing validation of empty namespace
    IF result = abap_true AND iv_package CP '//*'.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_url.

    " Basic URL validation
    CONSTANTS lc_url_regex TYPE string VALUE 'https?://.+\..+'.

    IF iv_url IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX lc_url_regex IN iv_url.
      result = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_version.
    " Check if it is a semantic version
    TRY.
        zcl_abappm_semver=>create( iv_version ).
        result = abap_true.
      CATCH zcx_abappm_semver_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abappm_package_json~is_valid_version_range.
    " Check if it is a semantic version range
    TRY.
        zcl_abappm_semver_range=>create( iv_range ).
        result = abap_true.
      CATCH zcx_abappm_semver_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abappm_package_json~load.
    zif_abappm_package_json~set_json( mo_persist->load( )-data ).
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_package_json~save.
    zif_abappm_package_json~validate( ms_package_json ).
    mo_persist->save(  zif_abappm_package_json~get_json( ) ).
  ENDMETHOD.


  METHOD zif_abappm_package_json~set.
    zif_abappm_package_json~validate( is_json ).
    ms_package_json = sort_dependencies( is_json ).
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_package_json~set_json.

    DATA:
      li_json         TYPE REF TO zif_abappm_ajson,
      ls_json_wo_deps TYPE zif_abappm_package_json=>ty_package_json_wo_deps,
      ls_dependency   TYPE zif_abappm_package_json=>ty_dependency,
      ls_json         TYPE zif_abappm_package_json=>ty_package_json,
      lx_error        TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        li_json = zcl_abappm_ajson=>parse( iv_json ).
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

        zif_abappm_package_json~validate( ls_json ).

        ms_package_json = sort_dependencies( ls_json ).
      CATCH zcx_abappm_ajson_error INTO lx_error.
        zcx_abappm_package_json=>raise_with_text( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.


  METHOD zif_abappm_package_json~validate.

    DATA:
      ls_dependency TYPE zif_abappm_package_json=>ty_dependency,
      ls_person     TYPE zif_abappm_package_json=>ty_person,
      lv_value      TYPE string,
      lt_values     TYPE string_table.

    IF zif_abappm_package_json~is_valid_name( is_package_json-name ) = abap_false.
      INSERT |Invalid name: { is_package_json-name }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_version( is_package_json-version ) = abap_false.
      INSERT |Invalid version: { is_package_json-version }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_package_type( is_package_json-type ) = abap_false.
      INSERT |Invalid package type: { is_package_json-type }| INTO TABLE result.
    ENDIF.

    IF is_package_json-private <> abap_false AND is_package_json-private <> abap_true.
      INSERT |Invalid private flag: { is_package_json-private }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_url( is_package_json-homepage ) = abap_false.
      INSERT |Invalid homepage URL: { is_package_json-homepage }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_email( is_package_json-bugs-email ) = abap_false.
      INSERT |Invalid bugs email: { is_package_json-bugs-email }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_url( is_package_json-bugs-url ) = abap_false.
      INSERT |Invalid bugs URL: { is_package_json-bugs-url }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_email( is_package_json-author-email ) = abap_false.
      INSERT |Invalid author email: { is_package_json-author-email }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_url( is_package_json-author-url ) = abap_false.
      INSERT |Invalid author URL: { is_package_json-author-url }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_cpu( is_package_json-cpu ) = abap_false.
      INSERT |Invalid CPU: { is_package_json-cpu }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_db( is_package_json-db ) = abap_false.
      INSERT |Invalid database: { is_package_json-db }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_os( is_package_json-os ) = abap_false.
      INSERT |Invalid operating system: { is_package_json-os }| INTO TABLE result.
    ENDIF.

    IF zif_abappm_package_json~is_valid_url( is_package_json-repository-url ) = abap_false.
      INSERT |Invalid repository URL: { is_package_json-repository-url }| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-contributors INTO ls_person.
      COLLECT ls_person-name INTO lt_values.
      IF zif_abappm_package_json~is_valid_email( ls_person-email ) = abap_false.
        INSERT |Invalid contributor email: { ls_person-name } { ls_person-email }| INTO TABLE result.
      ENDIF.
      IF zif_abappm_package_json~is_valid_url( ls_person-url ) = abap_false.
        INSERT |Invalid contributor URL: { ls_person-name } { ls_person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-contributors ) <> lines( lt_values ).
      INSERT |Duplicate contributors| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-maintainers INTO ls_person.
      COLLECT ls_person-name INTO lt_values.
      IF zif_abappm_package_json~is_valid_email( ls_person-email ) = abap_false.
        INSERT |Invalid maintainer email: { ls_person-name } { ls_person-email }| INTO TABLE result.
      ENDIF.
      IF zif_abappm_package_json~is_valid_url( ls_person-url ) = abap_false.
        INSERT |Invalid maintainer URL: { ls_person-name } { ls_person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-maintainers ) <> lines( lt_values ).
      INSERT |Duplicate maintainers| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-engines INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_abappm_package_json~is_valid_engine( ls_dependency-name ) = abap_false.
        INSERT |Invalid engine: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zif_abappm_package_json~is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid engine version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-engines ) <> lines( lt_values ).
      INSERT |Duplicate engines| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_abappm_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        INSERT |Invalid dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zif_abappm_package_json~is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid dependency version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-dependencies ) <> lines( lt_values ).
      INSERT |Duplicate dependencies| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-dev_dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_abappm_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        INSERT |Invalid dev dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zif_abappm_package_json~is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid dev dependency version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
      READ TABLE is_package_json-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        INSERT |Dev dependency { ls_dependency-name } already included in dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-dev_dependencies ) <> lines( lt_values ).
      INSERT |Duplicate dev dependencies| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-optional_dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_abappm_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        INSERT |Invalid opt dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zif_abappm_package_json~is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid opt dependency version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
      READ TABLE is_package_json-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        INSERT |Opt dependency { ls_dependency-name } already included in dependencies| INTO TABLE result.
      ENDIF.
      READ TABLE is_package_json-dev_dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        INSERT |Opt dependency { ls_dependency-name } already included in dev dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-optional_dependencies ) <> lines( lt_values ).
      INSERT |Duplicate optional dependencies| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-bundled_dependencies INTO lv_value.
      COLLECT lv_value INTO lt_values.
      IF zif_abappm_package_json~is_valid_name( lv_value ) = abap_false.
        INSERT |Invalid bundled dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      READ TABLE is_package_json-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc <> 0.
        INSERT |Bundeled dependency { ls_dependency-name } not included in dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-bundled_dependencies ) <> lines( lt_values ).
      INSERT |Duplicate bundled dependencies| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_package_json-engines INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zif_abappm_package_json~is_valid_name( ls_dependency-name ) = abap_false.
        INSERT |Invalid engine: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zif_abappm_package_json~is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid engine version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_package_json-engines ) <> lines( lt_values ).
      INSERT |Duplicate engines| INTO TABLE result.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
