CLASS ZCL_ABAPPM_SETTINGS DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Settings
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES ZIF_ABAPPM_SETTINGS.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_name      TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_SETTINGS
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS injector
      IMPORTING
        !iv_name TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME
        !ii_mock TYPE REF TO ZIF_ABAPPM_SETTINGS.

    METHODS constructor
      IMPORTING
        !iv_name TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS initialize_global_settings
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS get_setting_key
      IMPORTING
        !iv_name      TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY.

    CLASS-METHODS get_default
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME,
        instance TYPE REF TO ZIF_ABAPPM_SETTINGS,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CONSTANTS c_settings TYPE string VALUE 'SETTINGS'.

    CLASS-DATA:
      gi_persist   TYPE REF TO ZIF_ABAPPM_PERSIST_APM,
      gt_instances TYPE ty_instances.

    DATA:
      mv_key      TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY,
      mv_name     TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME,
      ms_settings TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS.

    CLASS-METHODS check_settings
      IMPORTING
        !is_settings  TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS merge_settings
      CHANGING
        !cs_settings TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS.

ENDCLASS.



CLASS ZCL_ABAPPM_SETTINGS IMPLEMENTATION.


  METHOD check_settings.
    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_URL( is_settings-registry ) = abap_false.
      INSERT |Invalid registry URL: { is_settings-registry }| INTO TABLE result.
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.
    gi_persist = ZCL_ABAPPM_PERSIST_APM=>GET_INSTANCE( ).
  ENDMETHOD.


  METHOD constructor.

    IF iv_name IS INITIAL OR strlen( iv_name ) > 12.
      ZCX_ABAPPM_ERROR=>RAISE( |Invalid name: { iv_name }| ).
    ENDIF.

    mv_name = iv_name.
    mv_key  = get_setting_key( mv_name ).

    TRY.
        ZIF_ABAPPM_SETTINGS~LOAD( ).
      CATCH ZCX_ABAPPM_ERROR.
        IF mv_name = ZIF_ABAPPM_SETTINGS=>C_GLOBAL.
          ms_settings = get_default( ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE ZCL_ABAPPM_SETTINGS
        EXPORTING
          iv_name = iv_name.

      ls_instance-name     = iv_name.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_default.
    " Default values for settings
    result-registry = ZIF_ABAPPM_SETTINGS=>C_REGISTRY.
  ENDMETHOD.


  METHOD get_setting_key.
    result = |{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-SETTINGS }:{ iv_name }|.
  ENDMETHOD.


  METHOD initialize_global_settings.

    DATA:
      li_global TYPE REF TO ZIF_ABAPPM_SETTINGS,
      ls_global TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS.

    li_global = factory( ZIF_ABAPPM_SETTINGS=>C_GLOBAL ).

    " Check if global settings exist already
    TRY.
        ls_global = li_global->load( )->get( ).
      CATCH ZCX_ABAPPM_ERROR ##NO_HANDLER.
    ENDTRY.

    IF ls_global IS NOT INITIAL.
      RETURN.
    ENDIF.

    li_global->set( get_default( ) ).

    " Save defaults to global settings
    gi_persist->save(
      iv_key   = get_setting_key( ZIF_ABAPPM_SETTINGS=>C_GLOBAL )
      iv_value = li_global->get_json( ) ).

  ENDMETHOD.


  METHOD injector.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      <ls_instance>-instance = ii_mock.
    ELSE.
      ls_instance-name     = iv_name.
      ls_instance-instance = ii_mock.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD merge_settings.

    DATA:
      ls_global  TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS,
      ls_default TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS.

    FIELD-SYMBOLS:
      <lv_value>   TYPE any,
      <lv_global>  TYPE any,
      <lv_default> TYPE any.

    TRY.
        ls_global  = factory( ZIF_ABAPPM_SETTINGS=>C_GLOBAL )->get( ).
      CATCH ZCX_ABAPPM_ERROR ##NO_HANDLER.
        " Just use defaults
    ENDTRY.

    ls_default = get_default( ).

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE cs_settings TO <lv_value>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <lv_value> IS INITIAL.
        ASSIGN COMPONENT sy-index OF STRUCTURE ls_global TO <lv_global>.
        ASSERT sy-subrc = 0.

        IF <lv_value> IS INITIAL.
          ASSIGN COMPONENT sy-index OF STRUCTURE ls_default TO <lv_default>.
          ASSERT sy-subrc = 0.

          <lv_value> = <lv_default>.
        ELSE.
          <lv_value> = <lv_global>.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~DELETE.

    IF mv_name = ZIF_ABAPPM_SETTINGS=>C_GLOBAL.
      ZCX_ABAPPM_ERROR=>RAISE( 'Global settings can not be deleted' ).
    ENDIF.

    gi_persist->delete( mv_key ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~GET.
    result = ms_settings.
    IF mv_name <> ZIF_ABAPPM_SETTINGS=>C_GLOBAL.
      merge_settings( CHANGING cs_settings = result ).
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~GET_JSON.

    DATA:
      li_json  TYPE REF TO ZIF_ABAPPM_AJSON,
      lx_error TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        li_json = ZCL_ABAPPM_AJSON=>NEW( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = ZIF_ABAPPM_SETTINGS~GET( ) ).

        li_json = li_json->map( ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( ) ).

        IF iv_complete = abap_false.
          li_json = li_json->filter( lcl_ajson_filters=>create_empty_filter( ) ).
        ENDIF.

        result = li_json->stringify( 2 ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO lx_error.
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~IS_VALID.
    result = boolc( check_settings( ms_settings ) IS INITIAL ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~LOAD.
    ZIF_ABAPPM_SETTINGS~SET_JSON( gi_persist->load( mv_key )-value ).
    result = me.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~SAVE.

    IF ZIF_ABAPPM_SETTINGS~IS_VALID( ) = abap_false.
      ZCX_ABAPPM_ERROR=>RAISE( 'Invalid settings' ).
    ENDIF.

    " Save complete JSON including empty values for easy editing
    gi_persist->save(
      iv_key   = mv_key
      iv_value = ZIF_ABAPPM_SETTINGS~GET_JSON( abap_true ) ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~SET.

    IF check_settings( is_settings ) IS NOT INITIAL.
      ZCX_ABAPPM_ERROR=>RAISE( 'Invalid settings' ).
    ENDIF.

    MOVE-CORRESPONDING is_settings TO ms_settings.
    result = me.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~SET_JSON.

    DATA:
      li_json  TYPE REF TO ZIF_ABAPPM_AJSON,
      ls_json  TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS,
      lx_error TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        li_json = ZCL_ABAPPM_AJSON=>PARSE( iv_json ).
        " TODO: packageSettings does not map to package_setting
        " Looks like a ajson bug
        li_json->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = ls_json ).

        IF check_settings( ls_json ) IS NOT INITIAL.
          ZCX_ABAPPM_ERROR=>RAISE( 'Invalid settings' ).
        ENDIF.

        MOVE-CORRESPONDING ls_json TO ms_settings.
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO lx_error.
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.
ENDCLASS.
