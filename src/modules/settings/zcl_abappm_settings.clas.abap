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
        !name         TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_SETTINGS
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS injector
      IMPORTING
        !name TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME
        !mock TYPE REF TO ZIF_ABAPPM_SETTINGS.

    METHODS constructor
      IMPORTING
        !name TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS initialize_global_settings
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS get_setting_key
      IMPORTING
        !name         TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME
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
      db_persist TYPE REF TO ZIF_ABAPPM_PERSIST_APM,
      instances  TYPE ty_instances.

    DATA:
      key      TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY,
      name     TYPE ZIF_ABAPPM_SETTINGS=>TY_NAME,
      settings TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS.

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
    db_persist = ZCL_ABAPPM_PERSIST_APM=>GET_INSTANCE( ).
  ENDMETHOD.


  METHOD constructor.

    IF name IS INITIAL OR strlen( name ) > 12.
      ZCX_ABAPPM_ERROR=>RAISE( |Invalid name: { name }| ).
    ENDIF.

    me->name = name.
    me->key  = get_setting_key( name ).

    TRY.
        ZIF_ABAPPM_SETTINGS~LOAD( ).
      CATCH ZCX_ABAPPM_ERROR.
        IF name = ZIF_ABAPPM_SETTINGS=>C_GLOBAL.
          settings = get_default( ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE instances ASSIGNING <ls_instance> WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE ZCL_ABAPPM_SETTINGS
        EXPORTING
          name = name.

      ls_instance-name     = name.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_default.
    " Default values for settings
    result-registry = ZIF_ABAPPM_SETTINGS=>C_REGISTRY.
  ENDMETHOD.


  METHOD get_setting_key.
    result = |{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-SETTINGS }:{ name }|.
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
    db_persist->save(
      key   = get_setting_key( ZIF_ABAPPM_SETTINGS=>C_GLOBAL )
      value = li_global->get_json( ) ).

  ENDMETHOD.


  METHOD injector.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE instances ASSIGNING <ls_instance> WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      <ls_instance>-instance = mock.
    ELSE.
      ls_instance-name     = name.
      ls_instance-instance = mock.
      INSERT ls_instance INTO TABLE instances.
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
      " Current settings
      ASSIGN COMPONENT sy-index OF STRUCTURE cs_settings TO <lv_value>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <lv_value> IS INITIAL.
        " Global settings
        ASSIGN COMPONENT sy-index OF STRUCTURE ls_global TO <lv_global>.
        ASSERT sy-subrc = 0.

        IF <lv_value> IS INITIAL.
          " apm default settings
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

    IF name = ZIF_ABAPPM_SETTINGS=>C_GLOBAL.
      ZCX_ABAPPM_ERROR=>RAISE( 'Global settings can not be deleted' ).
    ENDIF.

    db_persist->delete( key ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~GET.
    result = settings.
    IF name <> ZIF_ABAPPM_SETTINGS=>C_GLOBAL.
      merge_settings( CHANGING cs_settings = result ).
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~GET_JSON.

    DATA:
      ajson    TYPE REF TO ZIF_ABAPPM_AJSON,
      lx_error TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        ajson = ZCL_ABAPPM_AJSON=>NEW( )->keep_item_order( )->map(
          ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( ) )->set(
            iv_path = '/'
            iv_val  = ZIF_ABAPPM_SETTINGS~GET( ) ).

        IF is_complete = abap_false.
          ajson = ajson->filter( lcl_ajson_filters=>create_empty_filter( ) ).
        ENDIF.

        result = ajson->stringify( 2 ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO lx_error.
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~IS_VALID.
    result = boolc( check_settings( settings ) IS INITIAL ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~LOAD.
    ZIF_ABAPPM_SETTINGS~SET_JSON( db_persist->load( key )-value ).
    result = me.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~SAVE.

    IF ZIF_ABAPPM_SETTINGS~IS_VALID( ) = abap_false.
      ZCX_ABAPPM_ERROR=>RAISE( 'Invalid settings' ).
    ENDIF.

    " Save complete JSON including empty values for easy editing
    db_persist->save(
      key   = key
      value = ZIF_ABAPPM_SETTINGS~GET_JSON( abap_true ) ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~SET.

    IF check_settings( settings ) IS NOT INITIAL.
      ZCX_ABAPPM_ERROR=>RAISE( 'Invalid settings' ).
    ENDIF.

    MOVE-CORRESPONDING settings TO me->settings.
    result = me.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_SETTINGS~SET_JSON.

    DATA settings TYPE ZIF_ABAPPM_SETTINGS=>TY_SETTINGS.

    TRY.
        DATA(ajson) = ZCL_ABAPPM_AJSON=>PARSE(
          iv_json           = json
          ii_custom_mapping = ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( ) ).

        ajson->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = settings ).

        IF check_settings( settings ) IS NOT INITIAL.
          ZCX_ABAPPM_ERROR=>RAISE( 'Invalid settings' ).
        ENDIF.

        MOVE-CORRESPONDING settings TO me->settings.
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO DATA(error).
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( error ).
    ENDTRY.

    result = me.

  ENDMETHOD.
ENDCLASS.
