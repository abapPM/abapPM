CLASS zcl_abappm_settings DEFINITION
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

    INTERFACES zif_abappm_settings.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !name         TYPE zif_abappm_settings=>ty_name DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_settings
      RAISING
        zcx_abappm_error.

    CLASS-METHODS injector
      IMPORTING
        !name TYPE zif_abappm_settings=>ty_name
        !mock TYPE REF TO zif_abappm_settings.

    METHODS constructor
      IMPORTING
        !name TYPE zif_abappm_settings=>ty_name
      RAISING
        zcx_abappm_error.

    CLASS-METHODS initialize_global_settings
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_setting_key
      IMPORTING
        !name         TYPE zif_abappm_settings=>ty_name
      RETURNING
        VALUE(result) TYPE zif_abappm_persist_apm=>ty_key.

    CLASS-METHODS get_default
      RETURNING
        VALUE(result) TYPE zif_abappm_settings=>ty_settings.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE zif_abappm_settings=>ty_name,
        instance TYPE REF TO zif_abappm_settings,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CLASS-DATA:
      db_persist TYPE REF TO zif_abappm_persist_apm,
      instances  TYPE ty_instances.

    DATA:
      key      TYPE zif_abappm_persist_apm=>ty_key,
      name     TYPE zif_abappm_settings=>ty_name,
      settings TYPE zif_abappm_settings=>ty_settings.

    CLASS-METHODS check_settings
      IMPORTING
        !is_settings  TYPE zif_abappm_settings=>ty_settings
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS merge_settings
      CHANGING
        !cs_settings TYPE zif_abappm_settings=>ty_settings.

ENDCLASS.



CLASS zcl_abappm_settings IMPLEMENTATION.


  METHOD check_settings.

    IF zcl_abappm_package_json_valid=>is_valid_url( is_settings-registry ) = abap_false.
      INSERT |Invalid registry URL: { is_settings-registry }| INTO TABLE result.
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.

    db_persist = zcl_abappm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    IF name IS INITIAL OR strlen( name ) > 12.
      zcx_abappm_error=>raise( |Invalid name: { name }| ).
    ENDIF.

    me->name = name.
    key = get_setting_key( name ).

    TRY.
        zif_abappm_settings~load( ).
      CATCH zcx_abappm_error.
        IF name = zif_abappm_settings=>c_global.
          settings = get_default( ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      result = NEW zcl_abappm_settings( name ).

      DATA(instance) = VALUE ty_instance(
        name     = name
        instance = result ).
      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_default.

    " Default values for settings
    " TODO: Change to production registry
    result-registry = zif_abappm_settings=>c_playground.

    result-list_settings-order_by = 'PACKAGE'.

  ENDMETHOD.


  METHOD get_setting_key.

    IF name = zif_abappm_settings=>c_global.
      result = |{ zif_abappm_persist_apm=>c_key_type-settings }:{ zif_abappm_settings=>c_global }:ALL|.
    ELSE.
      result = |{ zif_abappm_persist_apm=>c_key_type-settings }:{ zif_abappm_settings=>c_user }:{ name }|.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_global_settings.

    DATA(global) = factory( zif_abappm_settings=>c_global ).

    " Check if global settings exist already
    TRY.
        DATA(settings) = global->load( )->get( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

    IF settings IS NOT INITIAL.
      RETURN.
    ENDIF.

    global->set( get_default( ) ).

    " Save defaults to global settings
    db_persist->save(
      key   = get_setting_key( zif_abappm_settings=>c_global )
      value = global->get_json( ) ).

  ENDMETHOD.


  METHOD injector.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      <instance>-instance = mock.
    ELSE.
      DATA(instance) = VALUE ty_instance(
        name     = name
        instance = mock ).
      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD merge_settings.

    TRY.
        DATA(global) = factory( zif_abappm_settings=>c_global )->get( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
        " Just use defaults
    ENDTRY.

    DATA(default) = get_default( ).

    DO.
      " Current settings
      ASSIGN COMPONENT sy-index OF STRUCTURE cs_settings TO FIELD-SYMBOL(<value>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <value> IS INITIAL.
        " Global settings
        ASSIGN COMPONENT sy-index OF STRUCTURE global TO FIELD-SYMBOL(<global>).
        ASSERT sy-subrc = 0.

        IF <value> IS INITIAL.
          " apm default settings
          ASSIGN COMPONENT sy-index OF STRUCTURE default TO FIELD-SYMBOL(<default>).
          ASSERT sy-subrc = 0.

          <value> = <default>.
        ELSE.
          <value> = <global>.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_abappm_settings~delete.

    IF name = zif_abappm_settings=>c_global.
      zcx_abappm_error=>raise( 'Global settings can not be deleted' ).
    ENDIF.

    db_persist->delete( key ).

  ENDMETHOD.


  METHOD zif_abappm_settings~get.

    result = settings.

    IF name <> zif_abappm_settings=>c_global.
      merge_settings( CHANGING cs_settings = result ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_settings~get_json.

    TRY.
        DATA(ajson) = zcl_abappm_ajson=>new( )->keep_item_order( )->map(
          zcl_abappm_ajson_mapping=>create_to_camel_case( ) )->set(
            iv_path = '/'
            iv_val  = zif_abappm_settings~get( ) ).

        IF is_complete = abap_false.
          ajson = ajson->filter( lcl_ajson_filters=>create_empty_filter( ) ).
        ENDIF.

        result = ajson->stringify( 2 ).
      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_settings~is_valid.

    result = xsdbool( check_settings( settings ) IS INITIAL ).

  ENDMETHOD.


  METHOD zif_abappm_settings~load.

    zif_abappm_settings~set_json( db_persist->load( key )-value ).
    result = me.

  ENDMETHOD.


  METHOD zif_abappm_settings~save.

    IF zif_abappm_settings~is_valid( ) = abap_false.
      zcx_abappm_error=>raise( 'Invalid settings' ).
    ENDIF.

    " Save complete JSON including empty values for easy editing
    db_persist->save(
      key   = key
      value = zif_abappm_settings~get_json( abap_true ) ).

  ENDMETHOD.


  METHOD zif_abappm_settings~set.

    IF check_settings( settings ) IS NOT INITIAL.
      zcx_abappm_error=>raise( 'Invalid settings' ).
    ENDIF.

    me->settings = CORRESPONDING #( settings ).
    result = me.

  ENDMETHOD.


  METHOD zif_abappm_settings~set_json.

    DATA settings TYPE zif_abappm_settings=>ty_settings.

    TRY.
        DATA(ajson) = zcl_abappm_ajson=>parse(
          iv_json           = json
          ii_custom_mapping = zcl_abappm_ajson_mapping=>create_to_camel_case( ) ).

        ajson->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = settings ).

        IF check_settings( settings ) IS NOT INITIAL.
          zcx_abappm_error=>raise( 'Invalid settings' ).
        ENDIF.

        me->settings = CORRESPONDING #( settings ).
      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    result = me.

  ENDMETHOD.
ENDCLASS.
