CLASS /apmg/cl_apm_settings DEFINITION
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

    INTERFACES /apmg/if_apm_settings.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !name         TYPE /apmg/if_apm_settings=>ty_name DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_settings
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS injector
      IMPORTING
        !name TYPE /apmg/if_apm_settings=>ty_name
        !mock TYPE REF TO /apmg/if_apm_settings.

    METHODS constructor
      IMPORTING
        !name TYPE /apmg/if_apm_settings=>ty_name
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS initialize_global_settings
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_setting_key
      IMPORTING
        !name         TYPE /apmg/if_apm_settings=>ty_name
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_persist_apm=>ty_key.

    CLASS-METHODS get_default
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_settings=>ty_settings.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE /apmg/if_apm_settings=>ty_name,
        instance TYPE REF TO /apmg/if_apm_settings,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CLASS-DATA:
      db_persist TYPE REF TO /apmg/if_apm_persist_apm,
      instances  TYPE ty_instances.

    DATA:
      key      TYPE /apmg/if_apm_persist_apm=>ty_key,
      name     TYPE /apmg/if_apm_settings=>ty_name,
      settings TYPE /apmg/if_apm_settings=>ty_settings.

    CLASS-METHODS check_settings
      IMPORTING
        !is_settings  TYPE /apmg/if_apm_settings=>ty_settings
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS merge_settings
      CHANGING
        !cs_settings TYPE /apmg/if_apm_settings=>ty_settings.

ENDCLASS.



CLASS /apmg/cl_apm_settings IMPLEMENTATION.


  METHOD /apmg/if_apm_settings~delete.

    IF name = /apmg/if_apm_settings=>c_global.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Global settings can not be deleted'.
    ENDIF.

    db_persist->delete( key ).

  ENDMETHOD.


  METHOD /apmg/if_apm_settings~get.

    result = settings.

    IF name <> /apmg/if_apm_settings=>c_global.
      merge_settings( CHANGING cs_settings = result ).
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_settings~get_json.

    TRY.
        DATA(ajson) = /apmg/cl_apm_ajson=>new(
          )->keep_item_order(
          )->set(
            iv_path = '/'
            iv_val  = /apmg/if_apm_settings~get( )
          )->map( /apmg/cl_apm_ajson_mapping=>create_to_camel_case( ) ).

        IF is_complete = abap_false.
          ajson = ajson->filter( /apmg/cl_apm_ajson_extensions=>filter_empty_zero_null( ) ).
        ENDIF.

        result = ajson->stringify( 2 ).
      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_settings~is_valid.

    result = xsdbool( check_settings( settings ) IS INITIAL ).

  ENDMETHOD.


  METHOD /apmg/if_apm_settings~load.

    /apmg/if_apm_settings~set_json( db_persist->load( key )-value ).
    result = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_settings~save.

    IF /apmg/if_apm_settings~is_valid( ) = abap_false.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid settings'.
    ENDIF.

    " Save complete JSON including empty values for easy editing
    db_persist->save(
      key   = key
      value = /apmg/if_apm_settings~get_json( abap_true ) ).

  ENDMETHOD.


  METHOD /apmg/if_apm_settings~set.

    IF check_settings( settings ) IS NOT INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid settings'.
    ENDIF.

    me->settings = CORRESPONDING #( settings ).
    result = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_settings~set_json.

    DATA settings TYPE /apmg/if_apm_settings=>ty_settings.

    TRY.
        DATA(ajson) = /apmg/cl_apm_ajson=>parse( json
          )->to_abap_corresponding_only(
          )->map( /apmg/cl_apm_ajson_extensions=>from_camel_case_underscore( ) ).

        ajson->to_abap( IMPORTING ev_container = settings ).

        IF check_settings( settings ) IS NOT INITIAL.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid settings'.
        ENDIF.

        me->settings = CORRESPONDING #( settings ).
      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

    result = me.

  ENDMETHOD.


  METHOD check_settings.

    TRY.
        IF is_settings-registry IS NOT INITIAL.
          DATA(url) = /apmg/cl_apm_url=>parse( is_settings-registry ).

          IF url->components-path IS NOT INITIAL.
            INSERT |Registry URL must not include any trailing slash or path: { is_settings-registry }|
              INTO TABLE result.
          ENDIF.
          IF url->components-query IS NOT INITIAL.
            INSERT |Registry URL must not include any query: { is_settings-registry }| INTO TABLE result.
          ENDIF.
          IF url->components-fragment IS NOT INITIAL.
            INSERT |Registry URL must not include any fragment: { is_settings-registry }| INTO TABLE result.
          ENDIF.
        ENDIF.
      CATCH /apmg/cx_apm_error.
        INSERT |Invalid registry URL: { is_settings-registry }| INTO TABLE result.
    ENDTRY.

  ENDMETHOD.


  METHOD class_constructor.

    db_persist = /apmg/cl_apm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    IF name IS INITIAL OR strlen( name ) > 12.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = |Invalid name: { name }|.
    ENDIF.

    me->name = name.
    key = get_setting_key( name ).

    TRY.
        /apmg/if_apm_settings~load( ).
      CATCH /apmg/cx_apm_error.
        IF name = /apmg/if_apm_settings=>c_global.
          settings = get_default( ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      result = NEW /apmg/cl_apm_settings( name ).

      DATA(instance) = VALUE ty_instance(
        name     = name
        instance = result ).
      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_default.

    " Default values for settings
    " TODO: Change to production registry
    result-registry = /apmg/if_apm_settings=>c_playground.

    result-list_settings-order_by = 'PACKAGE'.

  ENDMETHOD.


  METHOD get_setting_key.

    IF name = /apmg/if_apm_settings=>c_global.
      result = |{ /apmg/if_apm_persist_apm=>c_key_type-settings }:{ /apmg/if_apm_settings=>c_global }:ALL|.
    ELSE.
      result = |{ /apmg/if_apm_persist_apm=>c_key_type-settings }:{ /apmg/if_apm_settings=>c_user }:{ name }|.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_global_settings.

    DATA(global) = factory( /apmg/if_apm_settings=>c_global ).

    " Check if global settings exist already
    TRY.
        DATA(settings) = global->load( )->get( ).
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
    ENDTRY.

    IF settings IS NOT INITIAL.
      RETURN.
    ENDIF.

    global->set( get_default( ) ).

    " Save defaults to global settings
    db_persist->save(
      key   = get_setting_key( /apmg/if_apm_settings=>c_global )
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
        DATA(global) = factory( /apmg/if_apm_settings=>c_global )->get( ).
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
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
ENDCLASS.
