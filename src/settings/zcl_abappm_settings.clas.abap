CLASS zcl_abappm_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Settings
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abappm_settings.

    TYPES ty_name TYPE uname.

    CONSTANTS c_global TYPE ty_name VALUE '$GLOBAL$'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_name      TYPE ty_name DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_settings
      RAISING
        zcx_abappm_error.

    CLASS-METHODS injector
      IMPORTING
        !iv_name TYPE ty_name
        !ii_mock TYPE REF TO zif_abappm_settings.

    METHODS constructor
      IMPORTING
        !iv_name TYPE ty_name
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_default
      RETURNING
        VALUE(result) TYPE zif_abappm_settings=>ty_settings.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE ty_name,
        instance TYPE REF TO zif_abappm_settings,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CONSTANTS c_settings TYPE string VALUE 'SETTINGS'.

    CLASS-DATA:
      gi_persist   TYPE REF TO zif_abappm_persist_apm,
      gt_instances TYPE ty_instances.

    DATA:
      mv_key      TYPE zif_abappm_persist_apm=>ty_key,
      mv_name     TYPE ty_name,
      ms_settings TYPE zif_abappm_settings=>ty_settings.

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
    gi_persist = zcl_abappm_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

    IF iv_name IS INITIAL OR strlen( iv_name ) > 12.
      zcx_abappm_error=>raise( |Invalid name: { iv_name }| ).
    ENDIF.

    mv_name = iv_name.
    mv_key  = |{ zif_abappm_persist_apm=>c_key_type-settings }:{ mv_name }|.

    TRY.
        zif_abappm_settings~load( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE zcl_abappm_settings
        EXPORTING
          iv_name = iv_name.

      ls_instance-name     = iv_name.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_default.
    " Default values for settings
    result-registry = zif_abappm_constants=>c_registry.
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
      ls_global  TYPE zif_abappm_settings=>ty_settings,
      ls_default TYPE zif_abappm_settings=>ty_settings.

    FIELD-SYMBOLS:
      <lv_value>   TYPE any,
      <lv_global>  TYPE any,
      <lv_default> TYPE any.

    TRY.
        ls_global  = factory( c_global )->get( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
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


  METHOD zif_abappm_settings~delete.

    DATA lx_error TYPE REF TO zcx_abappm_persist_apm.

    IF mv_name = c_global.
      zcx_abappm_error=>raise( 'Global settings can not be deleted' ).
    ENDIF.

    TRY.
        gi_persist->delete( mv_key ).
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_settings~get.
    result = ms_settings.
    IF mv_name <> c_global.
      merge_settings( CHANGING cs_settings = result ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abappm_settings~get_json.

    DATA:
      li_json  TYPE REF TO zif_abappm_ajson,
      lx_error TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        li_json = zcl_abappm_ajson=>new( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = zif_abappm_settings~get( ) ).

        li_json = li_json->map( zcl_abappm_ajson_mapping=>create_to_camel_case( ) ).

        IF iv_complete = abap_false.
          li_json = li_json->filter( lcl_ajson_filters=>create_empty_filter( ) ).
        ENDIF.

        result = li_json->stringify( 2 ).
      CATCH zcx_abappm_ajson_error.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_settings~is_valid.
    result = boolc( check_settings( ms_settings ) IS INITIAL ).
  ENDMETHOD.


  METHOD zif_abappm_settings~load.

    DATA:
      lv_value TYPE string,
      lx_error TYPE REF TO zcx_abappm_persist_apm.

    TRY.
        lv_value = gi_persist->load( mv_key )-value.
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    zif_abappm_settings~set_json( lv_value ).
    result = me.

  ENDMETHOD.


  METHOD zif_abappm_settings~save.

    DATA lx_error TYPE REF TO zcx_abappm_persist_apm.

    IF zif_abappm_settings~is_valid( ) = abap_false.
      zcx_abappm_error=>raise( 'Invalid settings' ).
    ENDIF.

    TRY.
        gi_persist->save(
          iv_key   = mv_key
          iv_value = zif_abappm_settings~get_json( ) ).
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_settings~set.
    IF check_settings( is_settings ) IS NOT INITIAL.
      zcx_abappm_error=>raise( 'Invalid settings' ).
    ENDIF.

    MOVE-CORRESPONDING is_settings TO ms_settings.
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_settings~set_json.

    DATA:
      li_json  TYPE REF TO zif_abappm_ajson,
      ls_json  TYPE zif_abappm_settings=>ty_settings,
      lx_error TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        li_json = zcl_abappm_ajson=>parse( iv_json ).
        li_json->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = ls_json ).

        IF check_settings( ls_json ) IS NOT INITIAL.
          zcx_abappm_error=>raise( 'Invalid settings' ).
        ENDIF.

        MOVE-CORRESPONDING ls_json TO ms_settings.
      CATCH zcx_abappm_ajson_error INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.
ENDCLASS.
