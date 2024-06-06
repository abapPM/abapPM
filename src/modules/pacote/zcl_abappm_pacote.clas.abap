CLASS zcl_abappm_pacote DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Pacote
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abappm_pacote.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_registry  TYPE string
        !iv_name      TYPE string
        !iv_packument TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_pacote
      RAISING
        zcx_abappm_pacote.

    CLASS-METHODS injector
      IMPORTING
        !iv_name TYPE string
        !ii_mock TYPE REF TO zif_abappm_pacote.

    METHODS constructor
      IMPORTING
        !iv_registry  TYPE string
        !iv_name      TYPE string
        !iv_packument TYPE string OPTIONAL
      RAISING
        zcx_abappm_pacote.

    CLASS-METHODS get_packument_key
      IMPORTING
        !iv_name      TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_persist_apm=>ty_key.

    CLASS-METHODS get_packument_from_key
      IMPORTING
        !iv_key       TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE string,
        instance TYPE REF TO zif_abappm_pacote,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CLASS-DATA:
      gi_persist   TYPE REF TO zif_abappm_persist_apm,
      gt_instances TYPE ty_instances.

    DATA:
      mv_registry TYPE string,
      ms_pacote   TYPE zif_abappm_pacote=>ty_pacote.

    METHODS get_agent
      IMPORTING
        !iv_url         TYPE string
        !iv_abbreviated TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE REF TO zif_abapgit_http_agent
      RAISING
        zcx_abapgit_exception.

    METHODS check_result
      IMPORTING
        !iv_json TYPE string
      RAISING
        zcx_abappm_pacote.

ENDCLASS.



CLASS zcl_abappm_pacote IMPLEMENTATION.


  METHOD check_result.

    DATA:
      lv_error TYPE string,
      lx_error TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        lv_error = zcl_abappm_ajson=>parse( iv_json )->get_string( '/error' ).
      CATCH zcx_abappm_ajson_error INTO lx_error.
        zcx_abappm_pacote=>raise_with_text( lx_error ).
    ENDTRY.

    IF lv_error IS NOT INITIAL.
      zcx_abappm_pacote=>raise( lv_error ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
    gi_persist = zcl_abappm_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

    IF iv_registry <> 'https://registry.abappm.com'.
      zcx_abappm_pacote=>raise( 'Only works with registry.abappm.com' ).
    ENDIF.

    mv_registry = iv_registry.

    ms_pacote-key       = get_packument_key( iv_name ).
    ms_pacote-name      = escape(
                            val    = iv_name
                            format = cl_abap_format=>e_url_full ).
    ms_pacote-packument = iv_packument.

    TRY.
        zif_abappm_pacote~load( ).
      CATCH zcx_abappm_pacote ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE zcl_abappm_pacote
        EXPORTING
          iv_registry  = iv_registry
          iv_name      = iv_name
          iv_packument = iv_packument.

      ls_instance-name     = iv_name.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_agent.

    DATA lv_url TYPE string.

    result = zcl_abapgit_factory=>get_http_agent( ).

    IF iv_abbreviated = abap_true.
      result->global_headers( )->set(
        iv_key = 'Accept'
        iv_val = 'application/vnd.npm.install-v1+json' ).
    ELSE.
      result->global_headers( )->set(
      iv_key = 'Accept'
      iv_val = 'application/json' ).
    ENDIF.

    " Login manager requires git-like url so we add some dummy repo
    lv_url = iv_url && '/apm/apm.git'.

    " Get auth token from repo
    IF zcl_abapgit_login_manager=>get( lv_url ) IS NOT INITIAL.
      result->global_headers( )->set(
        iv_key = 'Authorization'
        iv_val = zcl_abapgit_login_manager=>get( lv_url ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_packument_from_key.

    DATA:
      lv_prefix TYPE string,
      lv_suffix TYPE string.

    SPLIT iv_key AT ':' INTO lv_prefix result lv_suffix.
    result = to_lower( result ).

  ENDMETHOD.


  METHOD get_packument_key.
    result = |{ zif_abappm_persist_apm=>c_key_type-packument }:{ to_upper( iv_name ) }|.
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


  METHOD zif_abappm_pacote~delete.

    DATA lx_error TYPE REF TO zcx_abappm_persist_apm.

    TRY.
        gi_persist->delete( ms_pacote-key ).
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abappm_pacote=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_pacote~exists.

    TRY.
        gi_persist->load( ms_pacote-key ).
        result = abap_true.
      CATCH zcx_abappm_persist_apm.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_pacote~get.
    result = ms_pacote-packument.
  ENDMETHOD.


  METHOD zif_abappm_pacote~load.

    DATA lx_error TYPE REF TO zcx_abappm_persist_apm.

    TRY.
        ms_pacote-packument = gi_persist->load( ms_pacote-key )-value.
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abappm_pacote=>raise_with_text( lx_error ).
    ENDTRY.

    result = me.

  ENDMETHOD.


  METHOD zif_abappm_pacote~manifest.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        result = get_agent(
          iv_url         = mv_registry
          iv_abbreviated = iv_abbreviated )->request( |{ mv_registry }/{ ms_pacote-name }/{ iv_version }| )->cdata( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_pacote=>raise_with_text( lx_error ).
    ENDTRY.

    check_result( result ).

  ENDMETHOD.


  METHOD zif_abappm_pacote~packument.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        result = get_agent( mv_registry )->request( |{ mv_registry }/{ ms_pacote-name }| )->cdata( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_pacote=>raise_with_text( lx_error ).
    ENDTRY.

    check_result( result ).

    ms_pacote-packument = result.

  ENDMETHOD.


  METHOD zif_abappm_pacote~save.

    DATA lx_error TYPE REF TO zcx_abappm_persist_apm.

    TRY.
        gi_persist->save(
          iv_key   = ms_pacote-key
          iv_value = zif_abappm_pacote~get( ) ).
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abappm_pacote=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_pacote~set.
    ms_pacote-packument = iv_packument.
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_pacote~tarball.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        " TODO: Error check (HTTP status)
        result = get_agent( mv_registry )->request( iv_filename )->data( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_pacote=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
