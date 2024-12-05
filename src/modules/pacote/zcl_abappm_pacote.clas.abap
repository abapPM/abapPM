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
        zcx_abappm_error.

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
        zcx_abappm_error.

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

    CLASS-METHODS convert_json_to_packument
      IMPORTING
        !iv_json      TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS convert_packument_to_json
      IMPORTING
        !is_packument TYPE zif_abappm_pacote=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

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
        VALUE(result)   TYPE REF TO zif_abappm_http_agent
      RAISING
        zcx_abappm_error.

    METHODS request
      IMPORTING
        !iv_url         TYPE string
        !iv_abbreviated TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE REF TO zif_abappm_http_response
      RAISING
        zcx_abappm_error.

    METHODS check_result
      IMPORTING
        !iv_json TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS sort_packument
      IMPORTING
        !is_packument TYPE zif_abappm_pacote=>ty_packument
      RETURNING
        VALUE(result) TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_pacote IMPLEMENTATION.


  METHOD check_result.

    DATA:
      lv_error TYPE string,
      lx_error TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        lv_error = zcl_abappm_ajson=>parse( iv_json )->get_string( '/error' ).
      CATCH zcx_abappm_ajson_error INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    IF lv_error IS NOT INITIAL.
      zcx_abappm_error=>raise( lv_error ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
    gi_persist = zcl_abappm_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

*    IF iv_registry NP 'https://*.abappm.com'.
*      zcx_error=>raise( 'Only works with abappm.com' ).
*    ENDIF.

    mv_registry = iv_registry.

    ms_pacote-key            = get_packument_key( iv_name ).
    ms_pacote-name           = escape(
                                 val    = iv_name
                                 format = cl_abap_format=>e_url_full ).

    IF iv_packument IS NOT INITIAL.
      ms_pacote-json      = iv_packument.
      ms_pacote-packument = convert_json_to_packument( ms_pacote-json ).
    ELSE.
      TRY.
          zif_abappm_pacote~load( ).
        CATCH zcx_abappm_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD convert_json_to_packument.

    TYPES:
      " Copy of schema but without object attributes (which need to be converted to tables)
      BEGIN OF ty_packument_partial,
        name        TYPE string,
        description TYPE string,
        readme      TYPE string,
        homepage    TYPE string,
        BEGIN OF bugs,
          url   TYPE zif_abappm_package_json_types=>ty_uri,
          email TYPE zif_abappm_package_json_types=>ty_email,
        END OF bugs,
        license     TYPE string,
        keywords    TYPE string_table,
        author      TYPE zif_abappm_package_json_types=>ty_person,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE zif_abappm_package_json_types=>ty_uri,
          directory TYPE string,
        END OF repository,
        _id         TYPE string,
        _rev        TYPE string,
        access      TYPE string,
      END OF ty_packument_partial.

    DATA:
      li_json         TYPE REF TO zif_abappm_ajson,
      li_version      TYPE REF TO zif_abappm_ajson,
      ls_json_partial TYPE ty_packument_partial,
      lv_key          TYPE string,
      ls_generic      TYPE zif_abappm_package_json_types=>ty_generic,
      ls_time         TYPE zif_abappm_package_json_types=>ty_time,
      ls_person       TYPE zif_abappm_package_json_types=>ty_person,
      ls_user         TYPE zif_abappm_package_json_types=>ty_user,
      ls_package_json TYPE zif_abappm_package_json_types=>ty_package_json,
      ls_version      TYPE zif_abappm_pacote=>ty_version,
      ls_attachment   TYPE zif_abappm_pacote=>ty_attachment,
      ls_json         TYPE zif_abappm_pacote=>ty_packument,
      lx_error        TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        li_json = zcl_abappm_ajson=>parse( iv_json ).
        li_json->to_abap(
          EXPORTING
            iv_corresponding = abap_true
          IMPORTING
            ev_container     = ls_json_partial ).

        MOVE-CORRESPONDING ls_json_partial TO ls_json.

        " Transpose dist-tags, times, users, versions...
        LOOP AT li_json->members( '/dist-tags' ) INTO ls_generic-key.
          ls_generic-value = li_json->get( '/dist-tags/' && ls_generic-key ).
          INSERT ls_generic INTO TABLE ls_json-dist_tags.
        ENDLOOP.

        LOOP AT li_json->members( '/time' ) INTO ls_time-key.
          ls_time-timestamp = li_json->get_timestamp( '/time/' && ls_time-key ).
          INSERT ls_time INTO TABLE ls_json-time.
        ENDLOOP.

        LOOP AT li_json->members( '/maintainers' ) INTO lv_key.
          ls_person-name   = li_json->get( '/maintainers/' && lv_key && '/name' ).
          ls_person-email  = li_json->get( '/maintainers/' && lv_key && '/email' ).
          ls_person-url    = li_json->get( '/maintainers/' && lv_key && '/url' ).
          ls_person-avatar = li_json->get( '/maintainers/' && lv_key && '/avatar' ).
          INSERT ls_person INTO TABLE ls_json-maintainers.
        ENDLOOP.

        LOOP AT li_json->members( '/users' ) INTO ls_user-name.
          ls_user-value = li_json->get( '/users/' && ls_user-name ).
          INSERT ls_user INTO TABLE ls_json-users.
        ENDLOOP.

        LOOP AT li_json->members( '/_attachments' ) INTO ls_attachment-key.
          ls_attachment-tarball-content_type = li_json->get( '/_attachments/' && ls_attachment-key && '/content_type' ).
          ls_attachment-tarball-data         = li_json->get( '/_attachments/' && ls_attachment-key && '/data' ).
          ls_attachment-tarball-length       = li_json->get_integer( '/_attachments/' && ls_attachment-key && '/length' ).
          INSERT ls_attachment INTO TABLE ls_json-__attachments.
        ENDLOOP.

        LOOP AT li_json->members( '/versions' ) INTO ls_version-key.
          li_version = li_json->slice( '/versions/' && ls_version-key ).
          ls_version-version = zcl_abappm_package_json=>convert_json_to_manifest( li_version->stringify( ) ).
          INSERT ls_version INTO TABLE ls_json-versions.
        ENDLOOP.

        " TODO: validation of packument

        result = sort_packument( ls_json ).

      CATCH zcx_abappm_ajson_error INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_packument_to_json.

    DATA:
      li_json       TYPE REF TO zif_abappm_ajson,
      li_version    TYPE REF TO zif_abappm_ajson,
      lv_version    TYPE string,
      ls_generic    TYPE zif_abappm_package_json_types=>ty_generic,
      ls_time       TYPE zif_abappm_package_json_types=>ty_time,
      ls_person     TYPE zif_abappm_package_json_types=>ty_person,
      ls_user       TYPE zif_abappm_package_json_types=>ty_user,
      ls_version    TYPE zif_abappm_pacote=>ty_version,
      ls_attachment TYPE zif_abappm_pacote=>ty_attachment,
      lx_error      TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        li_json = zcl_abappm_ajson=>new( )->keep_item_order( )->set(
          iv_path = '/'
          iv_val  = is_packument ).

        li_json = li_json->map( zcl_abappm_ajson_mapping=>create_to_camel_case( ) ).

        " Transpose dist-tags, times, users, versions...
        li_json->setx( '/dist-tags:{ }' ).
        LOOP AT is_packument-dist_tags INTO ls_generic.
          li_json->set(
            iv_path = 'dist-tags/' && ls_generic-key
            iv_val  = ls_generic-value ).
        ENDLOOP.

        li_json->setx( '/time:{ }' ).
        LOOP AT is_packument-time INTO ls_time.
          li_json->set_timestamp(
            iv_path = 'time/' && ls_time-key
            iv_val  = ls_time-timestamp ).
        ENDLOOP.

        li_json->setx( '/maintainers:{ }' ).
        LOOP AT is_packument-maintainers INTO ls_person.
          li_json->set(
            iv_path = 'maintainers/name'
            iv_val  = ls_person-name ).
          li_json->set(
            iv_path = 'maintainers/email'
            iv_val  = ls_person-email ).
          li_json->set(
            iv_path = 'maintainers/url'
            iv_val  = ls_person-url ).
          li_json->set(
            iv_path = 'maintainers/avatar'
            iv_val  = ls_person-avatar ).
        ENDLOOP.

        li_json->setx( '/users:{ }' ).
        LOOP AT is_packument-users INTO ls_user.
          li_json->set(
            iv_path = 'users/' && ls_user-name
            iv_val  = ls_user-value ).
        ENDLOOP.

        li_json->setx( '/_attachments:{ }' ).
        LOOP AT is_packument-__attachments INTO ls_attachment.
          li_json->set(
            iv_path = '_attachments/' && ls_attachment-key && '/content_type'
            iv_val  = ls_attachment-tarball-content_type ).
          li_json->set(
            iv_path = '_attachments/' && ls_attachment-key && '/data'
            iv_val  = ls_attachment-tarball-data ).
          li_json->set_integer(
            iv_path = '_attachments/' && ls_attachment-key && '/length'
            iv_val  = ls_attachment-tarball-length ).
        ENDLOOP.

        li_json->setx( '/versions:{ }' ).
        LOOP AT is_packument-versions INTO ls_version.
          lv_version = zcl_abappm_package_json=>convert_manifest_to_json( is_manifest = ls_version-version ).

          li_version = zcl_abappm_ajson=>parse( lv_version )->keep_item_order( ).

          li_json->set(
            iv_path = 'versions/' && ls_version-key
            iv_val  = li_version ).
        ENDLOOP.

        result = li_json->stringify( 2 ).
      CATCH zcx_abappm_ajson_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
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

    result = zcl_abappm_http_agent=>create( ).

    IF iv_abbreviated = abap_true.
      result->global_headers( )->set(
        iv_key = 'Accept'
        iv_val = 'application/vnd.npm.install-v1+json' ).
    ELSE.
      result->global_headers( )->set(
        iv_key = 'Accept'
        iv_val = 'application/json' ).
    ENDIF.

    " Login manager requires git-like URL so we add some dummy repo
    lv_url = iv_url && '/apm/apm.git'.

    " Get auth token from URL
    IF zcl_abappm_http_login_manager=>get( lv_url ) IS NOT INITIAL.
      result->global_headers( )->set(
        iv_key = 'Authorization'
        iv_val = zcl_abappm_http_login_manager=>get( lv_url ) ).
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


  METHOD request.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        result = get_agent( mv_registry )->request( iv_url ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD sort_packument.
    result = is_packument.
    SORT result-dist_tags BY key.
    SORT result-time BY key.
    SORT result-maintainers BY name.
    SORT result-users BY name.
    SORT result-versions BY key.
    SORT result-__attachments BY key.
  ENDMETHOD.


  METHOD zif_abappm_pacote~delete.
    gi_persist->delete( ms_pacote-key ).
  ENDMETHOD.


  METHOD zif_abappm_pacote~exists.
    TRY.
        gi_persist->load( ms_pacote-key ).
        result = abap_true.
      CATCH zcx_abappm_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abappm_pacote~get.
    result = ms_pacote-packument.
  ENDMETHOD.


  METHOD zif_abappm_pacote~get_json.
    result = ms_pacote-json.
  ENDMETHOD.


  METHOD zif_abappm_pacote~load.
    ms_pacote-json      = gi_persist->load( ms_pacote-key )-value.
    ms_pacote-packument = convert_json_to_packument( ms_pacote-json ).
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_pacote~manifest.
    result = request(
      iv_url         = |{ mv_registry }/{ ms_pacote-name }/{ iv_version }|
      iv_abbreviated = iv_abbreviated )->cdata( ).
    check_result( result ).
  ENDMETHOD.


  METHOD zif_abappm_pacote~packument.
    result = request( |{ mv_registry }/{ ms_pacote-name }| )->cdata( ).
    check_result( result ).
    zif_abappm_pacote~set_json( result ).
  ENDMETHOD.


  METHOD zif_abappm_pacote~save.
    gi_persist->save(
      iv_key   = ms_pacote-key
      iv_value = ms_pacote-json ).
  ENDMETHOD.


  METHOD zif_abappm_pacote~set.
    ms_pacote-packument = is_packument.
    ms_pacote-json      = convert_packument_to_json( is_packument ).
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_pacote~set_json.
    ms_pacote-json      = iv_json.
    ms_pacote-packument = convert_json_to_packument( iv_json ).
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_pacote~tarball.
    " TODO: Error check (HTTP status)
    result = request( iv_filename )->data( ).
  ENDMETHOD.
ENDCLASS.
