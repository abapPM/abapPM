CLASS ZCL_ABAPPM_PACOTE DEFINITION
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

    INTERFACES ZIF_ABAPPM_PACOTE.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !packument    TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_PACOTE
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS injector
      IMPORTING
        !name TYPE string
        !mock TYPE REF TO ZIF_ABAPPM_PACOTE.

    METHODS constructor
      IMPORTING
        !registry  TYPE string
        !name      TYPE string
        !packument TYPE string OPTIONAL
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS get_packument_key
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY.

    CLASS-METHODS get_packument_from_key
      IMPORTING
        !key          TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS convert_json_to_packument
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS convert_packument_to_json
      IMPORTING
        !packument    TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_ERROR.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE string,
        instance TYPE REF TO ZIF_ABAPPM_PACOTE,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CLASS-DATA:
      db_persist TYPE REF TO ZIF_ABAPPM_PERSIST_APM,
      instances  TYPE ty_instances.

    DATA:
      registry TYPE string,
      pacote   TYPE ZIF_ABAPPM_PACOTE=>TY_PACOTE.

    METHODS get_agent
      IMPORTING
        !url          TYPE string
        !abbreviated  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_HTTP_AGENT
      RAISING
        ZCX_ABAPPM_ERROR.

    METHODS request
      IMPORTING
        !url          TYPE string
        !abbreviated  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_HTTP_RESPONSE
      RAISING
        ZCX_ABAPPM_ERROR.

    METHODS check_result
      IMPORTING
        !json TYPE string
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS sort_packument
      IMPORTING
        !packument    TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT
      RAISING
        ZCX_ABAPPM_ERROR.

ENDCLASS.



CLASS ZCL_ABAPPM_PACOTE IMPLEMENTATION.


  METHOD check_result.

    TRY.
        DATA(error_message) = ZCL_ABAPPM_AJSON=>PARSE( json )->get_string( '/error' ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO DATA(error).
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( error ).
    ENDTRY.

    IF error_message IS NOT INITIAL.
      ZCX_ABAPPM_ERROR=>RAISE( error_message ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
    db_persist = ZCL_ABAPPM_PERSIST_APM=>GET_INSTANCE( ).
  ENDMETHOD.


  METHOD constructor.

    IF registry <> 'https://playground.abappm.com'.
      ZCX_ABAPPM_ERROR=>RAISE( 'apm only works with playground.abappm.com. Stay tuned for offical registry :-)' ).
    ENDIF.

    me->registry = registry.

    pacote-key  = get_packument_key( name ).
    pacote-name = escape(
      val    = name
      format = cl_abap_format=>e_url_full ).

    IF packument IS NOT INITIAL.
      pacote-json      = packument.
      pacote-packument = convert_json_to_packument( pacote-json ).
    ELSE.
      TRY.
          ZIF_ABAPPM_PACOTE~LOAD( ).
        CATCH ZCX_ABAPPM_ERROR ##NO_HANDLER.
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
          url   TYPE ZIF_ABAPPM_TYPES=>TY_URI,
          email TYPE ZIF_ABAPPM_TYPES=>TY_EMAIL,
        END OF bugs,
        license     TYPE string,
        keywords    TYPE string_table,
        author      TYPE ZIF_ABAPPM_TYPES=>TY_PERSON,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE ZIF_ABAPPM_TYPES=>TY_URI,
          directory TYPE string,
        END OF repository,
        _id         TYPE string,
        _rev        TYPE string,
        access      TYPE string,
      END OF ty_packument_partial.

    DATA:
      json_partial TYPE ty_packument_partial,
      generic      TYPE ZIF_ABAPPM_TYPES=>TY_GENERIC,
      time         TYPE ZIF_ABAPPM_TYPES=>TY_TIME,
      person       TYPE ZIF_ABAPPM_TYPES=>TY_PERSON,
      user         TYPE ZIF_ABAPPM_TYPES=>TY_USER,
      package_json TYPE ZIF_ABAPPM_TYPES=>TY_PACKAGE_JSON,
      version      TYPE ZIF_ABAPPM_TYPES=>TY_VERSION,
      attachment   TYPE ZIF_ABAPPM_TYPES=>TY_ATTACHMENT,
      packument    TYPE ZIF_ABAPPM_TYPES=>TY_PACKUMENT.

    TRY.
        DATA(ajson) = ZCL_ABAPPM_AJSON=>PARSE( json
          )->map( ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_SNAKE_CASE( )
          )->to_abap_corresponding_only( ).

        ajson->to_abap( IMPORTING ev_container = json_partial ).

        MOVE-CORRESPONDING json_partial TO packument.

        " Transpose dist-tags, times, users, versions...
        LOOP AT ajson->members( '/dist-tags' ) INTO generic-key.
          generic-value = ajson->get( '/dist-tags/' && generic-key ).
          INSERT generic INTO TABLE packument-dist_tags.
        ENDLOOP.

        LOOP AT ajson->members( '/time' ) INTO time-key.
          time-timestamp = ajson->get_timestamp( '/time/' && time-key ).
          INSERT time INTO TABLE packument-time.
        ENDLOOP.

        LOOP AT ajson->members( '/maintainers' ) INTO DATA(key).
          person-name   = ajson->get( '/maintainers/' && key && '/name' ).
          person-email  = ajson->get( '/maintainers/' && key && '/email' ).
          person-url    = ajson->get( '/maintainers/' && key && '/url' ).
          person-avatar = ajson->get( '/maintainers/' && key && '/avatar' ).
          INSERT person INTO TABLE packument-maintainers.
        ENDLOOP.

        LOOP AT ajson->members( '/users' ) INTO user-name.
          user-value = ajson->get( '/users/' && user-name ).
          INSERT user INTO TABLE packument-users.
        ENDLOOP.

        LOOP AT ajson->members( '/_attachments' ) INTO attachment-key.
          attachment-tarball-content_type = ajson->get( '/_attachments/' && attachment-key && '/content_type' ).
          attachment-tarball-data         = ajson->get( '/_attachments/' && attachment-key && '/data' ).
          attachment-tarball-length       = ajson->get_integer( '/_attachments/' && attachment-key && '/length' ).
          INSERT attachment INTO TABLE packument-__attachments.
        ENDLOOP.

        LOOP AT ajson->members( '/versions' ) INTO version-key.
          DATA(ajson_version) = ajson->slice( '/versions/' && version-key ).
          version-version = ZCL_ABAPPM_PACKAGE_JSON=>CONVERT_JSON_TO_MANIFEST( ajson_version->stringify( ) ).
          INSERT version INTO TABLE packument-versions.
        ENDLOOP.

        " TODO: validation of packument

        result = sort_packument( packument ).

      CATCH ZCX_ABAPPM_AJSON_ERROR INTO DATA(error).
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_packument_to_json.

    DATA:
      generic    TYPE ZIF_ABAPPM_TYPES=>TY_GENERIC,
      time       TYPE ZIF_ABAPPM_TYPES=>TY_TIME,
      person     TYPE ZIF_ABAPPM_TYPES=>TY_PERSON,
      user       TYPE ZIF_ABAPPM_TYPES=>TY_USER,
      version    TYPE ZIF_ABAPPM_TYPES=>TY_VERSION,
      attachment TYPE ZIF_ABAPPM_TYPES=>TY_ATTACHMENT.

    TRY.
        DATA(ajson) = ZCL_ABAPPM_AJSON=>NEW(
          )->keep_item_order(
          )->set(
            iv_path = '/'
            iv_val  = packument
          )->map( ZCL_ABAPPM_AJSON_MAPPING=>CREATE_TO_CAMEL_CASE( ) ).

        " Transpose dist-tags, times, users, versions...
        ajson->delete( '/distTags' ). " created incorrectly due to underscope
        ajson->setx( '/dist-tags:{ }' ).
        LOOP AT packument-dist_tags INTO generic.
          ajson->set(
            iv_path = 'dist-tags/' && generic-key
            iv_val  = generic-value ).
        ENDLOOP.

        ajson->setx( '/time:{ }' ).
        LOOP AT packument-time INTO time.
          ajson->set_timestamp(
            iv_path = 'time/' && time-key
            iv_val  = time-timestamp ).
        ENDLOOP.

        ajson->setx( '/maintainers:{ }' ).
        LOOP AT packument-maintainers INTO person.
          ajson->set(
            iv_path = 'maintainers/name'
            iv_val  = person-name ).
          ajson->set(
            iv_path = 'maintainers/email'
            iv_val  = person-email ).
          ajson->set(
            iv_path = 'maintainers/url'
            iv_val  = person-url ).
          ajson->set(
            iv_path = 'maintainers/avatar'
            iv_val  = person-avatar ).
        ENDLOOP.

        ajson->setx( '/users:{ }' ).
        LOOP AT packument-users INTO user.
          ajson->set(
            iv_path = 'users/' && user-name
            iv_val  = user-value ).
        ENDLOOP.

        ajson->setx( '/_attachments:{ }' ).
        LOOP AT packument-__attachments INTO attachment.
          ajson->set(
            iv_path = '_attachments/' && attachment-key && '/content_type'
            iv_val  = attachment-tarball-content_type ).
          ajson->set(
            iv_path = '_attachments/' && attachment-key && '/data'
            iv_val  = attachment-tarball-data ).
          ajson->set_integer(
            iv_path = '_attachments/' && attachment-key && '/length'
            iv_val  = attachment-tarball-length ).
        ENDLOOP.

        ajson->setx( '/versions:{ }' ).
        LOOP AT packument-versions ASSIGNING FIELD-SYMBOL(<version>).
          DATA(version_json) = ZCL_ABAPPM_PACKAGE_JSON=>CONVERT_MANIFEST_TO_JSON( manifest = <version>-version ).

          DATA(ajson_version) = ZCL_ABAPPM_AJSON=>PARSE( version_json )->keep_item_order( ).

          ajson->set(
            iv_path = 'versions/' && <version>-key
            iv_val  = ajson_version ).
        ENDLOOP.

        result = ajson->stringify( 2 ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO DATA(error).
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE ZCL_ABAPPM_PACOTE
        EXPORTING
          registry  = registry
          name      = name
          packument = packument.

      DATA(instance) = VALUE ty_instance(
        name     = name
        instance = result ).
      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_agent.

    result = ZCL_ABAPPM_HTTP_AGENT=>CREATE( ).

    IF abbreviated = abap_true.
      result->global_headers( )->set(
        iv_key = 'Accept'
        iv_val = 'application/vnd.npm.install-v1+json' ).
    ELSE.
      result->global_headers( )->set(
        iv_key = 'Accept'
        iv_val = 'application/json' ).
    ENDIF.

    " Login manager requires git-like URL so we add some dummy repo
    DATA(login_url) = url && '/apm/apm.git'.

    " Get auth token from URL
    IF ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>GET( login_url ) IS NOT INITIAL.
      result->global_headers( )->set(
        iv_key = 'Authorization'
        iv_val = ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>GET( login_url ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_packument_from_key.

    SPLIT key AT ':' INTO DATA(prefix) result DATA(suffix).
    result = to_lower( result ).

  ENDMETHOD.


  METHOD get_packument_key.

    result = |{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-PACKUMENT }:{ to_upper( name ) }|.

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


  METHOD request.

    TRY.
        result = get_agent( registry )->request( url ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD sort_packument.

    result = packument.
    SORT result-dist_tags BY key.
    SORT result-time BY key.
    SORT result-maintainers BY name.
    SORT result-users BY name.
    SORT result-versions BY key.
    SORT result-__attachments BY key.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~DELETE.

    db_persist->delete( pacote-key ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~EXISTS.

    TRY.
        db_persist->load( pacote-key ).
        result = abap_true.
      CATCH ZCX_ABAPPM_ERROR.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~GET.

    result = pacote-packument.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~GET_JSON.

    result = pacote-json.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~GET_VERSION.

    READ TABLE pacote-packument-versions INTO result
      WITH TABLE KEY key = version.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~LOAD.

    pacote-json      = db_persist->load( pacote-key )-value.
    pacote-packument = convert_json_to_packument( pacote-json ).

    result = me.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~MANIFEST.

    result = request(
      url         = |{ registry }/{ pacote-name }/{ version }|
      abbreviated = abbreviated )->cdata( ).

    check_result( result ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~PACKUMENT.

    result = request( |{ registry }/{ pacote-name }| )->cdata( ).

    check_result( result ).
    ZIF_ABAPPM_PACOTE~SET_JSON( result ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~SAVE.

    db_persist->save(
      key   = pacote-key
      value = pacote-json ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~SET.

    pacote-packument = packument.
    pacote-json      = convert_packument_to_json( packument ).

    result = me.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~SET_JSON.

    pacote-json      = json.
    pacote-packument = convert_json_to_packument( json ).

    result = me.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PACOTE~TARBALL.

    " TODO: Error check (HTTP status)
    result = request( filename )->data( ).

  ENDMETHOD.
ENDCLASS.
