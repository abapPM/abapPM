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
        !registry     TYPE string
        !name         TYPE string
        !packument    TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_pacote
      RAISING
        zcx_abappm_error.

    CLASS-METHODS injector
      IMPORTING
        !name TYPE string
        !mock TYPE REF TO zif_abappm_pacote.

    METHODS constructor
      IMPORTING
        !registry  TYPE string
        !name      TYPE string
        !packument TYPE string OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_packument_key
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_persist_apm=>ty_key.

    CLASS-METHODS get_packument_from_key
      IMPORTING
        !key          TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS convert_json_to_packument
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS convert_packument_to_json
      IMPORTING
        !packument    TYPE zif_abappm_types=>ty_packument
        !is_complete  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_abbreviated_json TYPE string VALUE 'application/vnd.npm.install-v1+json'.

    TYPES:
      BEGIN OF ty_instance,
        name     TYPE string,
        instance TYPE REF TO zif_abappm_pacote,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY name.

    CLASS-DATA:
      db_persist TYPE REF TO zif_abappm_persist_apm,
      instances  TYPE ty_instances.

    DATA:
      registry TYPE string,
      pacote   TYPE zif_abappm_pacote=>ty_pacote.

    METHODS get_agent
      IMPORTING
        !url          TYPE string
        !abbreviated  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_http_agent
      RAISING
        zcx_abappm_error.

    METHODS request
      IMPORTING
        !url          TYPE string
        !abbreviated  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_http_response
      RAISING
        zcx_abappm_error.

    METHODS check_result
      IMPORTING
        !json TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS check_packument
      IMPORTING
        !packument TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS sort_packument
      IMPORTING
        !packument    TYPE zif_abappm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS write_request
      IMPORTING
        !write        TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_abappm_pacote IMPLEMENTATION.


  METHOD check_packument.

    " packument has a lot of hoisted fields but no version
    DATA(manifest) = CORRESPONDING zif_abappm_types=>ty_manifest( packument ).
    manifest-version = '1.0.0'.

    DATA(issues) = zcl_abappm_package_json_valid=>check( manifest ).

    INSERT LINES OF lcl_validate=>validate_single_values( packument ) INTO TABLE issues.
    INSERT LINES OF lcl_validate=>validate_dist_tags( packument ) INTO TABLE issues.
    INSERT LINES OF lcl_validate=>validate_times( packument ) INTO TABLE issues.
    INSERT LINES OF lcl_validate=>validate_users( packument ) INTO TABLE issues.

    IF issues IS NOT INITIAL.
      zcx_abappm_error=>raise( |Invalid packument:\n{ concat_lines_of( table = issues sep = |\n| ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_result.

    TRY.
        DATA(error_message) = zcl_abappm_ajson=>parse( json )->get_string( '/error' ).
      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    IF error_message IS NOT INITIAL.
      zcx_abappm_error=>raise( error_message ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.

    db_persist = zcl_abappm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    IF registry <> 'https://playground.abappm.com'.
      zcx_abappm_error=>raise( 'apm only works with playground.abappm.com. Stay tuned for offical registry :-)' ).
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
        icon        TYPE string,
        BEGIN OF bugs,
          url   TYPE zif_abappm_types=>ty_uri,
          email TYPE zif_abappm_types=>ty_email,
        END OF bugs,
        license     TYPE string,
        keywords    TYPE string_table,
        main        TYPE string,
        man         TYPE string_table,
        author      TYPE zif_abappm_types=>ty_person,
        BEGIN OF repository,
          type      TYPE string,
          url       TYPE zif_abappm_types=>ty_uri,
          directory TYPE string,
        END OF repository,
        _id         TYPE string,
        _rev        TYPE string,
        access      TYPE string,
      END OF ty_packument_partial.

    DATA:
      json_partial TYPE ty_packument_partial,
      generic      TYPE zif_abappm_types=>ty_generic,
      time         TYPE zif_abappm_types=>ty_time,
      person       TYPE zif_abappm_types=>ty_person,
      user         TYPE zif_abappm_types=>ty_user,
      version      TYPE zif_abappm_types=>ty_version,
      attachment   TYPE zif_abappm_types=>ty_attachment,
      packument    TYPE zif_abappm_types=>ty_packument.

    TRY.
        DATA(ajson) = zcl_abappm_ajson=>parse( json
          )->map( zcl_abappm_ajson_extensions=>from_camel_case_underscore( )
          )->to_abap_corresponding_only( ).

        ajson->to_abap( IMPORTING ev_container = json_partial ).

        packument = CORRESPONDING #( json_partial ).

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
          user-stars = ajson->get( '/users/' && user-name ).
          INSERT user INTO TABLE packument-users.
        ENDLOOP.

        LOOP AT ajson->members( '/_attachments' ) INTO attachment-key.
          attachment-tarball-content_type = ajson->get( '/_attachments/' && attachment-key && '/content_type' ).
          attachment-tarball-data         = ajson->get( '/_attachments/' && attachment-key && '/data' ).
          attachment-tarball-length       = ajson->get_integer( '/_attachments/' && attachment-key && '/length' ).
          INSERT attachment INTO TABLE packument-_attachments.
        ENDLOOP.

        LOOP AT ajson->members( '/versions' ) INTO version-key.
          DATA(ajson_version) = ajson->slice( '/versions/' && version-key ).
          " this also validates the version manifest
          version-version = zcl_abappm_package_json=>convert_json_to_manifest( ajson_version->stringify( ) ).
          INSERT version INTO TABLE packument-versions.
        ENDLOOP.

        check_packument( packument ).

        result = sort_packument( packument ).

      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_packument_to_json.

    TRY.
        DATA(ajson) = zcl_abappm_ajson=>new(
          )->keep_item_order(
          )->set(
            iv_path = '/'
            iv_val  = packument
          )->map( zcl_abappm_ajson_mapping=>create_compound_mapper(
            ii_mapper1 = zcl_abappm_ajson_mapping=>create_rename( VALUE #( ( from = 'dist_tags' to = 'dist-tags' ) ) )
            ii_mapper2 = zcl_abappm_ajson_extensions=>to_camel_case_underscore( ) ) ).

        " Transpose dist-tags, times, users, versions... from arrays to objects
        ajson->setx( '/dist-tags:{ }' ).
        LOOP AT packument-dist_tags INTO DATA(generic).
          ajson->set(
            iv_path = 'dist-tags/' && generic-key
            iv_val  = generic-value ).
        ENDLOOP.

        ajson->setx( '/time:{ }' ).
        LOOP AT packument-time INTO DATA(time).
          " TODO: Replace with long timestamp
          ajson->set_timestamp(
            iv_path = 'time/' && time-key
            iv_val  = cl_abap_tstmp=>move_to_short( time-timestamp ) ).
        ENDLOOP.

        ajson->setx( '/users:{ }' ).
        LOOP AT packument-users INTO DATA(user).
          ajson->set(
            iv_path = 'users/' && user-name
            iv_val  = user-stars ).
        ENDLOOP.

        ajson->setx( '/_attachments:{ }' ).
        LOOP AT packument-_attachments INTO DATA(attachment).
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
          DATA(version_json) = zcl_abappm_package_json=>convert_manifest_to_json(
            manifest    = <version>-version
            is_complete = is_complete ).

          DATA(ajson_version) = zcl_abappm_ajson=>parse(
            iv_json            = version_json
            iv_keep_item_order = abap_true ).

          ajson->set(
            iv_path = 'versions/' && <version>-key
            iv_val  = ajson_version ).
        ENDLOOP.

        IF is_complete = abap_false.
          ajson = ajson->filter( zcl_abappm_ajson_extensions=>filter_empty_zero_null( ) ).
        ENDIF.

        result = ajson->stringify( 2 ).
      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      result = NEW zcl_abappm_pacote(
        registry  = registry
        name      = name
        packument = packument ).

      DATA(instance) = VALUE ty_instance(
        name     = name
        instance = result ).
      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_agent.

    result = zcl_abappm_http_agent=>create( ).

    IF abbreviated = abap_true.
      result->global_headers( )->set(
        iv_key = zif_abappm_http_agent=>c_header-accept
        iv_val = c_abbreviated_json ).
    ELSE.
      result->global_headers( )->set(
        iv_key = zif_abappm_http_agent=>c_header-accept
        iv_val = zif_abappm_http_agent=>c_content_type-json ).
    ENDIF.

    DATA(host) = zcl_abappm_url=>parse( url )->components-host.

    " Get/set auth token
    DATA(auth) = zcl_abappm_http_login_manager=>get( host ).

    IF auth IS NOT INITIAL.
      result->global_headers( )->set(
        iv_key = zif_abappm_http_agent=>c_header-authorization
        iv_val = auth ).
    ENDIF.

  ENDMETHOD.


  METHOD get_packument_from_key.

    SPLIT key AT ':' INTO DATA(prefix) result DATA(suffix) ##NEEDED.
    result = to_lower( result ).

  ENDMETHOD.


  METHOD get_packument_key.

    result = |{ zif_abappm_persist_apm=>c_key_type-packument }:{ to_upper( name ) }|.

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

    IF abbreviated IS INITIAL.
      result = get_agent( registry )->request( url ).
    ELSE.
      DATA(headers) = NEW zcl_abappm_string_map( )->set(
        iv_key = 'Accept'
        iv_val = 'application/vnd.npm.install-v1+json' ).

      result = get_agent( registry )->request(
        url     = url
        headers = headers ).
    ENDIF.

  ENDMETHOD.


  METHOD sort_packument.

    result = packument.
    SORT result-dist_tags BY key.
    SORT result-time BY key.
    SORT result-maintainers BY name.
    SORT result-users BY name.
    SORT result-versions BY key.
    SORT result-_attachments BY key.
    SORT result-keywords.

  ENDMETHOD.


  METHOD write_request.

    IF write = abap_true.
      result = '?write=true'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_pacote~delete.

    db_persist->delete( pacote-key ).

  ENDMETHOD.


  METHOD zif_abappm_pacote~exists.

    TRY.
        db_persist->load( pacote-key ).

        result = abap_true.
      CATCH zcx_abappm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_pacote~get.

    result = pacote-packument.

  ENDMETHOD.


  METHOD zif_abappm_pacote~get_json.

    result = pacote-json.

  ENDMETHOD.


  METHOD zif_abappm_pacote~get_version.

    result = pacote-packument-versions[ key = version ].

  ENDMETHOD.


  METHOD zif_abappm_pacote~load.

    pacote-json      = db_persist->load( pacote-key )-value.
    pacote-packument = convert_json_to_packument( pacote-json ).

    result = me.

  ENDMETHOD.


  METHOD zif_abappm_pacote~manifest.

    result = request(
      url         = |{ registry }/{ pacote-name }/{ version }{ write_request( write ) }|
      abbreviated = abbreviated )->cdata( ).

    check_result( result ).

  ENDMETHOD.


  METHOD zif_abappm_pacote~packument.

    result = request( |{ registry }/{ pacote-name }{ write_request( write ) }| )->cdata( ).

    check_result( result ).

    zif_abappm_pacote~set_json( result ).

  ENDMETHOD.


  METHOD zif_abappm_pacote~save.

    db_persist->save(
      key   = pacote-key
      value = pacote-json ).

  ENDMETHOD.


  METHOD zif_abappm_pacote~set.

    pacote-packument = packument.
    pacote-json      = convert_packument_to_json( packument ).

    result = me.

  ENDMETHOD.


  METHOD zif_abappm_pacote~set_json.

    pacote-json      = json.
    pacote-packument = convert_json_to_packument( json ).

    result = me.

  ENDMETHOD.


  METHOD zif_abappm_pacote~tarball.

    DATA(response) = request( filename ).

    IF response->is_ok( ) = abap_false.
      check_result( response->cdata( ) ).
    ELSE.
      result = response->data( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
