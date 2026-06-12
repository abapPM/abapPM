CLASS /apmg/cl_apm_registry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Registry
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS fetch
      IMPORTING
        !registry     TYPE string
        !url          TYPE string
        !command      TYPE string OPTIONAL
        !auth_type    TYPE string OPTIONAL
        !username     TYPE string OPTIONAL
        !password     TYPE string OPTIONAL
        !method       TYPE string DEFAULT /apmg/if_apm_http_agent=>c_method-get
        !payload      TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_http_response
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_packument
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !write        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_manifest
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !version      TYPE string
        !write        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_tarball
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !tarball      TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_latest_version
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_versions_from_packument
      IMPORTING
        packument     TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_package_from_name
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_scope_from_name
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_name_from_scope_package
      IMPORTING
        scope         TYPE string OPTIONAL
        package       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS check_response
      IMPORTING
        !response     TYPE REF TO /apmg/if_apm_http_response
        !text         TYPE string DEFAULT 'Error'
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_agent
      IMPORTING
        !host         TYPE string
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_http_agent
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_header,
        apm_command   TYPE string VALUE 'apm-command',
        apm_auth_type TYPE string VALUE 'apm-auth-type',
      END OF c_header.

    CLASS-METHODS get_error
      IMPORTING
        !response     TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_registry IMPLEMENTATION.


  METHOD check_response.

    IF response->is_ok( ) = abap_false.
      result = |{ text } ({ response->code( ) }): { get_error( response->error( ) ) }|.
    ENDIF.

  ENDMETHOD.


  METHOD fetch.

    DATA(headers) = /apmg/cl_apm_string_map=>create( ).

    " apm Headers
    IF command IS NOT INITIAL.
      headers->set(
        iv_key = c_header-apm_command
        iv_val = command ).
    ENDIF.
    IF auth_type IS NOT INITIAL.
      headers->set(
        iv_key = c_header-apm_auth_type
        iv_val = auth_type ).
    ENDIF.

    IF username IS NOT INITIAL AND password IS NOT INITIAL.
* TODO!
*      /apmg/cl_apm_http_login_manage=>set(
*        host     = registry
*        username = username
*        password = password ).
    ENDIF.

    /apmg/cl_apm_trace=>cdata( |{ method } { url }\n\n{ payload }| ).

    result = get_agent( registry )->request(
      url     = url
      headers = headers
      method  = method
      payload = payload ).

    /apmg/cl_apm_trace=>cdata( |{ result->code( ) }\n\n{ result->cdata( ) }| ).

  ENDMETHOD.


  METHOD get_agent.

    " Based on host, get registry settings from user (rfcdest, username, password, proxy, ...)
    " and pass these to the agent here
    result = /apmg/cl_apm_http_agent=>create( ).

    result->global_headers( )->set(
      iv_key = /apmg/if_apm_http_agent=>c_header-accept
      iv_val = /apmg/if_apm_http_agent=>c_content_type-json ).

    result->global_headers( )->set(
      iv_key = /apmg/if_apm_http_agent=>c_header-content_type
      iv_val = /apmg/if_apm_http_agent=>c_content_type-json ).

    result->global_headers( )->set(
      iv_key = /apmg/if_apm_http_agent=>c_header-user_agent
      iv_val = /apmg/cl_apm_utils=>get_user_agent( ) ).

    " Authorization (Basic or Bearer)
    DATA(auth) = /apmg/cl_apm_http_login_manage=>get( host ).

    IF auth IS NOT INITIAL.
      result->global_headers( )->set(
        iv_key = /apmg/if_apm_http_agent=>c_header-authorization
        iv_val = auth ).
    ENDIF.

  ENDMETHOD.


  METHOD get_error.

    CHECK response IS NOT INITIAL.

    IF response(1) <> '{'.
      result = response.
    ELSE.
      result = /apmg/cl_apm_json=>get(
        json = response
        path = '/error' ).
    ENDIF.

    IF result IS NOT INITIAL.
      result = to_upper( result(1) ) && result+1.
    ENDIF.

  ENDMETHOD.


  METHOD get_latest_version.

    DATA(pacote) = /apmg/cl_apm_pacote=>factory(
      registry = registry
      name     = name ).

    " TODO!
*    DATA(tags) = pacote->get_dist_tags( ).
*
*    IF line_exists( tags[ key = 'latest' ] ).
*      result = tags[ key = 'latest' ]-value.
*    ELSE.
*      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
*        EXPORTING
*          text = |"Latest" tag is missing for package { name }. Please, open an issue on GitHub|.
*    ENDIF.

  ENDMETHOD.


  METHOD get_manifest.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(pacote) = /apmg/cl_apm_pacote=>factory(
      registry = registry
      name     = name ).

    DATA(json) = pacote->manifest(
      version = version
      write   = write ).

    result = /apmg/cl_apm_package_json=>convert_json_to_manifest( json ).

  ENDMETHOD.


  METHOD get_name_from_scope_package.

    IF scope IS INITIAL.
      result = package.
    ELSE.
      result = |{ scope }/{ package }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_from_name.

    IF name(1) = '@' AND name CS '/'.
      result = substring_after( val = name sub = '/' ).
    ELSE.
      result = name.
    ENDIF.

  ENDMETHOD.


  METHOD get_packument.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(pacote) = /apmg/cl_apm_pacote=>factory(
      registry = registry
      name     = name ).

    pacote->packument( write ).

    result = pacote->get( ).

  ENDMETHOD.


  METHOD get_scope_from_name.

    IF name(1) = '@' AND name CA '/'.
      result = substring_before( val = name sub = '/' ).
    ELSE.
      result = ''.
    ENDIF.

  ENDMETHOD.


  METHOD get_tarball.

    result = /apmg/cl_apm_pacote=>factory(
      registry = registry
      name     = name )->tarball( tarball ).

  ENDMETHOD.


  METHOD get_versions_from_packument.

    LOOP AT packument-versions ASSIGNING FIELD-SYMBOL(<version>).
      INSERT <version>-key INTO TABLE result.
    ENDLOOP.

    result = /apmg/cl_apm_semver_functions=>sort( result ).

  ENDMETHOD.
ENDCLASS.
