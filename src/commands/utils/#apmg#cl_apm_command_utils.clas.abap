CLASS /apmg/cl_apm_command_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Command Utilities
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS fetch_registry
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

    CLASS-METHODS get_packument_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !write        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_manifest_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !version      TYPE string
        !write        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_tarball_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !tarball      TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_versions_from_packument
      IMPORTING
        packument     TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        /apmg/cx_apm_error.

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

    CLASS-METHODS get_abap_version
      RETURNING
        VALUE(result) TYPE string
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



CLASS /apmg/cl_apm_command_utils IMPLEMENTATION.


  METHOD check_response.

    IF response->is_ok( ) = abap_false.
      result = |{ text } ({ response->code( ) }): { get_error( response->error( ) ) }|.
    ENDIF.

  ENDMETHOD.


  METHOD fetch_registry.

    DATA(headers) = /apmg/cl_apm_string_map=>create( ).

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
      /apmg/cl_apm_http_login_manage=>set(
        host     = registry
        username = username
        password = password ).
    ENDIF.

    /apmg/cl_apm_trace=>cdata( |{ method } { url }\n\n{ payload }| ).

    result = get_agent( registry )->request(
      url     = url
      headers = headers
      method  = method
      payload = payload ).

    /apmg/cl_apm_trace=>cdata( |{ result->code( ) }\n\n{ result->cdata( ) }| ).

  ENDMETHOD.


  METHOD get_abap_version.

    TRY.
        result = NEW /apmg/cl_apm_semver_sap( )->sap_component_to_semver( 'SAP_BASIS' ).
      CATCH cx_abap_invalid_value INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_agent.

    result = /apmg/cl_apm_http_agent=>create( ).

    result->global_headers( )->set(
      iv_key = /apmg/if_apm_http_agent=>c_header-accept
      iv_val = /apmg/if_apm_http_agent=>c_content_type-json ).

    result->global_headers( )->set(
      iv_key = /apmg/if_apm_http_agent=>c_header-content_type
      iv_val = /apmg/if_apm_http_agent=>c_content_type-json ).

    " TODO: Add OS and DB
    result->global_headers( )->set(
      iv_key = /apmg/if_apm_http_agent=>c_header-user_agent
      iv_val = |apm/{ /apmg/if_apm_version=>c_version } abap/{ get_abap_version( ) }| ).

    DATA(url) = /apmg/cl_apm_url=>parse( host )->components.

    " Authorization token
    DATA(token) = /apmg/cl_apm_http_login_manage=>get( url-host ).

    IF token IS NOT INITIAL.
      result->global_headers( )->set(
        iv_key = /apmg/if_apm_http_agent=>c_header-authorization
        iv_val = token ).
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


  METHOD get_manifest_from_registry.

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


  METHOD get_packument_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(pacote) = /apmg/cl_apm_pacote=>factory(
      registry = registry
      name     = name ).

    pacote->packument( write ).

    result = pacote->get( ).

  ENDMETHOD.


  METHOD get_tarball_from_registry.

    result = /apmg/cl_apm_pacote=>factory(
      registry = registry
      name     = name )->tarball( tarball ).

  ENDMETHOD.


  METHOD get_versions_from_packument.

    LOOP AT packument-versions ASSIGNING FIELD-SYMBOL(<version>).
      INSERT <version>-key INTO TABLE result.
    ENDLOOP.

    DATA(semver) = NEW /apmg/cl_apm_semver_functions( ).

    result = semver->sort( result ).

  ENDMETHOD.
ENDCLASS.
