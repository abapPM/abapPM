CLASS /apmg/cl_apm_json DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm JSON Helpers
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !json         TYPE string
        !path         TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS to_string
      IMPORTING
        !value        TYPE any
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS to_abap
      IMPORTING
        !json   TYPE string
      CHANGING
        !result TYPE any
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS validate_and_prettify
      IMPORTING
        !json         TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_json IMPLEMENTATION.


  METHOD get.

    TRY.
        result = /apmg/cl_apm_ajson=>parse( json )->get_string( path ).
      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD to_abap.

    TRY.
        DATA(ajson) = /apmg/cl_apm_ajson=>parse( json
          )->map( /apmg/cl_apm_ajson_extensions=>from_camel_case_underscore( )
          )->to_abap_corresponding_only( ).

        ajson->to_abap( IMPORTING ev_container = result ).

      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD to_string.

    TRY.
        result = /apmg/cl_apm_ajson=>new( )->set(
          iv_path = '/'
          iv_val  = value )->stringify( ).
      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD validate_and_prettify.

    TRY.
        result = /apmg/cl_apm_ajson=>parse(
          iv_json            = json
          iv_keep_item_order = abap_true )->stringify( 2 ).
      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
