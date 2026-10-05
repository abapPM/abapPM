CLASS /apmg/cl_apm_abapgit_convert DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* apm abapGit Conversions
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS string_to_xstring_utf8_bom
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS xstring_to_string_utf8_bom
      IMPORTING
        !iv_xstring      TYPE xstring
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS split_string
      IMPORTING
        !iv_string      TYPE string
      RETURNING
        VALUE(rt_lines) TYPE string_table.

    CLASS-METHODS string_to_tab
      IMPORTING
        !iv_str  TYPE string
      EXPORTING
        !ev_size TYPE i
        !et_tab  TYPE STANDARD TABLE.

    CLASS-METHODS xstring_to_bintab
      IMPORTING
        !iv_xstr   TYPE xsequence
      EXPORTING
        !ev_size   TYPE i
        !et_bintab TYPE STANDARD TABLE.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_abapgit_convert IMPLEMENTATION.


  METHOD split_string.

    rt_lines = zcl_abapgit_convert=>split_string( iv_string ).

  ENDMETHOD.


  METHOD string_to_tab.

    zcl_abapgit_convert=>string_to_tab(
      EXPORTING
        iv_str  = iv_str
      IMPORTING
        ev_size = ev_size
        et_tab  = et_tab ).

  ENDMETHOD.


  METHOD string_to_xstring_utf8.

    TRY.
        rv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD string_to_xstring_utf8_bom.

    TRY.
        rv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8_bom( iv_string ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD xstring_to_bintab.

    zcl_abapgit_convert=>xstring_to_bintab(
      EXPORTING
        iv_xstr   = iv_xstr
      IMPORTING
        ev_size   = ev_size
        et_bintab = et_bintab ).

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    TRY.
        rv_string = zcl_abapgit_convert=>xstring_to_string_utf8(
          iv_data   = iv_data
          iv_length = iv_length ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD xstring_to_string_utf8_bom.

    TRY.
        rv_string = zcl_abapgit_convert=>xstring_to_string_utf8_bom( iv_xstring ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
