CLASS /apmg/cl_apm_url_params DEFINITION PUBLIC FINAL CREATE PUBLIC.

************************************************************************
* URL Query Parameters
*
* Implementation of WHATWG-URL standard
* https://url.spec.whatwg.org/#interface-urlsearchparams
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* TODO: Tests
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_param,
        key   TYPE string,
        value TYPE string,
      END OF ty_param,
      ty_params TYPE STANDARD TABLE OF ty_param WITH KEY key.

    DATA params TYPE ty_params READ-ONLY.

    CLASS-METHODS parse
      IMPORTING
        !query        TYPE string
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_url_params.

    CLASS-METHODS create
      IMPORTING
        !params       TYPE ty_params
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_url_params.

    METHODS constructor
      IMPORTING
        !params TYPE ty_params.

    METHODS append
      IMPORTING
        !key   TYPE string
        !value TYPE string.

    METHODS delete
      IMPORTING
        !key   TYPE string
        !value TYPE string OPTIONAL.

    METHODS get
      IMPORTING
        !key          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_all
      IMPORTING
        !key          TYPE string
      RETURNING
        VALUE(result) TYPE ty_params.

    METHODS has
      IMPORTING
        !key          TYPE string
        !value        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS set
      IMPORTING
        !key   TYPE string
        !value TYPE string.

    METHODS sort.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_url_params IMPLEMENTATION.


  METHOD append.
    APPEND INITIAL LINE TO params ASSIGNING FIELD-SYMBOL(<param>).
    <param>-key   = key.
    <param>-value = value.
  ENDMETHOD.


  METHOD constructor.
    me->params = params.
  ENDMETHOD.


  METHOD create.
    result = NEW #( params = params ).
  ENDMETHOD.


  METHOD delete.
    IF value IS SUPPLIED.
      DELETE params WHERE key = key AND value = value.
    ELSE.
      DELETE params WHERE key = key.
    ENDIF.
  ENDMETHOD.


  METHOD get.
    READ TABLE params WITH KEY key = key ASSIGNING FIELD-SYMBOL(<param>).
    IF sy-subrc = 0.
      result = <param>-value.
    ENDIF.
  ENDMETHOD.


  METHOD get_all.
    LOOP AT params ASSIGNING FIELD-SYMBOL(<param>) WHERE key = key.
      INSERT <param> INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.


  METHOD has.
    IF value IS SUPPLIED.
      READ TABLE params WITH KEY key = key value = value TRANSPORTING NO FIELDS ##SUBRC_OK.
    ELSE.
      READ TABLE params WITH KEY key = key TRANSPORTING NO FIELDS.
    ENDIF.
    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD parse.

    DATA:
      param  TYPE ty_param,
      params TYPE ty_params.

    SPLIT query AT '&' INTO TABLE DATA(key_values).

    LOOP AT key_values ASSIGNING FIELD-SYMBOL(<key_value>).
      CLEAR param.

      param-key = substring_before(
        val = <key_value>
        sub = '=' ).

      param-value = substring_after(
        val = <key_value>
        sub = '=' ).

      INSERT param INTO TABLE params.
    ENDLOOP.

    result = NEW #( params = params ).

  ENDMETHOD.


  METHOD set.
    delete( key ).
    append( key = key value = value ).
  ENDMETHOD.


  METHOD sort.
    SORT params.
  ENDMETHOD.
ENDCLASS.
