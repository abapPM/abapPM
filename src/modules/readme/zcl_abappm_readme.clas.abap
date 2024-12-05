CLASS ZCL_ABAPPM_README DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Readme
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES ZIF_ABAPPM_README.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_package   TYPE devclass
        !iv_markdown  TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_README
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS injector
      IMPORTING
        !iv_package TYPE devclass
        !ii_mock    TYPE REF TO ZIF_ABAPPM_README.

    METHODS constructor
      IMPORTING
        !iv_package  TYPE devclass
        !iv_markdown TYPE string OPTIONAL
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS get_package_key
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !iv_key       TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY
      RETURNING
        VALUE(result) TYPE devclass.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO ZIF_ABAPPM_README,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gi_persist   TYPE REF TO ZIF_ABAPPM_PERSIST_APM,
      gt_instances TYPE ty_instances.

    DATA:
      mv_package TYPE devclass,
      ms_readme  TYPE ZIF_ABAPPM_README=>TY_README.

ENDCLASS.



CLASS ZCL_ABAPPM_README IMPLEMENTATION.


  METHOD class_constructor.
    gi_persist = ZCL_ABAPPM_PERSIST_APM=>GET_INSTANCE( ).
  ENDMETHOD.


  METHOD constructor.

*    IF zcl_readme_valid=>is_valid_sap_package( iv_package ) = abap_false.
*      zcx_error=>raise( |Invalid package: { iv_package }| ).
*    ENDIF.

    mv_package         = iv_package.
    ms_readme-key      = get_package_key( mv_package ).
    ms_readme-markdown = iv_markdown.

    TRY.
        ZIF_ABAPPM_README~LOAD( ).
      CATCH ZCX_ABAPPM_ERROR ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE ZCL_ABAPPM_README
        EXPORTING
          iv_package  = iv_package
          iv_markdown = iv_markdown.

      ls_instance-package  = iv_package.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_from_key.

    DATA:
      lv_prefix TYPE string,
      lv_suffix TYPE string.

    SPLIT iv_key AT ':' INTO lv_prefix result lv_suffix.
    result = to_upper( result ).

  ENDMETHOD.


  METHOD get_package_key.
    result = |{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-PACKAGE }:{ iv_package }:{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_EXTRA-PACKAGE_README }|.
  ENDMETHOD.


  METHOD injector.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      <ls_instance>-instance = ii_mock.
    ELSE.
      ls_instance-package  = iv_package.
      ls_instance-instance = ii_mock.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~DELETE.
    gi_persist->delete( ms_readme-key ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~EXISTS.
    TRY.
        gi_persist->load( ms_readme-key ).
        result = abap_true.
      CATCH ZCX_ABAPPM_ERROR.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~GET.
    result = ms_readme-markdown.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~LOAD.
    ms_readme-markdown = gi_persist->load( ms_readme-key )-value.
    result = me.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~SAVE.
    gi_persist->save(
      iv_key   = ms_readme-key
      iv_value = ZIF_ABAPPM_README~GET( ) ).
  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~SET.
    ms_readme-markdown = iv_markdown.
    result = me.
  ENDMETHOD.
ENDCLASS.
