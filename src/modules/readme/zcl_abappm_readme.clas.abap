CLASS zcl_abappm_readme DEFINITION
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

    INTERFACES zif_abappm_readme.

    CLASS-METHODS class_constructor.

    CLASS-METHODS factory
      IMPORTING
        !iv_package   TYPE devclass
        !iv_markdown  TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_readme
      RAISING
        zcx_abappm_error.

    CLASS-METHODS injector
      IMPORTING
        !iv_package TYPE devclass
        !ii_mock    TYPE REF TO zif_abappm_readme.

    METHODS constructor
      IMPORTING
        !iv_package  TYPE devclass
        !iv_markdown TYPE string OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_package_key
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_persist_apm=>ty_key.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !iv_key       TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE devclass.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO zif_abappm_readme,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gi_persist   TYPE REF TO zif_abappm_persist_apm,
      gt_instances TYPE ty_instances.

    DATA:
      mv_package TYPE devclass,
      ms_readme  TYPE zif_abappm_readme=>ty_readme.

ENDCLASS.



CLASS zcl_abappm_readme IMPLEMENTATION.


  METHOD class_constructor.
    gi_persist = zcl_abappm_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

*    IF zcl_readme_valid=>is_valid_sap_package( iv_package ) = abap_false.
*      zcx_error=>raise( |Invalid package: { iv_package }| ).
*    ENDIF.

    mv_package         = iv_package.
    ms_readme-key      = get_package_key( mv_package ).
    ms_readme-markdown = iv_markdown.

    TRY.
        zif_abappm_readme~load( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE zcl_abappm_readme
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
    result = |{ zif_abappm_persist_apm=>c_key_type-package }:{ iv_package }:{ zif_abappm_persist_apm=>c_key_extra-package_readme }|.
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


  METHOD zif_abappm_readme~delete.
    gi_persist->delete( ms_readme-key ).
  ENDMETHOD.


  METHOD zif_abappm_readme~exists.
    TRY.
        gi_persist->load( ms_readme-key ).
        result = abap_true.
      CATCH zcx_abappm_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abappm_readme~get.
    result = ms_readme-markdown.
  ENDMETHOD.


  METHOD zif_abappm_readme~load.
    ms_readme-markdown = gi_persist->load( ms_readme-key )-value.
    result = me.
  ENDMETHOD.


  METHOD zif_abappm_readme~save.
    gi_persist->save(
      iv_key   = ms_readme-key
      iv_value = zif_abappm_readme~get( ) ).
  ENDMETHOD.


  METHOD zif_abappm_readme~set.
    ms_readme-markdown = iv_markdown.
    result = me.
  ENDMETHOD.
ENDCLASS.
