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
        !package      TYPE devclass
        !markdown     TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_readme
      RAISING
        zcx_abappm_error.

    CLASS-METHODS injector
      IMPORTING
        !package TYPE devclass
        !mock    TYPE REF TO zif_abappm_readme.

    METHODS constructor
      IMPORTING
        !package  TYPE devclass
        !markdown TYPE string OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_package_key
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_persist_apm=>ty_key.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !key          TYPE zif_abappm_persist_apm=>ty_key
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
      db_persist TYPE REF TO zif_abappm_persist_apm,
      instances  TYPE ty_instances.

    DATA:
      package TYPE devclass,
      readme  TYPE zif_abappm_readme=>ty_readme.

ENDCLASS.



CLASS zcl_abappm_readme IMPLEMENTATION.


  METHOD class_constructor.

    db_persist = zcl_abappm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

*    IF zcl_readme_valid=>is_valid_sap_package( package ) = abap_false
*      RAISE EXCEPTION TYPE zcx_error_text EXPORTING text = |Invalid package: { package }|
*    ENDIF

    me->package     = package.
    readme-key      = get_package_key( package ).
    readme-markdown = markdown.

    TRY.
        zif_abappm_readme~load( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>)
      WITH TABLE KEY package = package.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      result = NEW zcl_abappm_readme(
        package  = package
        markdown = markdown ).

      DATA(instance) = VALUE ty_instance(
        package  = package
        instance = result ).

      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_from_key.

    SPLIT key AT ':' INTO DATA(prefix) result DATA(suffix) ##NEEDED.
    result = to_upper( result ).

  ENDMETHOD.


  METHOD get_package_key.

    result = |{ zif_abappm_persist_apm=>c_key_type-package }:{ package }:|
          && |{ zif_abappm_persist_apm=>c_key_extra-package_readme }|.

  ENDMETHOD.


  METHOD injector.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>)
      WITH TABLE KEY package = package.
    IF sy-subrc = 0.
      <instance>-instance = mock.
    ELSE.
      DATA(instance) = VALUE ty_instance(
        package  = package
        instance = mock ).

      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_readme~delete.

    db_persist->delete( readme-key ).

  ENDMETHOD.


  METHOD zif_abappm_readme~exists.

    TRY.
        db_persist->load( readme-key ).
        result = abap_true.
      CATCH zcx_abappm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abappm_readme~get.

    result = readme-markdown.

  ENDMETHOD.


  METHOD zif_abappm_readme~load.

    readme-markdown = db_persist->load( readme-key )-value.
    result = me.

  ENDMETHOD.


  METHOD zif_abappm_readme~save.

    db_persist->save(
      key   = readme-key
      value = zif_abappm_readme~get( ) ).

  ENDMETHOD.


  METHOD zif_abappm_readme~set.

    readme-markdown = markdown.
    result = me.

  ENDMETHOD.
ENDCLASS.
