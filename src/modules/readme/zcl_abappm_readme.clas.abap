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
        !package      TYPE devclass
        !markdown     TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_README
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS injector
      IMPORTING
        !package TYPE devclass
        !mock    TYPE REF TO ZIF_ABAPPM_README.

    METHODS constructor
      IMPORTING
        !package  TYPE devclass
        !markdown TYPE string OPTIONAL
      RAISING
        ZCX_ABAPPM_ERROR.

    CLASS-METHODS get_package_key
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY.

    CLASS-METHODS get_package_from_key
      IMPORTING
        !key          TYPE ZIF_ABAPPM_PERSIST_APM=>TY_KEY
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
      db_persist TYPE REF TO ZIF_ABAPPM_PERSIST_APM,
      instances  TYPE ty_instances.

    DATA:
      package TYPE devclass,
      readme  TYPE ZIF_ABAPPM_README=>TY_README.

ENDCLASS.



CLASS ZCL_ABAPPM_README IMPLEMENTATION.


  METHOD class_constructor.

    db_persist = ZCL_ABAPPM_PERSIST_APM=>GET_INSTANCE( ).

  ENDMETHOD.


  METHOD constructor.

*    IF zcl_readme_valid=>is_valid_sap_package( package ) = abap_false.
*      zcx_error=>raise( |Invalid package: { package }| ).
*    ENDIF.

    me->package     = package.
    readme-key      = get_package_key( package ).
    readme-markdown = markdown.

    TRY.
        ZIF_ABAPPM_README~LOAD( ).
      CATCH ZCX_ABAPPM_ERROR ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD factory.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>)
      WITH TABLE KEY package = package.
    IF sy-subrc = 0.
      result = <instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE ZCL_ABAPPM_README
        EXPORTING
          package  = package
          markdown = markdown.

      DATA(instance) = VALUE ty_instance(
        package  = package
        instance = result ).

      INSERT instance INTO TABLE instances.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_from_key.

    SPLIT key AT ':' INTO DATA(prefix) result DATA(suffix).
    result = to_upper( result ).

  ENDMETHOD.


  METHOD get_package_key.

    result = |{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-PACKAGE }:{ package }:{ ZIF_ABAPPM_PERSIST_APM=>C_KEY_EXTRA-PACKAGE_README }|.

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


  METHOD ZIF_ABAPPM_README~DELETE.

    db_persist->delete( readme-key ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~EXISTS.

    TRY.
        db_persist->load( readme-key ).
        result = abap_true.
      CATCH ZCX_ABAPPM_ERROR.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~GET.

    result = readme-markdown.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~LOAD.

    readme-markdown = db_persist->load( readme-key )-value.
    result = me.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~SAVE.

    db_persist->save(
      key   = readme-key
      value = ZIF_ABAPPM_README~GET( ) ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_README~SET.

    readme-markdown = markdown.
    result = me.

  ENDMETHOD.
ENDCLASS.
