CLASS zcl_abappm_factory DEFINITION
  PUBLIC
  CREATE PRIVATE.

  " This is a replacement for ZCL_ABAPGIT_FACTORY
  "
  " Using ZCL_ABAPGIT_FACTORY would drag in many other dependencies
  " which are unnecessary for apm (like the abapGit Code Inspector, Staging, and UI layer).
  "
  " Note: This class will show errors when trying to activated it!  "
  " One must ignore errors like 'An instance of the class
  " "ZCL_ABAPGIT_..." cannot be created outside the class.'
  " and activate the class anyway.
  "
  " Once the class is embedded into the standalone program as a local class,
  " it will work just fine.
  PUBLIC SECTION.

    CLASS-METHODS get_tadir
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_tadir.

    CLASS-METHODS get_sap_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_sap_package.

    CLASS-METHODS get_cts_api
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_cts_api.

    CLASS-METHODS get_default_transport
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_default_transport.

    CLASS-METHODS get_environment
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_environment.

    CLASS-METHODS get_longtexts
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_longtexts.

    CLASS-METHODS get_lxe_texts
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_lxe_texts.

    CLASS-METHODS get_sap_namespace
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_sap_namespace.

    CLASS-METHODS get_sap_report
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_sap_report.

    CLASS-METHODS get_function_module
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_function_module.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_sap_package,
        package  TYPE devclass,
        instance TYPE REF TO zif_abapgit_sap_package,
      END OF ty_sap_package,
      ty_sap_packages TYPE HASHED TABLE OF ty_sap_package WITH UNIQUE KEY package.

    CLASS-DATA tadir TYPE REF TO zif_abapgit_tadir.
    CLASS-DATA sap_packages TYPE ty_sap_packages.
    CLASS-DATA cts_api TYPE REF TO zif_abapgit_cts_api.
    CLASS-DATA environment TYPE REF TO zif_abapgit_environment.
    CLASS-DATA longtext TYPE REF TO zif_abapgit_longtexts.
    CLASS-DATA lxe_texts TYPE REF TO zif_abapgit_lxe_texts.
    CLASS-DATA sap_namespace TYPE REF TO zif_abapgit_sap_namespace.
    CLASS-DATA sap_report TYPE REF TO zif_abapgit_sap_report.
    CLASS-DATA function_module TYPE REF TO zif_abapgit_function_module.
    CLASS-DATA default_transport TYPE REF TO zif_abapgit_default_transport.

ENDCLASS.



CLASS zcl_abappm_factory IMPLEMENTATION.


  METHOD get_cts_api.

    IF cts_api IS NOT BOUND.
      CREATE OBJECT cts_api TYPE zcl_abapgit_cts_api.
    ENDIF.

    result = cts_api.

  ENDMETHOD.


  METHOD get_default_transport.

    IF default_transport IS NOT BOUND.
      CREATE OBJECT default_transport TYPE zcl_abapgit_default_transport.
    ENDIF.

    result = default_transport.

  ENDMETHOD.


  METHOD get_environment.

    IF environment IS NOT BOUND.
      CREATE OBJECT environment TYPE zcl_abapgit_environment.
    ENDIF.

    result = environment.

  ENDMETHOD.


  METHOD get_function_module.

    IF function_module IS INITIAL.
      CREATE OBJECT function_module TYPE zcl_abapgit_function_module.
    ENDIF.

    result = function_module.

  ENDMETHOD.


  METHOD get_longtexts.

    IF longtext IS NOT BOUND.
      CREATE OBJECT longtext TYPE zcl_abapgit_longtexts.
    ENDIF.

    result = longtext.

  ENDMETHOD.


  METHOD get_lxe_texts.

    IF lxe_texts IS NOT BOUND.
      CREATE OBJECT lxe_texts TYPE zcl_abapgit_lxe_texts.
    ENDIF.

    result = lxe_texts.

  ENDMETHOD.


  METHOD get_sap_namespace.

    IF sap_namespace IS NOT BOUND.
      CREATE OBJECT sap_namespace TYPE zcl_abapgit_sap_namespace.
    ENDIF.

    result = sap_namespace.

  ENDMETHOD.


  METHOD get_sap_package.

    DATA sap_package TYPE ty_sap_package.

    READ TABLE sap_packages ASSIGNING FIELD-SYMBOL(<sap_package>) WITH TABLE KEY package = package.
    IF sy-subrc <> 0.
      sap_package-package = package.

      CREATE OBJECT sap_package-instance TYPE zcl_abapgit_sap_package
        EXPORTING
          iv_package = package.

      INSERT sap_package INTO TABLE sap_packages ASSIGNING <sap_package>.
    ENDIF.

    result = <sap_package>-instance.

  ENDMETHOD.


  METHOD get_sap_report.

    IF sap_report IS NOT BOUND.
      CREATE OBJECT sap_report TYPE zcl_abapgit_sap_report.
    ENDIF.

    result = sap_report.

  ENDMETHOD.


  METHOD get_tadir.

    IF tadir IS INITIAL.
      CREATE OBJECT tadir TYPE zcl_abapgit_tadir.
    ENDIF.

    result = tadir.

  ENDMETHOD.
ENDCLASS.
