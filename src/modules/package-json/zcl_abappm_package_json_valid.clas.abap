CLASS ZCL_ABAPPM_PACKAGE_JSON_VALID DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Package JSON Validator
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS check
      IMPORTING
        !is_manifest TYPE ZIF_ABAPPM_PACKAGE_JSON_TYPES=>TY_MANIFEST
      RETURNING
        VALUE(result)    TYPE string_table.

    CLASS-METHODS is_valid_package_type
      IMPORTING
        !iv_type      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_sap_package
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_name
      IMPORTING
        !iv_name      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_scoped_name
      IMPORTING
        !iv_name      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_version
      IMPORTING
        !iv_version   TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_version_range
      IMPORTING
        !iv_range     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_email
      IMPORTING
        !iv_email     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_url
      IMPORTING
        !iv_url       TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_engine
      IMPORTING
        !iv_engine    TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_os
      IMPORTING
        !iv_os        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_cpu
      IMPORTING
        !iv_cpu       TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_db
      IMPORTING
        !iv_db        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPPM_PACKAGE_JSON_VALID IMPLEMENTATION.


  METHOD check.

    APPEND LINES OF lcl_validate=>validate_single_values( is_manifest ) TO result.

    APPEND LINES OF lcl_validate=>validate_arrays( is_manifest ) TO result.

    APPEND LINES OF lcl_validate=>validate_persons( is_manifest ) TO result.

    APPEND LINES OF lcl_validate=>validate_dependencies( is_manifest ) TO result.

  ENDMETHOD.


  METHOD is_scoped_name.
    result = boolc( is_valid_name( iv_name ) AND iv_name(1) = '@' AND iv_name CS '/' ).
  ENDMETHOD.


  METHOD is_valid_cpu.

    DATA(lv_cpu) = iv_cpu.

    SHIFT lv_cpu LEFT DELETING LEADING '!'.

    result = boolc(
      lv_cpu IS INITIAL OR
      lv_cpu = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_CPU-X86_64 OR
      lv_cpu = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_CPU-POWER_PC OR
      lv_cpu = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_CPU-SPARC ).

  ENDMETHOD.


  METHOD is_valid_db.

    DATA(lv_db) = iv_db.

    SHIFT lv_db LEFT DELETING LEADING '!'.

    result = boolc(
      lv_db IS INITIAL OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-DB2 OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-DB400 OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-DB6 OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-HDB OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-INFORMIX OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-MSSQL OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-ORACLE OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-SAP_DB OR
      lv_db = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_DB-SYBASE ).

  ENDMETHOD.


  METHOD is_valid_email.

    " Email address validation (RFC 5322)
    CONSTANTS lc_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF iv_email IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX lc_email_regex IN iv_email.
      result = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_engine.

    result = boolc(
      iv_engine IS INITIAL OR
      iv_engine = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_ENGINE-ABAP OR
      iv_engine = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_ENGINE-APM ).

  ENDMETHOD.


  METHOD is_valid_name.
    " https://www.npmjs.com/package/validate-npm-package-name
    IF strlen( iv_name )
      BETWEEN ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_PACKAGE_NAME-MIN_LENGTH
          AND ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_PACKAGE_NAME-MAX_LENGTH.

      FIND REGEX ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_PACKAGE_NAME-REGEX IN iv_name RESPECTING CASE.
      result = boolc( sy-subrc =  0 ).
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD is_valid_os.

    DATA(lv_os) = iv_os.

    SHIFT lv_os LEFT DELETING LEADING '!'.

    result = boolc(
      lv_os IS INITIAL OR
      lv_os = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_OS-AIX OR
      lv_os = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_OS-HP_UX OR
      lv_os = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_OS-LINUX OR
      lv_os = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_OS-MS_WINDOWS OR
      lv_os = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_OS-OS_390 OR
      lv_os = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_OS-OS_400 OR
      lv_os = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_OS-SOLARIS ).

  ENDMETHOD.


  METHOD is_valid_package_type.

    result = boolc(
      iv_type IS INITIAL OR
      iv_type = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_PACKAGE_TYPE-COMMON_ABAP OR
      iv_type = ZIF_ABAPPM_PACKAGE_JSON_TYPES=>C_PACKAGE_TYPE-MODULE ).

  ENDMETHOD.


  METHOD is_valid_sap_package.

    DATA lv_package_type TYPE c LENGTH 1.

    " Limit to local, customer, namespaced, and partner packages (see type-pool TPAK)
    CALL METHOD cl_package_helper=>check_package_name
      EXPORTING
        i_package_name = iv_package
      IMPORTING
        e_package_type = lv_package_type
      EXCEPTIONS
        OTHERS         = 1.

    result = boolc( sy-subrc =  0 AND lv_package_type CA '$ZNJ' ).

    " Workaround for missing validation of empty namespace
    IF result = abap_true AND iv_package CP '//*'.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_url.

    " Basic URL validation
    CONSTANTS lc_url_regex TYPE string VALUE 'https?://.+\..+'.

    IF iv_url IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX lc_url_regex IN iv_url.
      result = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_version.
    " Check if it is a semantic version
    TRY.
        zcl_semver=>create( iv_version ).
        result = abap_true.
      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD is_valid_version_range.
    " Check if it is a semantic version range
    TRY.
        zcl_semver_range=>create( iv_range ).
        result = abap_true.
      CATCH zcx_semver_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
