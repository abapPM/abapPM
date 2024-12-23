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
        !manifest     TYPE ZIF_ABAPPM_TYPES=>TY_MANIFEST
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS is_valid_package_type
      IMPORTING
        !type         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_sap_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_name
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_scoped_name
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_version
      IMPORTING
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_version_range
      IMPORTING
        !range        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_email
      IMPORTING
        !email        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_url
      IMPORTING
        !url          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_engine
      IMPORTING
        !engine       TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_os
      IMPORTING
        !os           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_cpu
      IMPORTING
        !cpu          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_db
      IMPORTING
        !db           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPPM_PACKAGE_JSON_VALID IMPLEMENTATION.


  METHOD check.

    APPEND LINES OF lcl_validate=>validate_single_values( manifest ) TO result.

    APPEND LINES OF lcl_validate=>validate_arrays( manifest ) TO result.

    APPEND LINES OF lcl_validate=>validate_persons( manifest ) TO result.

    APPEND LINES OF lcl_validate=>validate_dependencies( manifest ) TO result.

  ENDMETHOD.


  METHOD is_scoped_name.

    result = boolc( is_valid_name( name ) AND name(1) = '@' AND name CS '/' ).

  ENDMETHOD.


  METHOD is_valid_cpu.

    DATA(cpu_val) = cpu.

    SHIFT cpu_val LEFT DELETING LEADING '!'.

    result = boolc(
      cpu_val IS INITIAL OR
      cpu_val = ZIF_ABAPPM_TYPES=>C_CPU-X86_64 OR
      cpu_val = ZIF_ABAPPM_TYPES=>C_CPU-POWER_PC OR
      cpu_val = ZIF_ABAPPM_TYPES=>C_CPU-SPARC ).

  ENDMETHOD.


  METHOD is_valid_db.

    DATA(db_val) = db.

    SHIFT db_val LEFT DELETING LEADING '!'.

    result = boolc(
      db_val IS INITIAL OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-DB2 OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-DB400 OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-DB6 OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-HDB OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-INFORMIX OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-MSSQL OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-ORACLE OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-SAP_DB OR
      db_val = ZIF_ABAPPM_TYPES=>C_DB-SYBASE ).

  ENDMETHOD.


  METHOD is_valid_email.

    " Email address validation (RFC 5322)
    CONSTANTS c_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF email IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX c_email_regex IN email.
      result = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_engine.

    result = boolc(
      engine IS INITIAL OR
      engine = ZIF_ABAPPM_TYPES=>C_ENGINE-ABAP OR
      engine = ZIF_ABAPPM_TYPES=>C_ENGINE-APM ).

  ENDMETHOD.


  METHOD is_valid_name.

    " https://www.npmjs.com/package/validate-npm-package-name
    IF strlen( name )
      BETWEEN ZIF_ABAPPM_TYPES=>C_PACKAGE_NAME-MIN_LENGTH
          AND ZIF_ABAPPM_TYPES=>C_PACKAGE_NAME-MAX_LENGTH.

      FIND REGEX ZIF_ABAPPM_TYPES=>C_PACKAGE_NAME-REGEX IN name RESPECTING CASE.
      result = boolc( sy-subrc =  0 ).
    ELSE.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_os.

    DATA(os_val) = os.

    SHIFT os_val LEFT DELETING LEADING '!'.

    result = boolc(
      os_val IS INITIAL OR
      os_val = ZIF_ABAPPM_TYPES=>C_OS-AIX OR
      os_val = ZIF_ABAPPM_TYPES=>C_OS-HP_UX OR
      os_val = ZIF_ABAPPM_TYPES=>C_OS-LINUX OR
      os_val = ZIF_ABAPPM_TYPES=>C_OS-MS_WINDOWS OR
      os_val = ZIF_ABAPPM_TYPES=>C_OS-OS_390 OR
      os_val = ZIF_ABAPPM_TYPES=>C_OS-OS_400 OR
      os_val = ZIF_ABAPPM_TYPES=>C_OS-SOLARIS ).

  ENDMETHOD.


  METHOD is_valid_package_type.

    result = boolc(
      type IS INITIAL OR
      type = ZIF_ABAPPM_TYPES=>C_PACKAGE_TYPE-COMMON_ABAP OR
      type = ZIF_ABAPPM_TYPES=>C_PACKAGE_TYPE-MODULE ).

  ENDMETHOD.


  METHOD is_valid_sap_package.

    DATA package_type TYPE c LENGTH 1.

    " Limit to local, customer, namespaced, and partner packages (see type-pool TPAK)
    CALL METHOD cl_package_helper=>check_package_name
      EXPORTING
        i_package_name = package
      IMPORTING
        e_package_type = package_type
      EXCEPTIONS
        OTHERS         = 1.

    result = boolc( sy-subrc =  0 AND package_type CA '$ZNJ' ).

    " Workaround for missing validation of empty namespace
    IF result = abap_true AND package CP '//*'.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_url.

    IF url IS INITIAL.
      result = abap_true.
    ELSE.
      result = cl_http_utility=>is_valid_url( url ).
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_version.

    " Check if it is a semantic version
    TRY.
        ZCL_ABAPPM_SEMVER=>CREATE( version ).
        result = abap_true.
      CATCH ZCX_ABAPPM_SEMVER_ERROR.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_valid_version_range.

    " Check if it is a semantic version range
    TRY.
        ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range ).
        result = abap_true.
      CATCH ZCX_ABAPPM_SEMVER_ERROR.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
