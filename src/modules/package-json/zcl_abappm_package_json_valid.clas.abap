CLASS zcl_abappm_package_json_valid DEFINITION
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
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
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



CLASS zcl_abappm_package_json_valid IMPLEMENTATION.


  METHOD check.

    APPEND LINES OF lcl_validate=>validate_single_values( is_package_json ) TO result.

    APPEND LINES OF lcl_validate=>validate_arrays( is_package_json ) TO result.

    APPEND LINES OF lcl_validate=>validate_persons( is_package_json ) TO result.

    APPEND LINES OF lcl_validate=>validate_dependencies( is_package_json ) TO result.

  ENDMETHOD.


  METHOD is_scoped_name.
    result = boolc( is_valid_name( iv_name ) AND iv_name(1) = '@' AND iv_name CS '/' ).
  ENDMETHOD.


  METHOD is_valid_cpu.

    DATA(lv_cpu) = iv_cpu.

    SHIFT lv_cpu LEFT DELETING LEADING '!'.

    result = boolc(
      lv_cpu IS INITIAL OR
      lv_cpu = zif_abappm_package_json_types=>c_cpu-x86_64 OR
      lv_cpu = zif_abappm_package_json_types=>c_cpu-power_pc OR
      lv_cpu = zif_abappm_package_json_types=>c_cpu-sparc ).

  ENDMETHOD.


  METHOD is_valid_db.

    DATA(lv_db) = iv_db.

    SHIFT lv_db LEFT DELETING LEADING '!'.

    result = boolc(
      lv_db IS INITIAL OR
      lv_db = zif_abappm_package_json_types=>c_db-db2 OR
      lv_db = zif_abappm_package_json_types=>c_db-db400 OR
      lv_db = zif_abappm_package_json_types=>c_db-db6 OR
      lv_db = zif_abappm_package_json_types=>c_db-hdb OR
      lv_db = zif_abappm_package_json_types=>c_db-informix OR
      lv_db = zif_abappm_package_json_types=>c_db-mssql OR
      lv_db = zif_abappm_package_json_types=>c_db-oracle OR
      lv_db = zif_abappm_package_json_types=>c_db-sap_db OR
      lv_db = zif_abappm_package_json_types=>c_db-sybase ).

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
      iv_engine = zif_abappm_package_json_types=>c_engine-abap OR
      iv_engine = zif_abappm_package_json_types=>c_engine-apm ).

  ENDMETHOD.


  METHOD is_valid_name.
    " https://www.npmjs.com/package/validate-npm-package-name
    IF strlen( iv_name )
      BETWEEN zif_abappm_package_json_types=>c_package_name-min_length
          AND zif_abappm_package_json_types=>c_package_name-max_length.

      FIND REGEX zif_abappm_package_json_types=>c_package_name-regex IN iv_name RESPECTING CASE.
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
      lv_os = zif_abappm_package_json_types=>c_os-aix OR
      lv_os = zif_abappm_package_json_types=>c_os-hp_ux OR
      lv_os = zif_abappm_package_json_types=>c_os-linux OR
      lv_os = zif_abappm_package_json_types=>c_os-ms_windows OR
      lv_os = zif_abappm_package_json_types=>c_os-os_390 OR
      lv_os = zif_abappm_package_json_types=>c_os-os_400 OR
      lv_os = zif_abappm_package_json_types=>c_os-solaris ).

  ENDMETHOD.


  METHOD is_valid_package_type.

    result = boolc(
      iv_type IS INITIAL OR
      iv_type = zif_abappm_package_json_types=>c_package_type-common_abap OR
      iv_type = zif_abappm_package_json_types=>c_package_type-module ).

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
