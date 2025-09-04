CLASS /apmg/cl_apm_package_json_vali DEFINITION
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
        !manifest     TYPE /apmg/if_apm_types=>ty_manifest
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

    CLASS-METHODS is_valid_timestamp
      IMPORTING
        !timestamp    TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_valid_abap_language_version
      IMPORTING
        !vers         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_package_json_vali IMPLEMENTATION.


  METHOD check.

    APPEND LINES OF lcl_validate=>validate_single_values( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_arrays( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_persons( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_engines( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_dependencies( manifest ) TO result.
    APPEND LINES OF lcl_validate=>validate_sap_package( manifest ) TO result.

  ENDMETHOD.


  METHOD is_scoped_name.

    result = xsdbool( is_valid_name( name ) AND name(1) = '@' AND name CS '/' ).

  ENDMETHOD.


  METHOD is_valid_abap_language_version.

    DATA(vers_val) = vers.

    SHIFT vers_val LEFT DELETING LEADING '!'.

    result = xsdbool(
      vers_val IS INITIAL OR
      vers_val = /apmg/if_apm_types=>c_abap_language_version-standard OR
      vers_val = /apmg/if_apm_types=>c_abap_language_version-key_user OR
      vers_val = /apmg/if_apm_types=>c_abap_language_version-cloud_development OR
      vers_val = /apmg/if_apm_types=>c_abap_language_version-ignore OR
      vers_val = /apmg/if_apm_types=>c_abap_language_version-undefined ).

  ENDMETHOD.


  METHOD is_valid_cpu.

    DATA(cpu_val) = cpu.

    SHIFT cpu_val LEFT DELETING LEADING '!'.

    result = xsdbool(
      cpu_val IS INITIAL OR
      cpu_val = /apmg/if_apm_types=>c_cpu-x86_64 OR
      cpu_val = /apmg/if_apm_types=>c_cpu-power_pc OR
      cpu_val = /apmg/if_apm_types=>c_cpu-sparc ).

  ENDMETHOD.


  METHOD is_valid_db.

    DATA(db_val) = db.

    SHIFT db_val LEFT DELETING LEADING '!'.

    result = xsdbool(
      db_val IS INITIAL OR
      db_val = /apmg/if_apm_types=>c_db-db2 OR
      db_val = /apmg/if_apm_types=>c_db-db400 OR
      db_val = /apmg/if_apm_types=>c_db-db6 OR
      db_val = /apmg/if_apm_types=>c_db-hdb OR
      db_val = /apmg/if_apm_types=>c_db-informix OR
      db_val = /apmg/if_apm_types=>c_db-mssql OR
      db_val = /apmg/if_apm_types=>c_db-oracle OR
      db_val = /apmg/if_apm_types=>c_db-sap_db OR
      db_val = /apmg/if_apm_types=>c_db-sybase ).

  ENDMETHOD.


  METHOD is_valid_email.

    " Email address validation (RFC 5322)
    CONSTANTS c_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF email IS INITIAL.
      result = abap_true.
    ELSE.
      FIND REGEX c_email_regex IN email.
      result = xsdbool( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_engine.

    result = xsdbool(
      engine IS INITIAL OR
      engine = /apmg/if_apm_types=>c_engine-abap OR
      engine = /apmg/if_apm_types=>c_engine-apm ).

  ENDMETHOD.


  METHOD is_valid_name.

    " https://www.npmjs.com/package/validate-npm-package-name
    IF strlen( name )
      BETWEEN /apmg/if_apm_types=>c_package_name-min_length
          AND /apmg/if_apm_types=>c_package_name-max_length.

      FIND REGEX /apmg/if_apm_types=>c_package_name-regex IN name RESPECTING CASE.
      result = xsdbool( sy-subrc = 0 ).
    ELSE.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_os.

    DATA(os_val) = os.

    SHIFT os_val LEFT DELETING LEADING '!'.

    result = xsdbool(
      os_val IS INITIAL OR
      os_val = /apmg/if_apm_types=>c_os-aix OR
      os_val = /apmg/if_apm_types=>c_os-hp_ux OR
      os_val = /apmg/if_apm_types=>c_os-linux OR
      os_val = /apmg/if_apm_types=>c_os-ms_windows OR
      os_val = /apmg/if_apm_types=>c_os-os_390 OR
      os_val = /apmg/if_apm_types=>c_os-os_400 OR
      os_val = /apmg/if_apm_types=>c_os-solaris ).

  ENDMETHOD.


  METHOD is_valid_package_type.

    result = xsdbool(
      type IS INITIAL OR
      type = /apmg/if_apm_types=>c_package_type-common_abap OR
      type = /apmg/if_apm_types=>c_package_type-module ).

  ENDMETHOD.


  METHOD is_valid_sap_package.

    DATA package_type TYPE c LENGTH 1.

    " Limit to local, customer, namespaced, and partner packages (see type-pool TPAK)
    cl_package_helper=>check_package_name(
      EXPORTING
        i_package_name = package
      IMPORTING
        e_package_type = package_type
      EXCEPTIONS
        OTHERS         = 1 ).

    result = xsdbool( sy-subrc = 0 AND package_type CA '$ZNJ' ).

    " Workaround for missing validation of empty namespace
    IF result = abap_true AND package CP '//*'.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_timestamp.

    CONSTANTS c_regex TYPE string
      VALUE `^(\d{4})-(\d{2})-(\d{2})(T)(\d{2}):(\d{2}):(\d{2})(\.\d+)?(Z|$)` ##NO_TEXT.

    FIND REGEX c_regex IN timestamp.
    result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_valid_url.

    TRY.
        IF url IS NOT INITIAL.
          /apmg/cl_apm_url=>parse( url ).
        ENDIF.
        result = abap_true.
      CATCH /apmg/cx_apm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_valid_version.

    " Check if it is a semantic version
    TRY.
        /apmg/cl_apm_semver=>create( version ).
        result = abap_true.
      CATCH /apmg/cx_apm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_valid_version_range.

    " Check if it is a semantic version range
    TRY.
        /apmg/cl_apm_semver_range=>create( range ).
        result = abap_true.
      CATCH /apmg/cx_apm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
