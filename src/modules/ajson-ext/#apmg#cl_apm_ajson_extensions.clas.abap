CLASS /apmg/cl_apm_ajson_extensions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Extensions for AJSON (Filter, Mappings, etc.)
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    "! Like zcl_ajson_mapping=>create_to_camel_case( ) but keep any leading underscore
    "! and first characters after it in lower case
    "!
    "! Examples:
    "! _ID              -> _id
    "! _ABAP_VERSION    -> _abapVersion
    "! DEV_DEPENDENCIES -> devDependencies
    CLASS-METHODS to_camel_case_underscore
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_ajson_mapping.

    "! Like zcl_ajson_mapping=>create_camel_case( ) but keep any leading underscore
    "!
    "! Examples:
    "! _id             -> _ID
    "! _abapVersion    -> _ABAP_VERSION
    "! devDependencies -> DEV_DEPENDENCIES
    CLASS-METHODS from_camel_case_underscore
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_ajson_mapping.

    "! Like zcl_ajson_filter_lib=>create_empty_filter( ) but also remove initial numbers and null
    CLASS-METHODS filter_empty_zero_null
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_ajson_filter.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_ajson_extensions IMPLEMENTATION.


  METHOD filter_empty_zero_null.
    CREATE OBJECT result TYPE lcl_empty_zero_null.
  ENDMETHOD.


  METHOD from_camel_case_underscore.
    CREATE OBJECT result TYPE lcl_from_camel_case_underscore.
  ENDMETHOD.


  METHOD to_camel_case_underscore.
    CREATE OBJECT result TYPE lcl_to_camel_case_underscore.
  ENDMETHOD.
ENDCLASS.
