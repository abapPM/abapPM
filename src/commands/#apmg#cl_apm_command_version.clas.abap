CLASS /apmg/cl_apm_command_version DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Version Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package      TYPE devclass
        !version      TYPE string OPTIONAL
        !release_type TYPE string OPTIONAL
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS execute
      IMPORTING
        !package      TYPE devclass
        !version      TYPE string
        !release_type TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS check_package
      IMPORTING
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

    METHODS get_package_json
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_version IMPLEMENTATION.


  METHOD check_package.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |SAP package { package } does not exist or is not initialized|.
    ENDIF.

  ENDMETHOD.


  METHOD execute.

    " 1. Check if package exists and is initialized
    check_package( package ).

    " 2. Get package.abap.json
    DATA(package_json) = get_package_json( package ).

    " 3. Initialize Package JSON service
    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory(
      package = package
      name    = package_json-name
      version = package_json-version
      private = package_json-private ).

    " 4. Update version
    IF version IS INITIAL.
      DATA(new_version) = /apmg/cl_apm_semver=>create( package_json-version ).
      package_json-version = new_version->inc( release_type )->to_string( ).
    ELSE.
      package_json-version = version.
    ENDIF.

    " 5. Save
    package_json_service->set( package_json )->save( ).

    MESSAGE |Package version changed to { package_json-version }| TYPE 'S'.

  ENDMETHOD.


  METHOD get_package_json.

    result = /apmg/cl_apm_package_json=>factory( package )->load( )->get( ).

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_version( ).

    command->execute(
      package      = package
      version      = version
      release_type = release_type ).

  ENDMETHOD.
ENDCLASS.
