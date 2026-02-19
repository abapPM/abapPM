CLASS /apmg/cl_apm_command_uninstall DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Uninstall Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS execute
      IMPORTING
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

    METHODS check_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_uninstall IMPLEMENTATION.


  METHOD check_package.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = |Package { package } not found|.
    ENDIF.

    result = package_json_service->get( ).

  ENDMETHOD.


  METHOD execute.

    DATA(package_json) = check_package( package ).

    /apmg/cl_apm_auth=>check_package_authorized(
      package  = package
      activity = /apmg/cl_apm_auth=>c_activity-delete ).

    /apmg/cl_apm_command_installer=>uninstall_package(
      name    = package_json-name
      version = package_json-version
      package = package ).

    MESSAGE 'Package successfully uninstalled' TYPE 'S'.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_uninstall( ).

    command->execute( package ).

  ENDMETHOD.
ENDCLASS.
