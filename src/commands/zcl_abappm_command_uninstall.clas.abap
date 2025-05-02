CLASS zcl_abappm_command_uninstall DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Uninstall Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_package_json
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_uninstall IMPLEMENTATION.


  METHOD check_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      zcx_abappm_error=>raise( |Package { package } not found| ).
    ENDIF.

    result = package_json_service->get( ).

  ENDMETHOD.


  METHOD run.

    DATA(package_json) = check_package( package ).

    zcl_abappm_command_utils=>uninstall_package(
      name    = package_json-name
      version = package_json-version
      package = package ).

    MESSAGE 'Package successfully uninstalled' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
