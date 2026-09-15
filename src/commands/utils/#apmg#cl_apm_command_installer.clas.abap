CLASS /apmg/cl_apm_command_installer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Command Installer
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS install_package
      IMPORTING
        !registry  TYPE string
        !manifest  TYPE /apmg/if_apm_types=>ty_manifest
        !package   TYPE devclass
        !name      TYPE string
        !version   TYPE string
        !transport TYPE trkorr
        !data      TYPE xstring OPTIONAL
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS replace_package
      IMPORTING
        !registry     TYPE string
        !manifest     TYPE /apmg/if_apm_types=>ty_manifest
        !package      TYPE devclass
        !name         TYPE string
        !from_version TYPE string
        !to_version   TYPE string
        !transport    TYPE trkorr
        !data         TYPE xstring OPTIONAL
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_package_data
      IMPORTING
        !registry     TYPE string
        !manifest     TYPE /apmg/if_apm_types=>ty_manifest
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS uninstall_package
      IMPORTING
        !name      TYPE string
        !version   TYPE string
        !package   TYPE devclass
        !transport TYPE trkorr
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /apmg/cl_apm_command_installer IMPLEMENTATION.


  METHOD install_package.

    DATA(package_data) = data.
    IF package_data IS INITIAL.
      package_data = get_package_data(
        registry = registry
        manifest = manifest
        name     = name ).
    ENDIF.

    " FUTURE: Allow other folder logic than prefix
    /apmg/cl_apm_installer=>install(
      name              = name
      version           = version
      data              = package_data
      package           = package
      transport         = transport
      enum_source       = /apmg/cl_apm_installer=>c_enum_source-registry
      enum_folder_logic = /apmg/cl_apm_installer=>c_enum_folder_logic-prefix ).

  ENDMETHOD.


  METHOD get_package_data.

    result = /apmg/cl_apm_registry=>get_tarball(
      registry = registry
      name     = name
      tarball  = manifest-dist-tarball ).

    /apmg/cl_apm_integrity=>check(
      tarball = result
      dist    = manifest-dist ).

  ENDMETHOD.


  METHOD replace_package.

    DATA(package_data) = data.
    IF package_data IS INITIAL.
      package_data = get_package_data(
        registry = registry
        manifest = manifest
        name     = name ).
    ENDIF.

    uninstall_package(
      name      = name
      version   = from_version
      package   = package
      transport = transport ).

    install_package(
      registry  = registry
      manifest  = manifest
      package   = package
      name      = name
      version   = to_version
      transport = transport
      data      = package_data ).

  ENDMETHOD.


  METHOD uninstall_package.

    /apmg/cl_apm_installer=>uninstall(
      name      = name
      version   = version
      package   = package
      transport = transport ).

  ENDMETHOD.
ENDCLASS.
