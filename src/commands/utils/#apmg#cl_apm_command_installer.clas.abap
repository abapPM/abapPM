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
        !registry      TYPE string
        !manifest      TYPE /apmg/if_apm_types=>ty_manifest
        !package       TYPE devclass
        !name          TYPE string
        !version       TYPE string
        !is_production TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS uninstall_package
      IMPORTING
        !name    TYPE string
        !version TYPE string
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /apmg/cl_apm_command_installer IMPLEMENTATION.


  METHOD install_package.

    " TODO: Currently hardcoded to local packages (no transport)
    DATA transport TYPE trkorr.

    DATA(tarball) = /apmg/cl_apm_command_utils=>get_tarball_from_registry(
      registry = registry
      name     = name
      tarball  = manifest-dist-tarball ).

    /apmg/cl_apm_command_integrity=>check_integrity(
      tarball = tarball
      dist    = manifest-dist ).

    " FUTURE: Allow other folder logic than prefix
    /apmg/cl_apm_installer=>install(
      name              = name
      version           = version
      data              = tarball
      package           = package
      transport         = transport
      enum_source       = /apmg/cl_apm_installer=>c_enum_source-registry
      enum_folder_logic = /apmg/cl_apm_installer=>c_enum_folder_logic-prefix
      is_production     = is_production ).

  ENDMETHOD.


  METHOD uninstall_package.

    /apmg/cl_apm_installer=>uninstall(
      name    = name
      version = version
      package = package ).

  ENDMETHOD.
ENDCLASS.
