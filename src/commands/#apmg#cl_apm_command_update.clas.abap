CLASS /apmg/cl_apm_command_update DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Update Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry      TYPE string
        !package       TYPE devclass
        !to_version    TYPE string
        !transport     TYPE trkorr OPTIONAL
        !force         TYPE abap_bool DEFAULT abap_false
        !is_dry_run    TYPE abap_bool DEFAULT abap_false
        !is_production TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS execute
      IMPORTING
        !registry      TYPE string
        !package       TYPE devclass
        !to_version    TYPE string
        !transport     TYPE trkorr
        !force         TYPE abap_bool
        !is_dry_run    TYPE abap_bool
        !is_production TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    METHODS get_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

    METHODS get_bundle_dependencies
      IMPORTING
        !package      TYPE devclass
        !manifest     TYPE /apmg/if_apm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_dependencies
      RAISING
        /apmg/cx_apm_error.

    METHODS get_max_satisfying_version
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !range        TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS get_dependencies_to_import
      IMPORTING
        !registry     TYPE string
        !dependencies TYPE /apmg/if_apm_importer=>ty_dependencies
        !force        TYPE abap_bool
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_dependencies
      RAISING
        /apmg/cx_apm_error.

    METHODS add_dependencies
      IMPORTING
        !package       TYPE devclass
        !dependencies  TYPE /apmg/if_apm_importer=>ty_dependencies
        !transport     TYPE trkorr
        !is_dry_run    TYPE abap_bool
        !is_production TYPE abap_bool
      RETURNING
        VALUE(result)  TYPE /apmg/if_apm_importer=>ty_dependencies
      RAISING
        /apmg/cx_apm_error.

    METHODS remove_dependencies
      IMPORTING
        !dependencies TYPE /apmg/if_apm_importer=>ty_dependencies
        !transport    TYPE trkorr
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_dependencies
      RAISING
        /apmg/cx_apm_error.

    METHODS is_newer
      IMPORTING
        !version_installed TYPE string
        !version_next      TYPE string
      RETURNING
        VALUE(result)      TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    METHODS save_package
      IMPORTING
        !package  TYPE devclass
        !manifest TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_update IMPLEMENTATION.


  METHOD add_dependencies.

    /apmg/cl_apm_importer=>run(
      package       = package
      dependencies  = dependencies
      transport     = transport
      is_dry_run    = is_dry_run
      is_production = is_production
      is_logging    = abap_false ).

  ENDMETHOD.


  METHOD execute.

    /apmg/cl_apm_auth=>check_package_authorized(
      package  = package
      activity = /apmg/cl_apm_auth=>c_activity-change ).

    " 1. Check package is installed and get version details
    DATA(package_json) = get_package( package ).

    " 2. Get manifest for target version
    TRY.
        DATA(manifest) = /apmg/cl_apm_registry=>get_manifest(
          registry = registry
          name     = package_json-name
          version  = to_version ).
      CATCH /apmg/cx_apm_error.
        " If version does not exist in registry, use local definition
        manifest = CORRESPONDING /apmg/if_apm_types=>ty_manifest( package_json ).
    ENDTRY.

    " 3. Update the package
    DATA(is_newer) = is_newer(
      version_installed = package_json-version
      version_next      = manifest-version ).

    " 4. Get bundled dependencies
    DATA(dependencies) = get_bundle_dependencies(
      package  = package
      manifest = manifest ).

    " 5. Add, remove, or update dependencies
    DATA(import_dependencies) = get_dependencies_to_import(
      registry     = registry
      dependencies = dependencies
      force        = force ).

    " 6. Import bundled dependencies
    add_dependencies(
      package       = package
      dependencies  = import_dependencies
      transport     = transport
      is_dry_run    = is_dry_run
      is_production = is_production ).

    " 7. Uninstall bundled dependencies that have been removed
    remove_dependencies(
      dependencies = dependencies
      transport    = transport ).

    " 8. Update package
    IF is_newer = abap_true OR force = abap_true.
      /apmg/cl_apm_command_installer=>install_package(
        registry      = registry
        manifest      = manifest
        package       = package
        name          = manifest-name
        version       = manifest-version
        transport     = transport
        is_production = is_production ).
    ENDIF.

    " 9. Save package to apm
    save_package(
      package  = package
      manifest = manifest ).

    MESSAGE 'Package and dependencies successfully updated' TYPE 'S'.

  ENDMETHOD.


  METHOD get_bundle_dependencies.

    " XXX: Major rewrite
    " Instead of scanning sub-packages and comparing to list of installed packages (apm persistence),
    " Use the existing package manifest and compare it to the new one from the registry

    " Get all installed packages
    DATA(list) = /apmg/cl_apm_package_json=>list( instanciate = abap_true ).

    " Get all sub packages where a dependency could be installed
    DATA(sub_packages) = zcl_abapgit_factory=>get_sap_package( package )->list_subpackages( ).

    LOOP AT sub_packages ASSIGNING FIELD-SYMBOL(<package>).

      READ TABLE list ASSIGNING FIELD-SYMBOL(<package_json>)
        WITH KEY package COMPONENTS package = <package>.
      IF sy-subrc = 0.
        IF line_exists( manifest-bundle_dependencies[ table_line = <package_json>-name ] ).
          READ TABLE manifest-dependencies ASSIGNING FIELD-SYMBOL(<dependency>)
            WITH KEY key = <package_json>-name.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
              EXPORTING
                text = |Bundle dependency { <package_json>-name } missing from dependencies|.
          ENDIF.
          " Installed bundle dependency which will be updated (if available)
          DATA(dependency) = VALUE /apmg/if_apm_importer=>ty_dependency(
            name    = <package_json>-name
            version = <package_json>-version
            package = <package>
            range   = <dependency>-range
            action  = /apmg/if_apm_importer=>c_action-update ).
        ELSE.
          " Dependency which is not bundled (anymore) and will be removed
          dependency = VALUE /apmg/if_apm_importer=>ty_dependency(
            name    = <package_json>-name
            version = <package_json>-version
            package = <package>
            action  = /apmg/if_apm_importer=>c_action-remove ).
        ENDIF.
        INSERT dependency INTO TABLE result.
      ENDIF.

    ENDLOOP.

    LOOP AT manifest-bundle_dependencies ASSIGNING FIELD-SYMBOL(<bundle>).
      IF NOT line_exists( result[ name = <bundle> ] ).
        IF NOT line_exists( manifest-dependencies[ key = <bundle> ] ).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
            EXPORTING
              text = |Bundle dependency { <bundle> } missing from dependencies|.
        ENDIF.
        " New bundle dependency which will be added
        dependency = VALUE /apmg/if_apm_importer=>ty_dependency(
          name   = <bundle>
          range  = <dependency>-range
          action = /apmg/if_apm_importer=>c_action-add ).
        INSERT dependency INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_dependencies_to_import.

    " Install new or update existing dependencies
    LOOP AT dependencies INTO DATA(dependency) WHERE action <> /apmg/if_apm_importer=>c_action-remove.

      DATA(max_version) = get_max_satisfying_version(
        registry = registry
        name     = dependency-name
        range    = dependency-range ).

      IF max_version IS INITIAL.
        " TODO: log errors and raise at end
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |No matching version found in registry for package { dependency-name }| &&
                   | and range { dependency-range }|.
      ENDIF.

      CASE dependency-action.
        WHEN /apmg/if_apm_importer=>c_action-add.
          " Install the maximum satisfying version
          dependency-version = max_version.
          INSERT dependency INTO TABLE result.
        WHEN /apmg/if_apm_importer=>c_action-update.
          DATA(is_newer) = is_newer(
            version_installed = dependency-version
            version_next      = max_version ).

          IF is_newer = abap_true OR force = abap_true.
            " Update to this newer version
            dependency-version = max_version.
            INSERT dependency INTO TABLE result.
          ENDIF.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_max_satisfying_version.

    DATA versions TYPE string_table.

    DATA(packument) = /apmg/cl_apm_registry=>get_packument(
      registry = registry
      name     = name ).

    LOOP AT packument-versions ASSIGNING FIELD-SYMBOL(<version>).
      INSERT <version>-key INTO TABLE versions.
    ENDLOOP.

    result = /apmg/cl_apm_semver_ranges=>max_satisfying(
      versions = versions
      range    = range ).

  ENDMETHOD.


  METHOD get_package.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_true.
      result = package_json_service->get( ).
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |{ package } does not contain any installed package|.
    ENDIF.

  ENDMETHOD.


  METHOD is_newer.

    " Is the next version greater than the installed version?
    result = /apmg/cl_apm_semver_functions=>gt(
      a = version_next
      b = version_installed ).

  ENDMETHOD.


  METHOD remove_dependencies.

    " Uninstall removed dependencies
    LOOP AT dependencies INTO DATA(dependency) WHERE action = /apmg/if_apm_importer=>c_action-remove.

      /apmg/cl_apm_command_installer=>uninstall_package(
        name      = dependency-name
        version   = dependency-version
        package   = dependency-package
        transport = transport ).

    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_update( ).

    command->execute(
      registry      = registry
      package       = package
      to_version    = to_version
      transport     = transport
      force         = force
      is_dry_run    = is_dry_run
      is_production = is_production ).

  ENDMETHOD.


  METHOD save_package.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).
    DATA(package_json) = CORRESPONDING /apmg/if_apm_types=>ty_package_json( manifest ).

    package_json_service->set( package_json )->save( ).

  ENDMETHOD.
ENDCLASS.
