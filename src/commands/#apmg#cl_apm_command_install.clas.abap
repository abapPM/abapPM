CLASS /apmg/cl_apm_command_install DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Install Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry      TYPE string
        !package       TYPE devclass
        !package_json  TYPE /apmg/if_apm_types=>ty_package_json
        !is_production TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_action,
        install TYPE /apmg/if_apm_types=>ty_dependency,
        update  TYPE /apmg/if_apm_types=>ty_dependency,
      END OF ty_action,
      BEGIN OF ty_actions,
        install TYPE /apmg/if_apm_types=>ty_dependencies,
        update  TYPE /apmg/if_apm_types=>ty_dependencies,
      END OF ty_actions.

    METHODS execute
      IMPORTING
        !registry      TYPE string
        !package       TYPE devclass
        !package_json  TYPE /apmg/if_apm_types=>ty_package_json
        !is_production TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

    METHODS check_package
      IMPORTING
        !package TYPE devclass
        !name    TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS check_prerequisites
      IMPORTING
        !manifest TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

    METHODS check_dependencies
      IMPORTING
        !manifest      TYPE /apmg/if_apm_types=>ty_manifest
        !is_production TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)  TYPE ty_actions
      RAISING
        /apmg/cx_apm_error.

    METHODS check_dependency
      IMPORTING
        !list         TYPE /apmg/if_apm_package_json=>ty_packages
        !dependency   TYPE /apmg/if_apm_types=>ty_dependency
        !category     TYPE string
        !is_optional  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE ty_action
      RAISING
        /apmg/cx_apm_error.

    METHODS check_semver
      IMPORTING
        !name        TYPE string
        !version     TYPE string
        !range       TYPE string
        !category    TYPE string
        !is_optional TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_install IMPLEMENTATION.


  METHOD check_dependencies.

    " Get all installed packages
    DATA(list) = /apmg/cl_apm_package_json=>list( ).

    LOOP AT manifest-dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
      IF NOT line_exists( manifest-bundle_dependencies[ table_line = <dependency>-key ] ).
        DATA(action) = check_dependency(
          list       = list
          dependency = <dependency>
          category   = 'Dependency' ).

        IF action-install IS NOT INITIAL.
          INSERT action-install INTO TABLE result-install.
        ENDIF.
        IF action-update IS NOT INITIAL.
          INSERT action-update INTO TABLE result-update.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF is_production = abap_false.
      LOOP AT manifest-dev_dependencies ASSIGNING <dependency>.
        action = check_dependency(
          list       = list
          dependency = <dependency>
          category   = 'devDependency' ).

        IF action-install IS NOT INITIAL.
          INSERT action-install INTO TABLE result-install.
        ENDIF.
        IF action-update IS NOT INITIAL.
          INSERT action-update INTO TABLE result-update.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT manifest-optional_dependencies ASSIGNING <dependency>.
      action = check_dependency(
        list        = list
        dependency  = <dependency>
        category    = 'optionalDependency'
        is_optional = abap_true ).

      IF action-install IS NOT INITIAL.
        INSERT action-install INTO TABLE result-install.
      ENDIF.
      IF action-update IS NOT INITIAL.
        INSERT action-update INTO TABLE result-update.
      ENDIF.
    ENDLOOP.

    LOOP AT manifest-peer_dependencies ASSIGNING <dependency>.
      action = check_dependency(
        list        = list
        dependency  = <dependency>
        category    = 'peerDependency'
        is_optional = abap_true ).

      IF action-install IS NOT INITIAL.
        INSERT action-install INTO TABLE result-install.
      ENDIF.
      IF action-update IS NOT INITIAL.
        INSERT action-update INTO TABLE result-update.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_dependency.

    READ TABLE list ASSIGNING FIELD-SYMBOL(<package>)
      WITH KEY name COMPONENTS name = dependency-key.
    IF sy-subrc = 0.
      DATA(satisfies) = /apmg/cl_apm_semver_functions=>satisfies(
        version = <package>-version
        range   = dependency-range ).

      IF satisfies = abap_false.
        IF is_optional = abap_true.
          " TODO: Log warning
        ELSE.
          " TODO: Log info
          DATA(text) = |{ category } { dependency-key } is installed in version { <package>-version } | &&
                       |but does not satisfy { dependency-range }|.
          result-update = dependency.
        ENDIF.
      ENDIF.
    ELSE.
      IF is_optional = abap_true.
        " TODO: Log warning
      ELSE.
        " TODO: Log info
        text = |{ category } { dependency-key } is not installed|.
        result-install = dependency.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_package.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_true.
      DATA(existing_name) = package_json_service->get( )-name.
      IF existing_name = name.
        " TODO: log warning
      ELSE.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |{ package } already contains package "{ existing_name }"|.
      ENDIF.
    ENDIF.

    SELECT COUNT(*) FROM tadir INTO @DATA(count) WHERE devclass = @package.
    IF count > 1.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |{ package } already contains { count } objects|.
    ENDIF.

  ENDMETHOD.


  METHOD check_prerequisites.

    " apm version
    READ TABLE manifest-engines ASSIGNING FIELD-SYMBOL(<dependency>)
      WITH KEY key = 'apm'.
    IF sy-subrc = 0.
      check_semver(
        name     = 'apm'
        version  = /apmg/if_apm_version=>c_version
        range    = <dependency>-range
        category = 'Engine' ).
    ENDIF.

    " abap release
    READ TABLE manifest-engines ASSIGNING <dependency>
      WITH KEY key = 'abap'.
    IF sy-subrc = 0.
      TRY.
          DATA(abap_version) = NEW /apmg/cl_apm_semver_sap( )->sap_component_to_semver( 'SAP_BASIS' ).
        CATCH cx_abap_invalid_value INTO DATA(error).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
      ENDTRY.

      check_semver(
        name     = 'ABAP'
        version  = abap_version
        range    = <dependency>-range
        category = 'Engine' ).
    ENDIF.

    " TODO: Check os & cpu (requires "env" package which is =WIP=)

  ENDMETHOD.


  METHOD check_semver.

    DATA(satisfies) = /apmg/cl_apm_semver_functions=>satisfies(
      version = version
      range   = range ).

    IF satisfies = abap_false.
      IF is_optional = abap_true.
        " TODO: Log warning
      ELSE.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |{ category } { name } is installed in version { version } | &&
                   |but does not satisfy { range }|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD execute.

    DATA package_json_init TYPE /apmg/if_apm_types=>ty_package_json.

    " 1. Check if something else is already installed
    check_package(
      package = package
      name    = package_json-name ).

    " 2. Get manifest
    DATA(manifest) = /apmg/cl_apm_command_utils=>get_manifest_from_registry(
      registry = registry
      name     = package_json-name
      version  = package_json-version ).

    " 3. Check prerequisites (os, cpu, engines)
    check_prerequisites( manifest ).

    " TODO!: Instead of just checking if dependencies are installed, it should install them.
    " For that to happen, we need arborist to build the dependency tree and pass it here.
    " This needs to include the target SAP package for each dependency :-)

    " 4. Check dependencies
    DATA(actions) = check_dependencies( manifest ).

    " TODO: 4. Get dependencies
    " TODO: 5. Install dependencies

    " 6. Get tarball from registry and install it into package
    /apmg/cl_apm_command_utils=>install_package(
      registry      = registry
      manifest      = manifest
      package       = package
      name          = package_json-name
      version       = package_json-version
      is_production = is_production ).

    " 7. Save package.abap.json and readme
    package_json_init = CORRESPONDING #( manifest ).

    /apmg/cl_apm_command_init=>run(
      package      = package
      package_json = package_json_init ).

    MESSAGE 'Package successfully installed' TYPE 'S'.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_install( ).

    command->execute(
      registry      = registry
      package       = package
      package_json  = package_json
      is_production = is_production ).

  ENDMETHOD.
ENDCLASS.
