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
        !is_force      TYPE abap_bool DEFAULT abap_false
        !is_dry_run    TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_action,
        missing TYPE /apmg/if_apm_types=>ty_dependency,
        invalid TYPE /apmg/if_apm_types=>ty_dependency,
        error   TYPE string,
        warning TYPE string,
      END OF ty_action,
      BEGIN OF ty_actions,
        missing  TYPE /apmg/if_apm_types=>ty_dependencies,
        invalid  TYPE /apmg/if_apm_types=>ty_dependencies,
        errors   TYPE string_table,
        warnings TYPE string_table,
      END OF ty_actions.

    METHODS execute
      IMPORTING
        !registry      TYPE string
        !package       TYPE devclass
        !package_json  TYPE /apmg/if_apm_types=>ty_package_json
        !is_production TYPE abap_bool
        !is_force      TYPE abap_bool
        !is_dry_run    TYPE abap_bool
      RAISING
        /apmg/cx_apm_error ##NEEDED.

    METHODS check_package
      IMPORTING
        !package TYPE devclass
        !name    TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS check_prerequisites
      IMPORTING
        !manifest TYPE /apmg/if_apm_types=>ty_manifest
        !is_force TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    METHODS collect_actions
      IMPORTING
        !action TYPE ty_action
      CHANGING
        result  TYPE ty_actions.

    METHODS check_actions
      IMPORTING
        !actions TYPE ty_actions
      RAISING
        /apmg/cx_apm_error.

    METHODS check_dependencies
      IMPORTING
        !manifest      TYPE /apmg/if_apm_types=>ty_manifest
        !is_force      TYPE abap_bool DEFAULT abap_false
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
        !is_force     TYPE abap_bool DEFAULT abap_false
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
        !is_force    TYPE abap_bool DEFAULT abap_false
        !is_optional TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.
ENDCLASS.



CLASS /apmg/cl_apm_command_install IMPLEMENTATION.


  METHOD check_actions.

    " TODO: log all warnings and errors
    IF actions-errors IS NOT INITIAL.
      DATA(text) = concat_lines_of( table = actions-errors sep = |\n| ).
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = text.
    ENDIF.

  ENDMETHOD.


  METHOD check_dependencies.

    " Get all installed packages
    DATA(list) = /apmg/cl_apm_package_json=>list( ).

    LOOP AT manifest-dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
      IF NOT line_exists( manifest-bundle_dependencies[ table_line = <dependency>-key ] ).
        DATA(action) = check_dependency(
          list       = list
          dependency = <dependency>
          category   = 'Dependency'
          is_force   = is_force ).

        collect_actions(
          EXPORTING
            action = action
          CHANGING
            result = result ).
      ENDIF.
    ENDLOOP.

    IF is_production = abap_false.
      LOOP AT manifest-dev_dependencies ASSIGNING <dependency>.
        action = check_dependency(
          list       = list
          dependency = <dependency>
          category   = 'devDependency'
          is_force   = is_force ).

        collect_actions(
          EXPORTING
            action = action
          CHANGING
            result = result ).
      ENDLOOP.
    ENDIF.

    LOOP AT manifest-optional_dependencies ASSIGNING <dependency>.
      action = check_dependency(
        list        = list
        dependency  = <dependency>
        category    = 'optionalDependency'
        is_force    = is_force
        is_optional = abap_true ).

      collect_actions(
        EXPORTING
          action = action
        CHANGING
          result = result ).
    ENDLOOP.

    LOOP AT manifest-peer_dependencies ASSIGNING <dependency>.
      action = check_dependency(
        list        = list
        dependency  = <dependency>
        category    = 'peerDependency'
        is_force    = is_force
        is_optional = abap_true ).

      collect_actions(
        EXPORTING
          action = action
        CHANGING
          result = result ).
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
        IF is_optional = abap_true OR is_force = abap_true.
          result-warning = |{ category } { dependency-key } is installed in version { <package>-version } | &&
                           |and does not satisfy { dependency-range } but is optional|.
        ELSE.
          result-invalid = dependency.
          result-error   = |{ category } { dependency-key } is installed in version { <package>-version } | &&
                           |but does not satisfy { dependency-range }|.
        ENDIF.
      ENDIF.
    ELSE.
      IF is_optional = abap_true OR is_force = abap_true.
        result-warning = |{ category } { dependency-key } is not installed but optional|.
      ELSE.
        result-missing = dependency.
        result-error   = |{ category } { dependency-key } is not installed|.
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

    SELECT COUNT(*) FROM tadir INTO @DATA(count) WHERE devclass = @package. "#EC CI_SGLSELECT
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
        category = 'Engine'
        is_force = is_force ).
    ENDIF.

    " abap release
    READ TABLE manifest-engines ASSIGNING <dependency>
      WITH KEY key = 'abap'.
    IF sy-subrc = 0.
      DATA(abap_version) = /apmg/cl_apm_command_utils=>get_abap_version( ).

      check_semver(
        name     = 'ABAP'
        version  = abap_version
        range    = <dependency>-range
        category = 'Engine'
        is_force = is_force ).
    ENDIF.

    " TODO: Check os & cpu (requires "env" package which is =WIP=)

  ENDMETHOD.


  METHOD check_semver.

    DATA(satisfies) = /apmg/cl_apm_semver_functions=>satisfies(
      version = version
      range   = range ).

    IF satisfies = abap_false.
      IF is_optional = abap_true OR is_force = abap_true.
        " TODO: Log warning
      ELSE.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |{ category } { name } is installed in version { version } | &&
                   |but does not satisfy { range }|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD collect_actions.

    IF action-missing IS NOT INITIAL.
      INSERT action-missing INTO TABLE result-missing.
    ENDIF.
    IF action-invalid IS NOT INITIAL.
      INSERT action-invalid INTO TABLE result-invalid.
    ENDIF.
    IF action-error IS NOT INITIAL.
      INSERT action-error INTO TABLE result-errors.
    ENDIF.
    IF action-warning IS NOT INITIAL.
      INSERT action-warning INTO TABLE result-warnings.
    ENDIF.

  ENDMETHOD.


  METHOD execute.

    DATA package_json_init TYPE /apmg/if_apm_types=>ty_package_json.

    " Authorization check
    /apmg/cl_apm_auth=>check_package_authorized(
      package  = package
      activity = /apmg/cl_apm_auth=>c_activity-create ).
    /apmg/cl_apm_auth=>check_package_authorized(
      package  = package
      activity = /apmg/cl_apm_auth=>c_activity-change ).

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
    check_prerequisites(
      manifest = manifest
      is_force = is_force ).

    " TODO!: Instead of just checking if dependencies are installed, it should install them.
    " For that to happen, we need arborist to build the dependency tree and pass it here.
    " This needs to include the target SAP package for each dependency :-)

    " 4. Check dependencies (not recursive!)
    DATA(actions) = check_dependencies(
      manifest = manifest
      is_force = is_force ).

    check_actions( actions ).

    " TODO: 4. Get dependencies
    " TODO: 5. Install dependencies

    " 6. Get tarball from registry and install it into package
    /apmg/cl_apm_command_installer=>install_package(
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
      is_production = is_production
      is_force      = is_force
      is_dry_run    = is_dry_run ).

  ENDMETHOD.
ENDCLASS.
