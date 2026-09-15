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

    TYPES:
      BEGIN OF ty_package_assignment,
        name    TYPE /apmg/if_apm_types=>ty_name,
        package TYPE devclass,
      END OF ty_package_assignment,
      ty_package_assignments TYPE HASHED TABLE OF ty_package_assignment
        WITH UNIQUE KEY name.

    CLASS-METHODS run
      IMPORTING
        !registry    TYPE string
        !root_name   TYPE /apmg/if_apm_types=>ty_name
        !diff        TYPE REF TO /apmg/if_apm_arborist_diff
        !assignments TYPE ty_package_assignments
        !transport   TYPE trkorr OPTIONAL
        !is_force    TYPE abap_bool DEFAULT abap_false
        !is_dry_run  TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_change,
        sequence     TYPE i,
        action       TYPE /apmg/if_apm_arborist=>ty_diff_action,
        name         TYPE /apmg/if_apm_types=>ty_name,
        from_version TYPE /apmg/if_apm_types=>ty_version,
        to_version   TYPE /apmg/if_apm_types=>ty_version,
        package      TYPE devclass,
        manifest     TYPE /apmg/if_apm_types=>ty_manifest,
        tarball      TYPE xstring,
        actual       TYPE REF TO /apmg/cl_apm_arborist_node,
        ideal        TYPE REF TO /apmg/cl_apm_arborist_node,
      END OF ty_change,
      ty_changes TYPE STANDARD TABLE OF ty_change WITH EMPTY KEY.

    DATA packages TYPE /apmg/if_apm_package_json=>ty_packages.

    METHODS execute
      IMPORTING
        !registry    TYPE string
        !root_name   TYPE /apmg/if_apm_types=>ty_name
        !diff        TYPE REF TO /apmg/if_apm_arborist_diff
        !assignments TYPE ty_package_assignments
        !transport   TYPE trkorr
        !is_force    TYPE abap_bool
        !is_dry_run  TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    METHODS normalize_changes
      IMPORTING
        !root_name    TYPE /apmg/if_apm_types=>ty_name
        !diff         TYPE REF TO /apmg/if_apm_arborist_diff
        !assignments  TYPE ty_package_assignments
      RETURNING
        VALUE(result) TYPE ty_changes
      RAISING
        /apmg/cx_apm_error.

    METHODS complete_change
      IMPORTING
        !assignments TYPE ty_package_assignments
      CHANGING
        !change      TYPE ty_change
      RAISING
        /apmg/cx_apm_error.

    METHODS preflight
      IMPORTING
        !registry  TYPE string
        !transport TYPE trkorr
        !is_force  TYPE abap_bool
      CHANGING
        !changes   TYPE ty_changes
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
        !is_force TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    METHODS check_semver
      IMPORTING
        !name     TYPE string
        !version  TYPE string
        !range    TYPE string
        !category TYPE string
        !is_force TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

    METHODS check_transport
      IMPORTING
        !package   TYPE devclass
        !transport TYPE trkorr
      RAISING
        /apmg/cx_apm_error.

    METHODS install_changes
      IMPORTING
        !registry  TYPE string
        !transport TYPE trkorr
        !changes   TYPE ty_changes
      RAISING
        /apmg/cx_apm_error.

    METHODS remove_changes
      IMPORTING
        !transport TYPE trkorr
        !changes   TYPE ty_changes
      RAISING
        /apmg/cx_apm_error.

    METHODS persist_manifest
      IMPORTING
        !package  TYPE devclass
        !manifest TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

    METHODS delete_manifest
      IMPORTING
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

    METHODS raise_error
      IMPORTING
        !text TYPE string
      RAISING
        /apmg/cx_apm_error.
ENDCLASS.



CLASS /apmg/cl_apm_command_install IMPLEMENTATION.


  METHOD check_package.

    IF package IS INITIAL.
      raise_error( |No SAP package was assigned to { name }| ).
    ENDIF.

    DATA(sap_package) = zcl_abapgit_factory=>get_sap_package( package ).
    IF sap_package->exists( ) = abap_false.
      raise_error( |SAP package { package } does not exist| ).
    ENDIF.

    IF line_exists( packages[ name = name ] ) ##PRIMKEY[NAME].
      raise_error( |Package "{ name }" is already installed in { packages[ name = name ]-package }| ) ##PRIMKEY[NAME].
    ENDIF.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).
    IF package_json_service->exists( ) = abap_true.
      DATA(existing_name) = package_json_service->get( )-name.
      raise_error( |{ package } already contains package "{ existing_name }"| ).
    ENDIF.

    SELECT COUNT(*) FROM tadir INTO @DATA(count) WHERE devclass = @package. "#EC CI_SGLSELECT
    IF count > 1.
      raise_error( |{ package } already contains { count } objects but must be empty| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_prerequisites.

    IF line_exists( manifest-engines[ key = 'apm' ] ).
      check_semver(
        name     = 'apm'
        version  = /apmg/if_apm_version=>c_version
        range    = manifest-engines[ key = 'apm' ]-range
        category = 'Engine'
        is_force = is_force ).
    ENDIF.

    IF line_exists( manifest-engines[ key = 'abap' ] ).
      check_semver(
        name     = 'ABAP'
        version  = /apmg/cl_apm_utils=>get_abap_version( )
        range    = manifest-engines[ key = 'abap' ]-range
        category = 'Engine'
        is_force = is_force ).
    ENDIF.

    DATA(db) = /apmg/cl_apm_utils=>get_database_platform( ).
    IF manifest-db IS NOT INITIAL AND NOT line_exists( manifest-db[ table_line = db ] ).
      raise_error( |Database platform "{ db }" is not supported with package { manifest-name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_semver.

    DATA(satisfies) = /apmg/cl_apm_semver_functions=>satisfies(
      version = version
      range   = range ).

    IF satisfies = abap_false AND is_force = abap_false.
      raise_error( |{ category } "{ name }" is installed in version { version } but does not satisfy { range }| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_transport.

    IF transport IS INITIAL.
      DATA(message) = /apmg/cl_apm_auth=>check_transport_required( package ).
      IF message IS NOT INITIAL.
        raise_error( |{ message }: { package }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD complete_change.

    CASE change-action.
      WHEN /apmg/if_apm_arborist=>c_diff_action-add.
        IF change-actual IS BOUND OR change-ideal IS NOT BOUND.
          raise_error( |Invalid ADD action for { change-name }| ).
        ENDIF.
        READ TABLE assignments ASSIGNING FIELD-SYMBOL(<assignment>)
          WITH TABLE KEY name = change-name.
        IF sy-subrc <> 0 OR <assignment>-package IS INITIAL.
          raise_error( |No SAP package was assigned to { change-name }| ).
        ENDIF.
        change-package    = <assignment>-package.
        change-to_version = change-ideal->version.
        change-manifest   = change-ideal->get_manifest( ).
        IF change-to_version IS INITIAL.
          raise_error( |ADD action for { change-name } has no target version| ).
        ENDIF.

      WHEN /apmg/if_apm_arborist=>c_diff_action-change.
        IF change-actual IS NOT BOUND
            OR change-ideal IS NOT BOUND
            OR change-actual->name <> change-ideal->name.
          raise_error( |Invalid CHANGE action for { change-name }| ).
        ENDIF.
        change-package      = change-actual->package.
        change-from_version = change-actual->version.
        change-to_version   = change-ideal->version.
        change-manifest     = change-ideal->get_manifest( ).
        IF change-package IS INITIAL.
          raise_error( |CHANGE action for { change-name } has no installed SAP package| ).
        ENDIF.
        IF change-from_version IS INITIAL OR change-to_version IS INITIAL.
          raise_error( |CHANGE action for { change-name } has an incomplete version| ).
        ENDIF.
        IF change-from_version = change-to_version.
          raise_error( |CHANGE action for { change-name } does not change the version| ).
        ENDIF.

      WHEN /apmg/if_apm_arborist=>c_diff_action-remove.
        IF change-actual IS NOT BOUND OR change-ideal IS BOUND.
          raise_error( |Invalid REMOVE action for { change-name }| ).
        ENDIF.
        change-package      = change-actual->package.
        change-from_version = change-actual->version.
        IF change-package IS INITIAL.
          raise_error( |REMOVE action for { change-name } has no installed SAP package| ).
        ENDIF.
        IF change-from_version IS INITIAL.
          raise_error( |REMOVE action for { change-name } has no installed version| ).
        ENDIF.

      WHEN OTHERS.
        raise_error( |Unknown install action for { change-name }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD delete_manifest.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory( package ).
    IF package_json_service->exists( ) = abap_true.
      package_json_service->delete( ).
    ENDIF.

    DATA(readme_service) = /apmg/cl_apm_readme=>factory( package ).
    IF readme_service->exists( ) = abap_true.
      readme_service->delete( ).
    ENDIF.

  ENDMETHOD.


  METHOD execute.

    /apmg/cl_apm_registry=>check_logged_in( registry ).

    packages = /apmg/cl_apm_package_json=>list(
      instanciate = abap_true
      is_bundle   = abap_false ).

    DATA(changes) = normalize_changes(
      root_name   = root_name
      diff        = diff
      assignments = assignments ).

    preflight(
      EXPORTING
        registry  = registry
        transport = transport
        is_force  = is_force
      CHANGING
        changes   = changes ).

    IF is_dry_run = abap_true.
      MESSAGE |Install dry run successful: { lines( changes ) } change(s)| TYPE 'S'.
      RETURN.
    ENDIF.

    install_changes(
      registry  = registry
      transport = transport
      changes   = changes ).

    remove_changes(
      transport = transport
      changes   = changes ).

    MESSAGE |Package successfully installed: { root_name }| TYPE 'S'.

  ENDMETHOD.


  METHOD install_changes.

    DATA(completed) = 0.

    LOOP AT changes ASSIGNING FIELD-SYMBOL(<change>)
        WHERE action = /apmg/if_apm_arborist=>c_diff_action-add
           OR action = /apmg/if_apm_arborist=>c_diff_action-change.

      TRY.
          IF <change>-action = /apmg/if_apm_arborist=>c_diff_action-change.
            /apmg/cl_apm_installer=>uninstall(
              name      = <change>-name
              version   = <change>-from_version
              package   = <change>-package
              transport = transport ).
          ENDIF.

          " FUTURE: Allow other folder logic than prefix
          /apmg/cl_apm_installer=>install(
            name              = <change>-name
            version           = <change>-to_version
            data              = <change>-tarball
            package           = <change>-package
            transport         = transport
            enum_source       = /apmg/cl_apm_installer=>c_enum_source-registry
            enum_folder_logic = /apmg/cl_apm_installer=>c_enum_folder_logic-prefix ).

          persist_manifest(
            package  = <change>-package
            manifest = <change>-manifest ).

          completed = completed + 1.

        CATCH /apmg/cx_apm_error INTO DATA(error).
          /apmg/cx_apm_error=>raise(
            text     = |{ <change>-action } failed for { <change>-name }@{ <change>-to_version } in | &&
                       |{ <change>-package } after { completed } completed ADD/CHANGE action(s)|
            previous = error ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD normalize_changes.

    IF diff IS NOT BOUND.
      raise_error( 'The Arborist diff is not available' ).
    ENDIF.

    DATA(diff_changes) = diff->get_changes( root_name ).
    IF diff_changes IS INITIAL.
      raise_error( |No install changes were found for { root_name }| ).
    ENDIF.

    DATA(root_found) = abap_false.

    LOOP AT diff_changes INTO DATA(diff_change).
      DATA(actual) = diff_change->get_actual( ).
      DATA(ideal)  = diff_change->get_ideal( ).
      DATA(action) = diff_change->get_action( ).
      DATA(name) = COND /apmg/if_apm_types=>ty_name(
        WHEN ideal IS BOUND THEN ideal->name
        WHEN actual IS BOUND THEN actual->name ).

      IF name IS INITIAL.
        raise_error( 'An install change has no registry package name' ).
      ENDIF.
      IF line_exists( result[ name = name ] ).
        raise_error( |Duplicate install action for { name }| ).
      ENDIF.

      DATA(change) = VALUE ty_change(
        sequence = sy-tabix
        action   = action
        name     = name
        actual   = actual
        ideal    = ideal ).

      complete_change(
        EXPORTING
          assignments = assignments
        CHANGING
          change      = change ).

      IF name = root_name AND action = /apmg/if_apm_arborist=>c_diff_action-add.
        root_found = abap_true.
      ENDIF.

      APPEND change TO result.
    ENDLOOP.

    IF root_found = abap_false.
      raise_error( |The requested installation of package { root_name } is not an ADD action| ).
    ENDIF.

    LOOP AT assignments ASSIGNING FIELD-SYMBOL(<assignment>).
      IF NOT line_exists( result[
        name   = <assignment>-name
        action = /apmg/if_apm_arborist=>c_diff_action-add ] ).
        raise_error( |Unexpected SAP package assignment for { <assignment>-name }| ).
      ENDIF.
    ENDLOOP.

    LOOP AT result ASSIGNING FIELD-SYMBOL(<left>) WHERE action = /apmg/if_apm_arborist=>c_diff_action-add.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<right>)
          WHERE action = /apmg/if_apm_arborist=>c_diff_action-add AND sequence > <left>-sequence.
        IF <left>-package = <right>-package.
          raise_error( |SAP package { <left>-package } is assigned to both { <left>-name } and { <right>-name }| ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD persist_manifest.

    DATA(package_json) = CORRESPONDING /apmg/if_apm_types=>ty_package_json( manifest ).
    DATA(markdown) = package_json-readme.
    IF markdown IS INITIAL.
      markdown = |# { package_json-name } - { package_json-description }|.
    ELSE.
      markdown = replace(
        val  = markdown
        sub  = '\n'
        with = cl_abap_char_utilities=>newline
        occ  = 0 ).
    ENDIF.
    CLEAR package_json-readme.

    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory(
      package = package
      name    = package_json-name
      version = package_json-version
      private = package_json-private ).
    package_json_service->set( package_json )->save( ).

    DATA(readme_service) = /apmg/cl_apm_readme=>factory(
      package  = package
      markdown = markdown ).
    readme_service->set( markdown )->save( ).

  ENDMETHOD.


  METHOD preflight.

    LOOP AT changes ASSIGNING FIELD-SYMBOL(<change>).
      check_transport(
        package   = <change>-package
        transport = transport ).

      CASE <change>-action.
        WHEN /apmg/if_apm_arborist=>c_diff_action-add.
          DATA(message) = /apmg/cl_apm_auth=>check_package_allowed( <change>-package ).
          IF message IS NOT INITIAL.
            raise_error( message ).
          ENDIF.
          /apmg/cl_apm_auth=>check_package_authorized(
            package  = <change>-package
            activity = /apmg/cl_apm_auth=>c_activity-create ).
          /apmg/cl_apm_auth=>check_package_authorized(
            package  = <change>-package
            activity = /apmg/cl_apm_auth=>c_activity-change ).
          check_package(
            package = <change>-package
            name    = <change>-name ).

        WHEN /apmg/if_apm_arborist=>c_diff_action-change.
          /apmg/cl_apm_auth=>check_package_authorized(
            package  = <change>-package
            activity = /apmg/cl_apm_auth=>c_activity-change ).

        WHEN /apmg/if_apm_arborist=>c_diff_action-remove.
          /apmg/cl_apm_auth=>check_package_authorized(
            package  = <change>-package
            activity = /apmg/cl_apm_auth=>c_activity-delete ).
      ENDCASE.

      IF <change>-action = /apmg/if_apm_arborist=>c_diff_action-add
          OR <change>-action = /apmg/if_apm_arborist=>c_diff_action-change.
        IF <change>-manifest-name <> <change>-name
            OR <change>-manifest-version <> <change>-to_version
            OR <change>-manifest-dist-tarball IS INITIAL.
          raise_error( |Incomplete target manifest for { <change>-name }@{ <change>-to_version }| ).
        ENDIF.

        check_prerequisites(
          manifest = <change>-manifest
          is_force = is_force ).

        <change>-tarball = /apmg/cl_apm_registry=>get_tarball(
          registry = registry
          name     = <change>-name
          tarball  = <change>-manifest-dist-tarball ).

        /apmg/cl_apm_integrity=>check(
          tarball = <change>-tarball
          dist    = <change>-manifest-dist ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD raise_error.

    RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = text.

  ENDMETHOD.


  METHOD remove_changes.

    DATA(removals) = changes.
    DELETE removals WHERE action <> /apmg/if_apm_arborist=>c_diff_action-remove.
    SORT removals BY sequence DESCENDING.

    DATA(completed) = 0.

    LOOP AT changes ASSIGNING FIELD-SYMBOL(<change>).
      TRY.
          /apmg/cl_apm_installer=>uninstall(
            name      = <change>-name
            version   = <change>-from_version
            package   = <change>-package
            transport = transport ).

          delete_manifest( <change>-package ).

          completed = completed + 1.

        CATCH /apmg/cx_apm_error INTO DATA(error).
          /apmg/cx_apm_error=>raise(
            text     = |REMOVE failed for { <change>-name }@{ <change>-from_version } in { <change>-package } | &&
                       |after { completed } completed REMOVE action(s)|
            previous = error ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_install( ).

    command->execute(
      registry    = /apmg/cl_apm_utils=>remove_trailing_slash( registry )
      root_name   = root_name
      diff        = diff
      assignments = assignments
      transport   = transport
      is_force    = is_force
      is_dry_run  = is_dry_run ).

  ENDMETHOD.
ENDCLASS.
