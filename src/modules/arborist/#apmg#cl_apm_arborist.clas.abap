CLASS /apmg/cl_apm_arborist DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Arborist
*
* Inspect and manage package trees. In ABAP, there's only one global
* tree containing all packages managed by apm.
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* https://www.npmjs.com/package/@npmcli/arborist
* https://github.com/npm/cli/tree/latest/workspaces/arborist
************************************************************************
  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_arborist.

    CLASS-METHODS factory
      IMPORTING
        !registry                  TYPE string
        !with_bundled_dependencies TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)              TYPE REF TO /apmg/if_apm_arborist.

    CLASS-METHODS injector
      IMPORTING
        !mock TYPE REF TO /apmg/if_apm_arborist.

    METHODS constructor
      IMPORTING
        !registry                  TYPE string
        !with_bundled_dependencies TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_visited,
        name TYPE /apmg/if_apm_types=>ty_name,
      END OF ty_visited,
      ty_visited_set TYPE HASHED TABLE OF ty_visited WITH UNIQUE KEY name.

    CLASS-DATA instance TYPE REF TO /apmg/if_apm_arborist.

    DATA registry TYPE string.
    DATA with_bundled_dependencies TYPE abap_bool.
    DATA log TYPE /apmg/if_apm_arborist=>ty_log.
    DATA visited TYPE ty_visited_set.
    DATA processing_stack TYPE string_table.

    "! Add a log entry
    METHODS add_log
      IMPORTING
        !type    TYPE string
        !message TYPE string
        !name    TYPE string OPTIONAL
        !version TYPE string OPTIONAL
        !spec    TYPE string OPTIONAL.

    "! Process a single package and its dependencies
    METHODS process_package
      IMPORTING
        !package_info TYPE /apmg/if_apm_package_json=>ty_package
        !depth        TYPE i DEFAULT 0.

    "! Process dependencies of a node
    METHODS process_dependencies
      IMPORTING
        !node  TYPE REF TO /apmg/cl_apm_arborist_node
        !depth TYPE i.

    "! Create edges for a dependency list
    METHODS create_edges
      IMPORTING
        !type                 TYPE /apmg/if_apm_arborist=>ty_dependency_type
        !node                 TYPE REF TO /apmg/cl_apm_arborist_node
        !dependencies         TYPE /apmg/if_apm_types=>ty_dependencies
        !bundled_dependencies TYPE /apmg/if_apm_types=>ty_bundled_dependencies OPTIONAL.

    "! Check for circular dependency
    METHODS is_circular
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Get manifest from pacote (cached locally if possible)
    METHODS get_manifest
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
        !version      TYPE /apmg/if_apm_types=>ty_version OPTIONAL
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json.

    "! Get list of available versions from manifest
    METHODS get_versions
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_versions.

ENDCLASS.



CLASS /apmg/cl_apm_arborist IMPLEMENTATION.


  METHOD /apmg/if_apm_arborist~build_ideal_tree.
    " TODO: Future implementation
  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~get_log.

    result = log.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~get_tree.

    result = /apmg/cl_apm_arborist_node=>get_all( ).

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~load_actual_tree.

    " Clear previous tree and state
    /apmg/cl_apm_arborist_node=>clear( ).
    CLEAR: log, visited, processing_stack.

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = 'Starting to load actual tree' ).

    " Step 1: Get all installed packages with their metadata
    DATA(packages) = /apmg/cl_apm_package_json=>list(
      instanciate = abap_true
      is_bundle   = abap_false ).

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = |Found { lines( packages ) } installed packages| ).

    " Step 2: Create nodes for all installed packages first
    " This ensures all nodes exist before we create edges
    LOOP AT packages ASSIGNING FIELD-SYMBOL(<package>).
      TRY.
          DATA(manifest) = <package>-instance->get( ).

          /apmg/cl_apm_arborist_node=>create(
            package   = <package>-package
            manifest  = manifest
            installed = abap_true ).

          INSERT VALUE #( name = <package>-name ) INTO TABLE visited.

        CATCH /apmg/cx_apm_error INTO DATA(error).
          add_log(
            type    = /apmg/if_apm_arborist=>c_log_type-warning
            message = |Error loading package { <package>-name }: { error->get_text( ) }|
            name    = <package>-name
            version = <package>-version ).
      ENDTRY.
    ENDLOOP.

    " Step 3: Process dependencies for each installed package
    " Now that all nodes exist, create edges
    LOOP AT packages ASSIGNING <package>.
      process_package(
        package_info = <package>
        depth        = 0 ).
    ENDLOOP.

    " Step 4: Process uninstalled dependencies recursively
    " This finds dependencies that are declared but not installed
    DATA(max_iterations) = 5.
    DATA(iteration) = 0.
    DATA(nodes_to_process) = VALUE /apmg/if_apm_arborist~ty_node_refs( ).

    DO.
      iteration = iteration + 1.
      IF iteration > max_iterations.
        add_log(
          type    = /apmg/if_apm_arborist=>c_log_type-warning
          message = |Stopped processing after { max_iterations } iterations to prevent infinite loop| ).
        EXIT.
      ENDIF.

      " Find nodes with unresolved dependencies
      CLEAR nodes_to_process.
      DATA(all_nodes) = /apmg/cl_apm_arborist_node=>get_all( ).

      LOOP AT all_nodes ASSIGNING FIELD-SYMBOL(<node>).
        LOOP AT <node>->edges_out ASSIGNING FIELD-SYMBOL(<edge>).
          IF <edge>->is_missing( ).
            " Check if we haven't visited this dependency yet
            IF NOT line_exists( visited[ name = <edge>->name ] ).
              " Try to get manifest from registry for uninstalled dependency
              DATA(uninstalled_manifest) = get_manifest( name = <edge>->name ).
              IF uninstalled_manifest IS NOT INITIAL.
                " Create placeholder node for uninstalled package
                DATA(new_node) = /apmg/cl_apm_arborist_node=>create(
                  manifest  = uninstalled_manifest
                  installed = abap_false ).

                INSERT VALUE #( name = <edge>->name ) INTO TABLE visited.
                INSERT new_node INTO TABLE nodes_to_process.

                add_log(
                  type    = /apmg/if_apm_arborist=>c_log_type-warning
                  message = |Dependency { <edge>->name }@{ <edge>->spec } is not installed|
                  name    = <edge>->name
                  spec    = <edge>->spec ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      " Process dependencies of newly added nodes
      IF nodes_to_process IS INITIAL.
        EXIT. " No more unresolved dependencies
      ENDIF.

      LOOP AT nodes_to_process ASSIGNING FIELD-SYMBOL(<new_node>).
        process_dependencies(
          node  = <new_node>
          depth = iteration ).
      ENDLOOP.
    ENDDO.

    " Step 5: Re-resolve all edges now that all nodes are created
    DATA(final_nodes) = /apmg/cl_apm_arborist_node=>get_all( ).

    LOOP AT final_nodes ASSIGNING <node>.
      LOOP AT <node>->edges_out ASSIGNING <edge>.
        <edge>->resolve( ).
      ENDLOOP.

      " Aggregate required versions from incoming edges and check satisfaction
      DATA(required_specs) = VALUE string_table( ).
      DATA(all_satisfied)  = abap_true.
      DATA(max_satisfying) = <node>->version.

      LOOP AT <node>->edges_in ASSIGNING <edge>.
        " Collect all specs from incoming edges
        INSERT <edge>->spec INTO TABLE required_specs.

        " Check if current node version satisfies this requirement
        IF <node>->satisfies( <edge>->spec ) = abap_false.
          all_satisfied = abap_false.
        ENDIF.
      ENDLOOP.

      " Determine max_satisfying: if current version satisfies all requirements, use it
      " Otherwise, max_satisfying needs to be calculated from available versions in registry
      IF all_satisfied = abap_false AND required_specs IS NOT INITIAL.
        DATA(available_versions) = get_versions( <node>->name ).

        " Current version doesn't satisfy all requirements
        " max_satisfying would ideally be the maximum version from registry that satisfies all specs
        max_satisfying = <node>->max_satisfying(
          versions = available_versions
          specs    = required_specs ).
      ENDIF.

      <node>->set_max_satisfying( max_satisfying ).
    ENDLOOP.

    " Log summary
    DATA(total_nodes) = lines( final_nodes ).
    DATA(installed_count) = 0.
    DATA(missing_count) = 0.
    DATA(invalid_count) = 0.

    LOOP AT final_nodes ASSIGNING <node>.
      IF <node>->installed = abap_true.
        installed_count = installed_count + 1.
      ENDIF.
      LOOP AT <node>->edges_out ASSIGNING <edge>.
        IF <edge>->is_missing( ).
          missing_count = missing_count + 1.
        ELSEIF <edge>->is_invalid( ).
          invalid_count = invalid_count + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = |Tree complete: { total_nodes } nodes, { installed_count } installed, |
                && |{ missing_count } missing deps, { invalid_count } invalid deps| ).

    result = final_nodes.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~load_virtual_tree.
    " TODO: Future implementation - read from package-lock.abap.json
  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~reify_tree.
    " TODO: Future implementation
  ENDMETHOD.


  METHOD add_log.

    DATA(entry) = VALUE /apmg/if_apm_arborist=>ty_log_entry(
      type    = type
      message = message
      name    = name
      version = version
      spec    = spec ).

    INSERT entry INTO TABLE log.

  ENDMETHOD.


  METHOD constructor.

    me->registry                  = registry.
    me->with_bundled_dependencies = with_bundled_dependencies.

  ENDMETHOD.


  METHOD create_edges.

    IF node IS NOT BOUND OR dependencies IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT dependencies ASSIGNING FIELD-SYMBOL(<dep>).
      IF with_bundled_dependencies = abap_false AND line_exists( bundled_dependencies[ table_line = <dep>-key ] ).
        CONTINUE.
      ENDIF.

      " Create edge (constructor automatically resolves target)
      /apmg/cl_apm_arborist_edge=>create(
        from = node
        type = type
        name = <dep>-key
        spec = <dep>-range ).
    ENDLOOP.

  ENDMETHOD.


  METHOD factory.

    IF instance IS INITIAL.
      result = NEW /apmg/cl_apm_arborist( registry ).
    ELSE.
      result = instance.
    ENDIF.

  ENDMETHOD.


  METHOD get_manifest.

    " First try to get from already loaded node
    DATA(existing_node) = /apmg/cl_apm_arborist_node=>get_by_name( name ).

    IF existing_node IS BOUND.
      result-name                  = existing_node->name.
      result-version               = existing_node->version.
      result-dependencies          = existing_node->deps_prod.
      result-dev_dependencies      = existing_node->deps_dev.
      result-optional_dependencies = existing_node->deps_optional.
      result-peer_dependencies     = existing_node->deps_peer.
      result-bundle_dependencies   = existing_node->deps_bundled.
      RETURN.
    ENDIF.

    " Try to get from pacote (registry cache)
    TRY.
        DATA(pacote) = /apmg/cl_apm_pacote=>factory(
          registry = registry
          name     = name ).

        IF pacote->exists( ).
          DATA(packument) = pacote->get( ).

          IF version IS NOT INITIAL.
            DATA(manifest) = pacote->get_version( version ).
            result = CORRESPONDING #( manifest ).
          ELSEIF packument-dist_tags IS NOT INITIAL.
            " Get latest version
            READ TABLE packument-dist_tags ASSIGNING FIELD-SYMBOL(<tag>)
              WITH KEY key = 'latest'.
            IF sy-subrc = 0.
              manifest = pacote->get_version( <tag>-value ).
              result = CORRESPONDING #( manifest ).
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH /apmg/cx_apm_error INTO DATA(error).
        add_log(
          type    = /apmg/if_apm_arborist=>c_log_type-warning
          message = |Could not fetch manifest for { name }: { error->get_text( ) }|
          name    = name ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_versions.

    TRY.
        DATA(pacote) = /apmg/cl_apm_pacote=>factory(
          registry = registry
          name     = name ).

        pacote->packument( ).

        result = pacote->get_versions( ).

      CATCH /apmg/cx_apm_error INTO DATA(error).
        add_log(
          type    = /apmg/if_apm_arborist=>c_log_type-warning
          message = |Could not fetch packument for { name }: { error->get_text( ) }|
          name    = name ).
    ENDTRY.

  ENDMETHOD.


  METHOD injector.

    instance = mock.

  ENDMETHOD.


  METHOD is_circular.

    result = xsdbool( line_exists( processing_stack[ table_line = name ] ) ).

  ENDMETHOD.


  METHOD process_dependencies.

    IF node IS NOT BOUND.
      RETURN.
    ENDIF.

    " Check for circular dependency
    IF is_circular( node->name ).
      add_log(
        type    = /apmg/if_apm_arborist=>c_log_type-circular
        message = |Circular dependency detected: { node->name }|
        name    = node->name ).
      RETURN.
    ENDIF.

    " Add to processing stack
    INSERT node->name INTO TABLE processing_stack.

    " Create edges for production dependencies
    create_edges(
      node                 = node
      dependencies         = node->deps_prod
      bundled_dependencies = node->deps_bundled
      type                 = /apmg/if_apm_arborist=>c_dependency_type-prod ).

    " Create edges for dev dependencies
    create_edges(
      node                 = node
      dependencies         = node->deps_dev
      bundled_dependencies = node->deps_bundled
      type                 = /apmg/if_apm_arborist=>c_dependency_type-dev ).

    " Create edges for optional dependencies
    create_edges(
      node         = node
      dependencies = node->deps_optional
      type         = /apmg/if_apm_arborist=>c_dependency_type-optional ).

    " Create edges for peer dependencies
    create_edges(
      node         = node
      dependencies = node->deps_peer
      type         = /apmg/if_apm_arborist=>c_dependency_type-peer ).

    " Remove from processing stack
    DELETE processing_stack WHERE table_line = node->name.

  ENDMETHOD.


  METHOD process_package.

    " Skip if no instance or no name
    IF package_info-instance IS NOT BOUND OR package_info-name IS INITIAL.
      RETURN.
    ENDIF.

    " Get the node from the tree
    DATA(node) = /apmg/cl_apm_arborist_node=>get_by_name( package_info-name ).

    IF node IS INITIAL.
      RETURN.
    ENDIF.

    " Process dependencies
    process_dependencies(
      node  = node
      depth = depth ).

  ENDMETHOD.
ENDCLASS.
