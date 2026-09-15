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
        !registry                 TYPE string
        !with_bundle_dependencies TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)             TYPE REF TO /apmg/if_apm_arborist.

    CLASS-METHODS injector
      IMPORTING
        !mock TYPE REF TO /apmg/if_apm_arborist OPTIONAL.

    METHODS constructor
      IMPORTING
        !registry                 TYPE string
        !with_bundle_dependencies TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_max_depth TYPE i VALUE 10.
    CONSTANTS c_max_iterations TYPE i VALUE 20.

    TYPES:
      BEGIN OF ty_visited,
        name TYPE /apmg/if_apm_types=>ty_name,
      END OF ty_visited,
      ty_visited_set TYPE HASHED TABLE OF ty_visited WITH UNIQUE KEY name.

    CLASS-DATA injected_mock TYPE REF TO /apmg/if_apm_arborist.

    DATA registry TYPE string.
    DATA with_bundle_dependencies TYPE abap_bool.
    DATA log TYPE /apmg/if_apm_arborist=>ty_log.
    DATA visited TYPE ty_visited_set.
    DATA processing_stack TYPE string_table.
    DATA current_tree TYPE REF TO /apmg/cl_apm_arborist_tree.
    DATA ideal_tree TYPE REF TO /apmg/cl_apm_arborist_tree.
    DATA is_production TYPE abap_bool.
    DATA actual_loaded TYPE abap_bool.

    METHODS add_log
      IMPORTING
        !type     TYPE string
        !category TYPE string OPTIONAL
        !message  TYPE string
        !name     TYPE string OPTIONAL
        !version  TYPE string OPTIONAL
        !spec     TYPE string OPTIONAL.

    METHODS process_package
      IMPORTING
        !tree    TYPE REF TO /apmg/cl_apm_arborist_tree
        !package TYPE /apmg/if_apm_package_json=>ty_package
        !depth   TYPE i DEFAULT 0.

    METHODS process_dependencies
      IMPORTING
        !tree  TYPE REF TO /apmg/cl_apm_arborist_tree
        !node  TYPE REF TO /apmg/cl_apm_arborist_node
        !depth TYPE i.

    METHODS resolve
      IMPORTING
        !tree         TYPE REF TO /apmg/cl_apm_arborist_tree
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_arborist=>ty_node_refs.

    METHODS create_edges
      IMPORTING
        !tree         TYPE REF TO /apmg/cl_apm_arborist_tree
        !type         TYPE /apmg/if_apm_arborist=>ty_dependency_type
        !node         TYPE REF TO /apmg/cl_apm_arborist_node
        !dependencies TYPE /apmg/if_apm_types=>ty_dependencies.

    METHODS is_circular
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS get_manifest
      IMPORTING
        !tree         TYPE REF TO /apmg/cl_apm_arborist_tree
        !name         TYPE /apmg/if_apm_types=>ty_name
        !version      TYPE /apmg/if_apm_types=>ty_version OPTIONAL
        !exact        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_manifest
      RAISING
        /apmg/cx_apm_error.

    METHODS get_versions
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_versions.

    METHODS validate_add_packages
      IMPORTING
        !add_packages TYPE /apmg/if_apm_arborist=>ty_add_packages
      RAISING
        /apmg/cx_apm_error.

    METHODS validate_remove_packages
      IMPORTING
        !remove_packages TYPE string_table
      RAISING
        /apmg/cx_apm_error.

    METHODS apply_removals
      IMPORTING
        !remove_packages TYPE string_table.

    METHODS apply_additions
      IMPORTING
        !add_packages TYPE /apmg/if_apm_arborist=>ty_add_packages
      RAISING
        /apmg/cx_apm_error.

    METHODS prune_exclusive_deps
      IMPORTING
        !remove_packages TYPE string_table.

    METHODS rebuild_tree
      IMPORTING
        !tree TYPE REF TO /apmg/cl_apm_arborist_tree
      RAISING
        /apmg/cx_apm_error.

    METHODS add_missing_nodes
      IMPORTING
        !tree         TYPE REF TO /apmg/cl_apm_arborist_tree
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS select_versions
      IMPORTING
        !tree         TYPE REF TO /apmg/cl_apm_arborist_tree
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS has_bundle_name
      IMPORTING
        !node         TYPE REF TO /apmg/cl_apm_arborist_node
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS raise_error
      IMPORTING
        !message TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_arborist IMPLEMENTATION.


  METHOD /apmg/if_apm_arborist~build_ideal_tree.

    me->is_production = is_production.

    IF actual_loaded = abap_false.
      /apmg/if_apm_arborist~load_actual_tree( ).
    ENDIF.

    CLEAR log.

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = 'Starting to build ideal tree' ).

    ideal_tree = current_tree->clone( ).

    validate_add_packages( add_packages ).
    validate_remove_packages( remove_packages ).

    apply_removals( remove_packages ).
    apply_additions( add_packages ).

    rebuild_tree( ideal_tree ).

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = |Ideal tree built: { lines( ideal_tree->get_all( ) ) } nodes| ).

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~get_current_tree.

    IF current_tree IS BOUND.
      result = current_tree->get_all( ).
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~get_diff.

    IF current_tree IS NOT BOUND OR ideal_tree IS NOT BOUND.
      RETURN.
    ENDIF.

    result = /apmg/cl_apm_arborist_diff=>calculate(
      actual = current_tree
      ideal  = ideal_tree ).

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~get_ideal_tree.

    IF ideal_tree IS BOUND.
      result = ideal_tree->get_all( ).
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~get_log.

    result = log.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~is_executable.

    result = abap_true.
    LOOP AT log TRANSPORTING NO FIELDS
        WHERE type = /apmg/if_apm_arborist=>c_log_type-error.
      result = abap_false.
      RETURN.
    ENDLOOP.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~load_actual_tree.

    current_tree = NEW /apmg/cl_apm_arborist_tree( ).
    current_tree->clear( ).
    ideal_tree = NEW /apmg/cl_apm_arborist_tree( ).
    actual_loaded = abap_true.

    CLEAR: log, visited, processing_stack.

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = 'Starting to load actual tree' ).

    DATA(packages) = /apmg/cl_apm_package_json=>list(
      instanciate = abap_true
      is_bundle   = abap_false ).

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = |Found { lines( packages ) } installed packages| ).

    LOOP AT packages ASSIGNING FIELD-SYMBOL(<package>).
      TRY.
          DATA(package_json) = <package>-instance->get( ).
          DATA(manifest) = CORRESPONDING /apmg/if_apm_types=>ty_manifest( package_json ).

          current_tree->add_node(
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

    LOOP AT packages ASSIGNING <package>.
      process_package(
        tree    = current_tree
        package = <package>
        depth   = 0 ).
    ENDLOOP.

    DATA(final_nodes) = resolve( current_tree ).

    DATA(total_nodes)     = lines( final_nodes ).
    DATA(installed_count) = 0.
    DATA(missing_count)   = 0.
    DATA(invalid_count)   = 0.

    LOOP AT final_nodes ASSIGNING FIELD-SYMBOL(<node>).
      IF <node>->installed = abap_true.
        installed_count = installed_count + 1.
      ENDIF.
      LOOP AT <node>->edges_out ASSIGNING FIELD-SYMBOL(<edge>).
        IF <edge>->is_missing( ).
          missing_count = missing_count + 1.
        ELSEIF <edge>->is_invalid( ).
          invalid_count = invalid_count + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    add_log(
      type    = /apmg/if_apm_arborist=>c_log_type-info
      message = |Actual tree complete: { total_nodes } nodes, { installed_count } installed, |
                && |{ missing_count } missing deps, { invalid_count } invalid deps| ).

    result = final_nodes.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~load_virtual_tree.
    ASSERT 0 = 1.
  ENDMETHOD.


  METHOD /apmg/if_apm_arborist~reify_tree.
    ASSERT 0 = 1.
  ENDMETHOD.


  METHOD add_log.

    INSERT VALUE #(
      type     = type
      category = category
      message  = message
      name     = name
      version  = version
      spec     = spec ) INTO TABLE log.

  ENDMETHOD.


  METHOD apply_additions.

    LOOP AT add_packages ASSIGNING FIELD-SYMBOL(<add>).
      TRY.
          DATA(manifest) = get_manifest(
            tree    = ideal_tree
            name    = <add>-name
            version = <add>-version
            exact   = abap_true ).
        CATCH /apmg/cx_apm_error INTO DATA(manifest_error).
          add_log(
            type     = /apmg/if_apm_arborist=>c_log_type-error
            category = /apmg/if_apm_arborist=>c_diagnostic_category-requested_version_not_found
            message  = manifest_error->get_text( )
            name     = <add>-name
            version  = <add>-version ).
          raise_error( manifest_error->get_text( ) ).
      ENDTRY.

      DATA(existing_node) = ideal_tree->get_by_name( <add>-name ).
      IF existing_node IS BOUND.
        existing_node->update_manifest( manifest ).
      ELSE.
        ideal_tree->add_node(
          manifest  = manifest
          installed = abap_false ).
      ENDIF.

      add_log(
        type    = /apmg/if_apm_arborist=>c_log_type-info
        message = |Added { <add>-name }@{ <add>-version } to ideal tree|
        name    = <add>-name
        version = <add>-version ).
    ENDLOOP.

  ENDMETHOD.


  METHOD apply_removals.

    IF remove_packages IS INITIAL.
      RETURN.
    ENDIF.

    prune_exclusive_deps( remove_packages ).

  ENDMETHOD.


  METHOD add_missing_nodes.

    DATA(missing_names) = VALUE string_table( ).

    LOOP AT tree->get_all( ) INTO DATA(source_node).
      LOOP AT source_node->edges_out INTO DATA(source_edge).
        IF source_edge->is_missing( ) = abap_true.
          INSERT source_edge->name INTO TABLE missing_names.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT missing_names.
    DELETE ADJACENT DUPLICATES FROM missing_names.

    LOOP AT missing_names ASSIGNING FIELD-SYMBOL(<missing_name>).
      DATA(specs) = VALUE string_table( ).
      DATA(mandatory_specs) = VALUE string_table( ).
      DATA(optional_specs) = VALUE string_table( ).
      DATA(has_install_dependency) = abap_false.
      DATA(has_peer_dependency) = abap_false.
      DATA(is_optional_only) = abap_true.

      LOOP AT tree->get_all( ) INTO source_node.
        LOOP AT source_node->edges_out INTO source_edge.
          IF source_edge->name <> <missing_name>.
            CONTINUE.
          ENDIF.
          DATA(version_helper) = source_edge->from.
          IF source_edge->type = /apmg/if_apm_arborist=>c_dependency_type-peer.
            has_peer_dependency = abap_true.
            INSERT source_edge->spec INTO TABLE mandatory_specs.
          ELSE.
            has_install_dependency = abap_true.
            IF source_edge->type = /apmg/if_apm_arborist=>c_dependency_type-optional.
              INSERT source_edge->spec INTO TABLE optional_specs.
            ELSE.
              INSERT source_edge->spec INTO TABLE mandatory_specs.
              is_optional_only = abap_false.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      IF is_optional_only = abap_true AND has_peer_dependency = abap_false.
        specs = optional_specs.
      ELSE.
        specs = mandatory_specs.
      ENDIF.

      IF has_install_dependency = abap_false AND has_peer_dependency = abap_true.
        add_log(
          type     = /apmg/if_apm_arborist=>c_log_type-error
          category = /apmg/if_apm_arborist=>c_diagnostic_category-peer_dependency
          message  = |Peer dependency { <missing_name> } is not present in the global tree|
          name     = <missing_name>
          spec     = concat_lines_of( table = specs sep = ` ` ) ).
        CONTINUE.
      ENDIF.

      DATA(available_versions) = get_versions( <missing_name> ).
      IF available_versions IS INITIAL.
        add_log(
          type     = COND #( WHEN is_optional_only = abap_true
                             THEN /apmg/if_apm_arborist=>c_log_type-warning
                             ELSE /apmg/if_apm_arborist=>c_log_type-error )
          category = /apmg/if_apm_arborist=>c_diagnostic_category-manifest_unavailable
          message  = |No registry versions are available for { <missing_name> }|
          name     = <missing_name>
          spec     = concat_lines_of( table = specs sep = ` ` ) ).
        CONTINUE.
      ENDIF.
      DATA(selected_version) = version_helper->max_satisfying(
        versions = available_versions
        specs    = specs ).

      IF selected_version IS INITIAL.
        add_log(
          type     = COND #( WHEN is_optional_only = abap_true
                             THEN /apmg/if_apm_arborist=>c_log_type-warning
                             ELSE /apmg/if_apm_arborist=>c_log_type-error )
          category = /apmg/if_apm_arborist=>c_diagnostic_category-no_satisfying_version
          message  = |No version of { <missing_name> } satisfies all incoming ranges|
          name     = <missing_name>
          spec     = concat_lines_of( table = specs sep = ` ` ) ).
        CONTINUE.
      ENDIF.

      TRY.
          DATA(manifest) = get_manifest(
            tree    = tree
            name    = <missing_name>
            version = selected_version
            exact   = abap_true ).
          tree->add_node(
            manifest  = manifest
            installed = abap_false ).
          result = abap_true.
        CATCH /apmg/cx_apm_error INTO DATA(manifest_error).
          add_log(
            type     = COND #( WHEN is_optional_only = abap_true
                               THEN /apmg/if_apm_arborist=>c_log_type-warning
                               ELSE /apmg/if_apm_arborist=>c_log_type-error )
            category = /apmg/if_apm_arborist=>c_diagnostic_category-manifest_unavailable
            message  = manifest_error->get_text( )
            name     = <missing_name>
            version  = selected_version ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    me->registry                 = registry.
    me->with_bundle_dependencies = with_bundle_dependencies.
    current_tree                 = NEW /apmg/cl_apm_arborist_tree( ).
    ideal_tree                   = NEW /apmg/cl_apm_arborist_tree( ).

  ENDMETHOD.


  METHOD create_edges.

    IF node IS NOT BOUND OR dependencies IS INITIAL OR tree IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT dependencies ASSIGNING FIELD-SYMBOL(<dep>).
      IF with_bundle_dependencies = abap_false
          AND has_bundle_name( node = node name = <dep>-key ) = abap_true.
        CONTINUE.
      ENDIF.
      /apmg/cl_apm_arborist_edge=>create(
        tree = tree
        from = node
        type = type
        name = <dep>-key
        spec = <dep>-range ).
    ENDLOOP.

  ENDMETHOD.


  METHOD factory.

    IF injected_mock IS BOUND.
      result = injected_mock.
      RETURN.
    ENDIF.

    result = NEW /apmg/cl_apm_arborist(
      registry                 = registry
      with_bundle_dependencies = with_bundle_dependencies ).

  ENDMETHOD.


  METHOD get_manifest.

    IF tree IS BOUND.
      DATA(existing_node) = tree->get_by_name( name ).
      IF existing_node IS BOUND AND exact = abap_false.
        result = existing_node->get_manifest( ).
        RETURN.
      ENDIF.
    ENDIF.

    TRY.
        DATA(pacote) = /apmg/cl_apm_pacote=>factory(
          registry = registry
          name     = name ).

        IF pacote->get( ) IS INITIAL.
          pacote->packument( ).
        ENDIF.

        IF exact = abap_true.
          IF version IS INITIAL.
            raise_error( |Exact version required for { name }| ).
          ENDIF.
          DATA(exact_manifest) = pacote->get_version( version ).
          IF exact_manifest IS INITIAL.
            raise_error( |Version { version } not found for { name }| ).
          ENDIF.
          result = exact_manifest.
          RETURN.
        ENDIF.

        DATA(packument) = pacote->get( ).

        IF version IS NOT INITIAL.
          DATA(version_manifest) = pacote->get_version( version ).
          result = CORRESPONDING #( version_manifest ).
        ELSEIF packument-dist_tags IS NOT INITIAL.
          READ TABLE packument-dist_tags ASSIGNING FIELD-SYMBOL(<tag>)
            WITH KEY key = 'latest'.
          IF sy-subrc = 0.
            version_manifest = pacote->get_version( <tag>-value ).
            result = version_manifest.
          ENDIF.
        ENDIF.

      CATCH /apmg/cx_apm_error INTO DATA(error).
        IF exact = abap_true.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = error->get_text( ).
        ENDIF.
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


  METHOD has_bundle_name.

    IF node IS BOUND.
      result = xsdbool( line_exists( node->bundle_dependencies[ table_line = name ] ) ).
    ENDIF.

  ENDMETHOD.


  METHOD injector.

    injected_mock = mock.

  ENDMETHOD.


  METHOD is_circular.

    result = xsdbool( line_exists( processing_stack[ table_line = name ] ) ).

  ENDMETHOD.


  METHOD process_dependencies.

    IF node IS NOT BOUND OR tree IS NOT BOUND.
      RETURN.
    ENDIF.

    IF is_circular( node->name ).
      add_log(
        type    = /apmg/if_apm_arborist=>c_log_type-circular
        message = |Circular dependency detected: { node->name }|
        name    = node->name ).
      RETURN.
    ENDIF.

    IF depth > c_max_depth.
      add_log(
        type    = /apmg/if_apm_arborist=>c_log_type-depth
        message = |Maximum depth reached: { node->name }|
        name    = node->name ).
      RETURN.
    ENDIF.

    INSERT node->name INTO TABLE processing_stack.

    create_edges(
      tree         = tree
      node         = node
      dependencies = node->dependencies
      type         = /apmg/if_apm_arborist=>c_dependency_type-prod ).

    IF is_production = abap_false.
      create_edges(
        tree         = tree
        node         = node
        dependencies = node->dev_dependencies
        type         = /apmg/if_apm_arborist=>c_dependency_type-dev ).
    ENDIF.

    create_edges(
      tree         = tree
      node         = node
      dependencies = node->optional_dependencies
      type         = /apmg/if_apm_arborist=>c_dependency_type-optional ).

    create_edges(
      tree         = tree
      node         = node
      dependencies = node->peer_dependencies
      type         = /apmg/if_apm_arborist=>c_dependency_type-peer ).

    DELETE processing_stack WHERE table_line = node->name.

  ENDMETHOD.


  METHOD process_package.

    IF package-instance IS NOT BOUND OR package-name IS INITIAL OR tree IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(node) = tree->get_by_name( package-name ).

    IF node IS INITIAL.
      RETURN.
    ENDIF.

    process_dependencies(
      tree  = tree
      node  = node
      depth = depth + 1 ).

  ENDMETHOD.


  METHOD prune_exclusive_deps.

    TYPES:
      BEGIN OF ty_remove_entry,
        name TYPE /apmg/if_apm_types=>ty_name,
      END OF ty_remove_entry,
      ty_remove_set TYPE HASHED TABLE OF ty_remove_entry WITH UNIQUE KEY name.

    DATA(remove_set) = VALUE ty_remove_set( ).

    LOOP AT remove_packages ASSIGNING FIELD-SYMBOL(<remove>).
      INSERT VALUE #( name = <remove> ) INTO TABLE remove_set.
    ENDLOOP.

    DATA(changed) = abap_true.
    WHILE changed = abap_true.
      changed = abap_false.

      LOOP AT ideal_tree->get_all( ) INTO DATA(prune_node).
        IF line_exists( remove_set[ name = prune_node->name ] ).
          CONTINUE.
        ENDIF.

        IF prune_node->edges_in IS INITIAL.
          CONTINUE.
        ENDIF.

        DATA(all_from_removed) = abap_true.
        LOOP AT prune_node->edges_in INTO DATA(prune_edge).
          IF prune_edge->from IS BOUND AND NOT line_exists( remove_set[ name = prune_edge->from->name ] ).
            all_from_removed = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF all_from_removed = abap_true.
          INSERT VALUE #( name = prune_node->name ) INTO TABLE remove_set.
          changed = abap_true.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    LOOP AT remove_set ASSIGNING FIELD-SYMBOL(<entry>).
      ideal_tree->remove_node( <entry>-name ).
      add_log(
        type    = /apmg/if_apm_arborist=>c_log_type-info
        message = |Removed { <entry>-name } from ideal tree|
        name    = <entry>-name ).
    ENDLOOP.

  ENDMETHOD.


  METHOD raise_error.

    RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.

  ENDMETHOD.


  METHOD rebuild_tree.

    IF tree IS NOT BOUND.
      RETURN.
    ENDIF.

    CLEAR: visited, processing_stack.

    DATA(stable) = abap_false.

    DO c_max_iterations TIMES.
      tree->clear_all_edges( ).

      LOOP AT tree->get_all( ) INTO DATA(clear_node).
        clear_node->clear_errors( ).
      ENDLOOP.

      LOOP AT tree->get_all( ) INTO DATA(rebuild_node).
        process_dependencies(
          tree  = tree
          node  = rebuild_node
          depth = 0 ).
      ENDLOOP.

      DATA(nodes_changed) = add_missing_nodes( tree ).
      DATA(versions_changed) = select_versions( tree ).

      IF nodes_changed = abap_false AND versions_changed = abap_false.
        stable = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    IF stable = abap_false.
      add_log(
        type     = /apmg/if_apm_arborist=>c_log_type-error
        category = /apmg/if_apm_arborist=>c_diagnostic_category-resolution_limit
        message  = |Ideal tree did not stabilize after { c_max_iterations } iterations| ).
      RETURN.
    ENDIF.

    tree->clear_all_edges( ).
    LOOP AT tree->get_all( ) INTO rebuild_node.
      process_dependencies(
        tree  = tree
        node  = rebuild_node
        depth = 0 ).
    ENDLOOP.
    resolve( tree ).

  ENDMETHOD.


  METHOD resolve.

    IF tree IS NOT BOUND.
      RETURN.
    ENDIF.

    result = tree->get_all( ).

    LOOP AT result INTO DATA(resolve_node).
      LOOP AT resolve_node->edges_out ASSIGNING FIELD-SYMBOL(<edge>).
        <edge>->resolve( tree ).

        IF <edge>->is_invalid( ).
          IF <edge>->type = /apmg/if_apm_arborist=>c_dependency_type-optional.
            add_log(
              type     = /apmg/if_apm_arborist=>c_log_type-warning
              category = /apmg/if_apm_arborist=>c_diagnostic_category-no_satisfying_version
              message  = <edge>->get_error_description( )
              name     = <edge>->name
              spec     = <edge>->spec ).
          ELSE.
            resolve_node->add_error( |Dependency "{ <edge>->name }" does not match specs| ).
            add_log(
              type     = /apmg/if_apm_arborist=>c_log_type-error
              category = COND #( WHEN <edge>->type = /apmg/if_apm_arborist=>c_dependency_type-peer
                                 THEN /apmg/if_apm_arborist=>c_diagnostic_category-peer_dependency
                                 ELSE /apmg/if_apm_arborist=>c_diagnostic_category-no_satisfying_version )
              message  = <edge>->get_error_description( )
              name     = <edge>->name
              spec     = <edge>->spec ).
          ENDIF.
        ELSEIF <edge>->is_missing( ).
          CASE <edge>->type.
            WHEN /apmg/if_apm_arborist=>c_dependency_type-optional.
              add_log(
                type    = /apmg/if_apm_arborist=>c_log_type-warning
                message = |Optional dependency "{ <edge>->name }" is not installed|
                name    = <edge>->name
                spec    = <edge>->spec ).
            WHEN /apmg/if_apm_arborist=>c_dependency_type-peer.
              resolve_node->add_error( |Peer dependency "{ <edge>->name }" is not installed| ).
              add_log(
                type     = /apmg/if_apm_arborist=>c_log_type-error
                category = /apmg/if_apm_arborist=>c_diagnostic_category-peer_dependency
                message  = <edge>->get_error_description( )
                name     = <edge>->name
                spec     = <edge>->spec ).
            WHEN OTHERS.
              resolve_node->add_error( |Dependency "{ <edge>->name }" is not installed| ).
              add_log(
                type     = /apmg/if_apm_arborist=>c_log_type-error
                category = /apmg/if_apm_arborist=>c_diagnostic_category-manifest_unavailable
                message  = <edge>->get_error_description( )
                name     = <edge>->name
                spec     = <edge>->spec ).
          ENDCASE.
        ENDIF.
      ENDLOOP.

      DATA(required_specs) = VALUE string_table( ).
      DATA(all_satisfied)  = abap_true.
      DATA(max_satisfying) = resolve_node->version.

      LOOP AT resolve_node->edges_in ASSIGNING <edge>.
        IF <edge>->type = /apmg/if_apm_arborist=>c_dependency_type-optional.
          CONTINUE.
        ENDIF.
        INSERT <edge>->spec INTO TABLE required_specs.

        IF resolve_node->satisfies( <edge>->spec ) = abap_false.
          all_satisfied = abap_false.
        ENDIF.
      ENDLOOP.

      IF all_satisfied = abap_false AND required_specs IS NOT INITIAL.
        DATA(available_versions) = get_versions( resolve_node->name ).

        max_satisfying = resolve_node->max_satisfying(
          versions = available_versions
          specs    = required_specs ).
      ENDIF.

      resolve_node->set_max_satisfying( max_satisfying ).
    ENDLOOP.

  ENDMETHOD.


  METHOD select_versions.

    LOOP AT tree->get_all( ) INTO DATA(node).
      DATA(specs) = VALUE string_table( ).
      DATA(all_satisfied) = abap_true.

      LOOP AT node->edges_in INTO DATA(edge).
        IF edge->type = /apmg/if_apm_arborist=>c_dependency_type-optional.
          CONTINUE.
        ENDIF.
        INSERT edge->spec INTO TABLE specs.
        IF node->satisfies( edge->spec ) = abap_false.
          all_satisfied = abap_false.
        ENDIF.
      ENDLOOP.

      IF specs IS INITIAL OR all_satisfied = abap_true.
        node->set_max_satisfying( node->version ).
        CONTINUE.
      ENDIF.

      DATA(available_versions) = get_versions( node->name ).
      IF available_versions IS INITIAL.
        add_log(
          type     = /apmg/if_apm_arborist=>c_log_type-error
          category = /apmg/if_apm_arborist=>c_diagnostic_category-manifest_unavailable
          message  = |No registry versions are available for { node->name }|
          name     = node->name
          spec     = concat_lines_of( table = specs sep = ` ` ) ).
        CONTINUE.
      ENDIF.
      DATA(selected_version) = node->max_satisfying(
        versions = available_versions
        specs    = specs ).

      IF selected_version IS INITIAL.
        node->set_max_satisfying( '' ).
        add_log(
          type     = /apmg/if_apm_arborist=>c_log_type-error
          category = /apmg/if_apm_arborist=>c_diagnostic_category-no_satisfying_version
          message  = |No version of { node->name } satisfies all incoming ranges|
          name     = node->name
          spec     = concat_lines_of( table = specs sep = ` ` ) ).
        CONTINUE.
      ENDIF.

      TRY.
          DATA(manifest) = get_manifest(
            tree    = tree
            name    = node->name
            version = selected_version
            exact   = abap_true ).
          node->update_manifest( manifest ).
          result = abap_true.
        CATCH /apmg/cx_apm_error INTO DATA(manifest_error).
          add_log(
            type     = /apmg/if_apm_arborist=>c_log_type-error
            category = /apmg/if_apm_arborist=>c_diagnostic_category-manifest_unavailable
            message  = manifest_error->get_text( )
            name     = node->name
            version  = selected_version ).
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_add_packages.

    LOOP AT add_packages ASSIGNING FIELD-SYMBOL(<add>).
      IF <add>-name IS INITIAL OR <add>-version IS INITIAL.
        raise_error( 'Added packages require an exact name and version' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_remove_packages.

    LOOP AT remove_packages ASSIGNING FIELD-SYMBOL(<remove>).
      IF NOT current_tree->exists( <remove> ).
        raise_error( |Package { <remove> } is not installed| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
