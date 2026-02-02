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

    METHODS constructor
      IMPORTING
        registry TYPE string.

    " READING

    "! Reads the installed packages
    METHODS load_actual_tree.
    "! Read just what the package-lock.abap.json says (FUTURE)
    METHODS load_virtual_tree.

    " OPTIMIZING AND DESIGNING

    "! Build an ideal tree from package.abap.json and various lockfiles
    METHODS build_ideal_tree.

    " WRITING

    "! Make the idealTree be the thing that's persisted
    METHODS reify_tree.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA registry TYPE string.

    TYPES:
      BEGIN OF ty_installed,
        name    TYPE /apmg/if_apm_types=>ty_name,
        version TYPE /apmg/if_apm_types=>ty_version,
        package TYPE /apmg/if_apm_types=>ty_devclass,
      END OF ty_installed,
      ty_installed_by_name TYPE HASHED TABLE OF ty_installed WITH UNIQUE KEY name.

    TYPES:
      BEGIN OF ty_seen,
        name    TYPE /apmg/if_apm_types=>ty_name,
        version TYPE /apmg/if_apm_types=>ty_version,
      END OF ty_seen,
      ty_seen_list TYPE HASHED TABLE OF ty_seen WITH UNIQUE KEY name version.

    TYPES:
      BEGIN OF ty_packument_cache,
        name      TYPE /apmg/if_apm_types=>ty_name,
        packument TYPE /apmg/if_apm_types=>ty_packument,
      END OF ty_packument_cache,
      ty_packument_caches TYPE HASHED TABLE OF ty_packument_cache WITH UNIQUE KEY name.

    DATA installed_by_name TYPE ty_installed_by_name.
    DATA processed TYPE ty_seen_list.
    DATA packument_cache TYPE ty_packument_caches.
    DATA loop_counter TYPE i.

    METHODS process_package
      IMPORTING
        name    TYPE /apmg/if_apm_types=>ty_name
        version TYPE /apmg/if_apm_types=>ty_version
        package TYPE /apmg/if_apm_types=>ty_devclass
        path    TYPE string_table.

    METHODS process_dependencies
      IMPORTING
        deps     TYPE /apmg/if_apm_types=>ty_dependencies
        dep_type TYPE /apmg/if_apm_arborist=>ty_dependency_type
        from     TYPE REF TO /apmg/cl_apm_arborist_node
        path     TYPE string_table.

    METHODS resolve_version
      IMPORTING
        name          TYPE /apmg/if_apm_types=>ty_name
        spec          TYPE /apmg/if_apm_types=>ty_spec
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_version.

    METHODS get_packument
      IMPORTING
        name          TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        cx_static_check.

    METHODS build_virtual_package
      IMPORTING
        name          TYPE /apmg/if_apm_types=>ty_name
        version       TYPE /apmg/if_apm_types=>ty_version
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_devclass.

ENDCLASS.



CLASS /apmg/cl_apm_arborist IMPLEMENTATION.


  METHOD build_ideal_tree.

    CHECK abap_true.

  ENDMETHOD.


  METHOD constructor.

    me->registry = registry.

  ENDMETHOD.


  METHOD load_actual_tree.

    /apmg/cl_apm_arborist_node=>clear( ).

    CLEAR installed_by_name.
    CLEAR processed.
    CLEAR packument_cache.
    loop_counter = 0.

    DATA(installed) = /apmg/cl_apm_package_json=>list( instanciate = abap_true ).

    LOOP AT installed ASSIGNING FIELD-SYMBOL(<installed>).
      IF <installed>-name IS INITIAL OR <installed>-version IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(installed_item) = VALUE ty_installed(
        name    = <installed>-name
        version = <installed>-version
        package = <installed>-package ).
      INSERT installed_item INTO TABLE installed_by_name.
    ENDLOOP.

    LOOP AT installed_by_name ASSIGNING FIELD-SYMBOL(<root>).
      process_package(
        name    = <root>-name
        version = <root>-version
        package = <root>-package
        path    = VALUE string_table( ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD process_package.

    DATA(key) = |{ name }@{ version }|.

    IF line_exists( path[ table_line = key ] ).
      loop_counter = loop_counter + 1.

      DATA(loop_package) = package.
      IF loop_package IS INITIAL.
        loop_package = build_virtual_package( name = name version = version ).
      ENDIF.

      DATA(loop_node) = /apmg/cl_apm_arborist_node=>get_or_create(
        package = loop_package
        name    = name
        version = version ).
      loop_node->add_error( |Circular dependency detected: { key }| ).

      IF loop_counter >= 5.
        RETURN.
      ENDIF.
      RETURN.
    ENDIF.

    IF line_exists( processed[ name = name version = version ] ).
      RETURN.
    ENDIF.
    INSERT VALUE ty_seen( name = name version = version ) INTO TABLE processed.

    DATA(node_package) = package.
    IF node_package IS INITIAL.
      node_package = build_virtual_package( name = name version = version ).
    ENDIF.

    DATA(node_ref) = /apmg/cl_apm_arborist_node=>get_or_create(
      package = node_package
      name    = name
      version = version ).

    DATA(packument) = VALUE /apmg/if_apm_types=>ty_packument( ).
    TRY.
        packument = get_packument( name ).
      CATCH cx_static_check.
        node_ref->add_error( |Failed to load packument: { name }| ).
        RETURN.
    ENDTRY.

    IF packument-versions IS INITIAL.
      node_ref->add_error( |Missing packument: { name }| ).
      RETURN.
    ENDIF.

    READ TABLE packument-versions ASSIGNING FIELD-SYMBOL(<version>)
      WITH KEY key = version.
    IF sy-subrc <> 0.
      node_ref->add_error( |Missing manifest version: { key }| ).
      RETURN.
    ENDIF.
    DATA(manifest) = <version>-manifest.

    node_ref->set_dependencies(
      deps_prod     = manifest-dependencies
      deps_dev      = manifest-dev_dependencies
      deps_peer     = manifest-peer_dependencies
      deps_optional = manifest-optional_dependencies ).

    DATA(next_path) = path.
    INSERT key INTO TABLE next_path.

    process_dependencies(
      deps     = manifest-dependencies
      dep_type = /apmg/if_apm_arborist=>c_dependency_type-prod
      from     = node_ref
      path     = next_path ).

    process_dependencies(
      deps     = manifest-dev_dependencies
      dep_type = /apmg/if_apm_arborist=>c_dependency_type-dev
      from     = node_ref
      path     = next_path ).

    process_dependencies(
      deps     = manifest-peer_dependencies
      dep_type = /apmg/if_apm_arborist=>c_dependency_type-peer
      from     = node_ref
      path     = next_path ).

    process_dependencies(
      deps     = manifest-optional_dependencies
      dep_type = /apmg/if_apm_arborist=>c_dependency_type-optional
      from     = node_ref
      path     = next_path ).

  ENDMETHOD.

  METHOD process_dependencies.

    LOOP AT deps INTO DATA(dep).
      DATA(dep_name) = dep-key.
      DATA(dep_spec) = dep-range.
      DATA(dep_version) = resolve_version( name = dep_name spec = dep_spec ).
      DATA(dep_package) = VALUE /apmg/if_apm_types=>ty_devclass( ).

      IF line_exists( installed_by_name[ name = dep_name ] ).
        DATA(installed_item) = installed_by_name[ name = dep_name ].
        dep_version = installed_item-version.
        dep_package = installed_item-package.
      ELSEIF dep_version IS NOT INITIAL.
        dep_package = build_virtual_package( name = dep_name version = dep_version ).
      ENDIF.

      DATA(edge) = /apmg/cl_apm_arborist_edge=>create(
        from = from
        type = dep_type
        name = dep_name
        spec = dep_spec ).
      from->add_edge_out( edge ).

      IF dep_version IS INITIAL.
        edge->mark_missing( ).
        from->add_error( |Missing dependency: { dep_name } ({ dep_spec })| ).
        CONTINUE.
      ENDIF.

      DATA(dep_node) = /apmg/cl_apm_arborist_node=>get_or_create(
        package = dep_package
        name    = dep_name
        version = dep_version ).
      dep_node->add_edge_in( edge ).
      edge->set_target( dep_node ).

      IF dep_spec IS NOT INITIAL AND dep_spec <> dep_version.
        from->add_error( |Dependency mismatch: { dep_name } { dep_spec } -> { dep_version }| ).
        edge->set_invalid( ).
      ENDIF.

      process_package(
        name    = dep_name
        version = dep_version
        package = dep_package
        path    = path ).
    ENDLOOP.

  ENDMETHOD.

  METHOD resolve_version.

    IF spec IS INITIAL.
      RETURN.
    ENDIF.

    DATA(packument) = VALUE /apmg/if_apm_types=>ty_packument( ).
    TRY.
        packument = get_packument( name ).
      CATCH cx_static_check.
        RETURN.
    ENDTRY.
    IF packument-versions IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE packument-dist_tags ASSIGNING FIELD-SYMBOL(<tag>) WITH KEY key = spec.
    IF sy-subrc = 0.
      result = <tag>-value.
      RETURN.
    ENDIF.

    IF line_exists( packument-versions[ key = spec ] ).
      result = spec.
    ENDIF.

  ENDMETHOD.

  METHOD get_packument.

    DATA cache_entry TYPE ty_packument_cache.

    FIELD-SYMBOLS <cached> TYPE ty_packument_cache.

    READ TABLE packument_cache ASSIGNING <cached> WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <cached>-packument.
      RETURN.
    ENDIF.

    DATA(method_name) = 'GET_PACKUMENT_FROM_REGISTRY'.

    TRY.
        CALL METHOD /apmg/cl_apm_command_utils=>(method_name)
          EXPORTING
            registry = registry
            name     = name
          RECEIVING
            result   = result.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

    cache_entry = VALUE ty_packument_cache(
      name      = name
      packument = result ).
    INSERT cache_entry INTO TABLE packument_cache.

  ENDMETHOD.

  METHOD build_virtual_package.

    CONSTANTS c_initial_key TYPE xstring VALUE ''.
    DATA hash TYPE string.

    TRY.
        cl_abap_hmac=>calculate_hmac_for_char(
          EXPORTING
            if_algorithm  = 'SHA1'
            if_key        = c_initial_key
            if_data       = |{ name }@{ version }|
          IMPORTING
            ef_hmacstring = hash ).
      CATCH cx_abap_message_digest.
        hash = |{ name }@{ version }|.
    ENDTRY.

    result = |APM${ hash }|.
    TRANSLATE result TO UPPER CASE.

    IF strlen( result ) > 30.
      result = result(30).
    ENDIF.

  ENDMETHOD.


  METHOD load_virtual_tree.

    CHECK abap_true.

  ENDMETHOD.


  METHOD reify_tree.

    CHECK abap_true.

  ENDMETHOD.
ENDCLASS.
