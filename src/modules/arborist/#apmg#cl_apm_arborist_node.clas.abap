CLASS /apmg/cl_apm_arborist_node DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Arborist - Node
*
* A node represents a package that is installed on this system, either
* as a global package, or as a bundle of another package (bundle).
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* https://www.npmjs.com/package/@npmcli/arborist
* https://github.com/npm/cli/tree/latest/workspaces/arborist
************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_edge  TYPE REF TO /apmg/cl_apm_arborist_edge,
      ty_edges TYPE STANDARD TABLE OF ty_edge WITH KEY table_line.

    TYPES:
      ty_node_ref  TYPE REF TO /apmg/cl_apm_arborist_node,
      ty_node_refs TYPE STANDARD TABLE OF ty_node_ref WITH KEY table_line.

    "! Package (SAP devclass)
    DATA package TYPE /apmg/if_apm_types=>ty_devclass READ-ONLY.
    "! Package name in registry
    DATA name TYPE /apmg/if_apm_types=>ty_name READ-ONLY.
    "! Installed version (current on system, or target for new packages)
    DATA version TYPE /apmg/if_apm_types=>ty_version READ-ONLY.
    "! Maximum version that satisfies the list of version specs (of all in edges)
    DATA max_satisfying_version TYPE /apmg/if_apm_types=>ty_version READ-ONLY.
    "! Production dependencies
    DATA dependencies TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! Development dependencies
    DATA dev_dependencies TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! Peer dependencies
    DATA peer_dependencies TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! Optional dependencies
    DATA optional_dependencies TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! bundle dependencies
    DATA bundle_dependencies TYPE /apmg/if_apm_types=>ty_bundle_dependencies READ-ONLY.
    "! Is this package installed on the system today
    DATA installed TYPE abap_bool READ-ONLY.
    "! Outgoing edges (dependencies of this package)
    DATA edges_out TYPE ty_edges READ-ONLY.
    "! Incoming edges (packages that depend on this)
    DATA edges_in TYPE ty_edges READ-ONLY.
    "! Errors during tree building
    DATA errors TYPE string_table READ-ONLY.

    "! Constructor
    METHODS constructor
      IMPORTING
        !package   TYPE /apmg/if_apm_types=>ty_devclass OPTIONAL
        !manifest  TYPE /apmg/if_apm_types=>ty_package_json
        !installed TYPE abap_bool DEFAULT abap_true.

    "! Add an outgoing edge (dependency)
    METHODS add_edge_out
      IMPORTING
        !edge TYPE REF TO /apmg/cl_apm_arborist_edge.

    "! Add an incoming edge (depended by)
    METHODS add_edge_in
      IMPORTING
        !edge TYPE REF TO /apmg/cl_apm_arborist_edge.

    "! Clear all edges
    METHODS clear_edges.

    "! Copy error messages from another node
    METHODS copy_errors
      IMPORTING
        !source TYPE REF TO /apmg/cl_apm_arborist_node.

    "! Clear all errors
    METHODS clear_errors.

    "! Get manifest data for this node
    METHODS get_manifest
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json.

    "! Update manifest fields from registry data
    METHODS update_manifest
      IMPORTING
        !manifest TYPE /apmg/if_apm_types=>ty_package_json.

    "! Check if this node satisfies a version spec
    METHODS satisfies
      IMPORTING
        !range        TYPE /apmg/if_apm_types=>ty_spec
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Get the maximum version that satisfies a list of version specs
    METHODS max_satisfying
      IMPORTING
        !versions     TYPE /apmg/if_apm_types=>ty_versions
        !specs        TYPE string_table
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_version.

    "! Set the maximum version that satisfies the version specs
    METHODS set_max_satisfying
      IMPORTING
        !max_satisfying TYPE /apmg/if_apm_types=>ty_version.

    "! Get the ideal target version for diffing
    METHODS get_target_version
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_version.

    "! Add an error message
    METHODS add_error
      IMPORTING
        !message TYPE string.

    "! Get all dependencies as a flat list
    METHODS get_all_dependencies
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_dependencies.

    "! Get prod dependency child names
    METHODS get_prod_dep_names
      RETURNING
        VALUE(result) TYPE string_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA max_satisfying_val TYPE /apmg/if_apm_types=>ty_version.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_node IMPLEMENTATION.


  METHOD add_edge_in.

    INSERT edge INTO TABLE edges_in.

  ENDMETHOD.


  METHOD add_edge_out.

    INSERT edge INTO TABLE edges_out.

  ENDMETHOD.


  METHOD add_error.

    INSERT message INTO TABLE errors.

  ENDMETHOD.


  METHOD clear_edges.

    CLEAR: edges_out, edges_in.

  ENDMETHOD.


  METHOD clear_errors.

    CLEAR errors.

  ENDMETHOD.


  METHOD constructor.

    me->package               = package.
    me->name                  = manifest-name.
    me->version               = manifest-version.
    me->dependencies          = manifest-dependencies.
    me->dev_dependencies      = manifest-dev_dependencies.
    me->peer_dependencies     = manifest-peer_dependencies.
    me->optional_dependencies = manifest-optional_dependencies.
    me->bundle_dependencies   = manifest-bundle_dependencies.
    me->installed             = installed.
    me->max_satisfying_val = manifest-version.
    me->max_satisfying_version = manifest-version.

  ENDMETHOD.


  METHOD copy_errors.

    IF source IS NOT BOUND.
      RETURN.
    ENDIF.
    errors = source->errors.

  ENDMETHOD.


  METHOD get_all_dependencies.

    APPEND LINES OF dependencies TO result.
    APPEND LINES OF dev_dependencies TO result.
    APPEND LINES OF peer_dependencies TO result.
    APPEND LINES OF optional_dependencies TO result.

  ENDMETHOD.


  METHOD get_manifest.

    result-name                  = name.
    result-version               = version.
    result-dependencies          = dependencies.
    result-dev_dependencies      = dev_dependencies.
    result-peer_dependencies     = peer_dependencies.
    result-optional_dependencies = optional_dependencies.
    result-bundle_dependencies   = bundle_dependencies.

  ENDMETHOD.


  METHOD get_prod_dep_names.

    LOOP AT dependencies ASSIGNING FIELD-SYMBOL(<dep>).
      INSERT <dep>-key INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_target_version.

    IF max_satisfying_val IS NOT INITIAL.
      result = max_satisfying_val.
    ELSE.
      result = version.
    ENDIF.

  ENDMETHOD.


  METHOD max_satisfying.

    DATA(range) = concat_lines_of(
      table = specs
      sep   = ` ` ).

    TRY.
        result = /apmg/cl_apm_semver_ranges=>max_satisfying(
          versions = versions
          range    = range ).
      CATCH /apmg/cx_apm_error.
        result = ''.
    ENDTRY.

  ENDMETHOD.


  METHOD satisfies.

    TRY.
        result = /apmg/cl_apm_semver_functions=>satisfies(
          version = version
          range   = range ).
      CATCH /apmg/cx_apm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD set_max_satisfying.

    max_satisfying_val = max_satisfying.
    max_satisfying_version = max_satisfying.

    IF max_satisfying IS INITIAL.
      add_error( 'No version satisfies required specs' ).
    ELSEIF installed = abap_true AND max_satisfying <> version.
      add_error( |Update to version { max_satisfying } required| ).
    ELSEIF installed = abap_false.
      add_error( |Install version { max_satisfying } required| ).
    ENDIF.

  ENDMETHOD.


  METHOD update_manifest.

    me->dependencies          = manifest-dependencies.
    me->dev_dependencies      = manifest-dev_dependencies.
    me->peer_dependencies     = manifest-peer_dependencies.
    me->optional_dependencies = manifest-optional_dependencies.
    me->bundle_dependencies   = manifest-bundle_dependencies.

  ENDMETHOD.


ENDCLASS.
