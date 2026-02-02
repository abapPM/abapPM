CLASS /apmg/cl_apm_arborist_node DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Arborist - Node
*
* A node represents a package that is installed on this system, either
* as a global package, or as a bundle of another package.
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
    "! Installed version
    DATA version TYPE /apmg/if_apm_types=>ty_version READ-ONLY.
    "! Production dependencies
    DATA deps_prod TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! Development dependencies
    DATA deps_dev TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! Peer dependencies
    DATA deps_peer TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! Optional dependencies
    DATA deps_optional TYPE /apmg/if_apm_types=>ty_dependencies READ-ONLY.
    "! Is this a bundled package
    DATA bundle TYPE abap_bool READ-ONLY.
    "! Is this a dev dependency
    DATA dev TYPE abap_bool READ-ONLY.
    "! Is this an optional dependency
    DATA optional TYPE abap_bool READ-ONLY.
    "! Is this a peer dependency
    DATA peer TYPE abap_bool READ-ONLY.
    "! Is this package installed
    DATA installed TYPE abap_bool READ-ONLY.
    "! Outgoing edges (dependencies of this package)
    DATA edges_out TYPE ty_edges READ-ONLY.
    "! Incoming edges (packages that depend on this)
    DATA edges_in TYPE ty_edges READ-ONLY.
    "! Errors during tree building
    DATA errors TYPE string_table READ-ONLY.

    "! Factory method to create a node from manifest
    CLASS-METHODS create
      IMPORTING
        !package      TYPE /apmg/if_apm_types=>ty_devclass OPTIONAL
        !manifest     TYPE /apmg/if_apm_types=>ty_package_json
        !installed    TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

    "! Get a node by name from the global tree
    CLASS-METHODS get_by_name
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

    "! Get a node by package from the global tree
    CLASS-METHODS get_by_package
      IMPORTING
        !package      TYPE /apmg/if_apm_types=>ty_devclass
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

    "! Get all nodes in the global tree
    CLASS-METHODS get_all
      RETURNING
        VALUE(result) TYPE ty_node_refs.

    "! Clear the global tree
    CLASS-METHODS clear.

    "! Check if a node exists in the tree by name
    CLASS-METHODS exists
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE abap_bool.

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

    "! Check if this node satisfies a version spec
    METHODS satisfies
      IMPORTING
        !spec         TYPE /apmg/if_apm_types=>ty_spec
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Add an error message
    METHODS add_error
      IMPORTING
        !message TYPE string.

    "! Get all dependencies as a flat list
    METHODS get_all_dependencies
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_dependencies.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_node_entry,
        name     TYPE /apmg/if_apm_types=>ty_name,
        package  TYPE /apmg/if_apm_types=>ty_devclass,
        instance TYPE REF TO /apmg/cl_apm_arborist_node,
      END OF ty_node_entry,
      ty_node_entries TYPE HASHED TABLE OF ty_node_entry WITH UNIQUE KEY name.

    "! Global tree storage (singleton pattern)
    CLASS-DATA tree TYPE ty_node_entries.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_node IMPLEMENTATION.


  METHOD create.

    " Check if node already exists
    IF exists( manifest-name ).
      result = get_by_name( manifest-name ).
      RETURN.
    ENDIF.

    " Create new node
    result = NEW /apmg/cl_apm_arborist_node(
      package   = package
      manifest  = manifest
      installed = installed ).

    " Add to global tree
    DATA(entry) = VALUE ty_node_entry(
      name     = manifest-name
      package  = package
      instance = result ).
    INSERT entry INTO TABLE tree.

  ENDMETHOD.


  METHOD constructor.

    me->package       = package.
    me->name          = manifest-name.
    me->version       = manifest-version.
    me->deps_prod     = manifest-dependencies.
    me->deps_dev      = manifest-dev_dependencies.
    me->deps_peer     = manifest-peer_dependencies.
    me->deps_optional = manifest-optional_dependencies.
    me->installed     = installed.

  ENDMETHOD.


  METHOD get_by_name.

    READ TABLE tree ASSIGNING FIELD-SYMBOL(<entry>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <entry>-instance.
    ENDIF.

  ENDMETHOD.


  METHOD get_by_package.

    LOOP AT tree ASSIGNING FIELD-SYMBOL(<entry>) WHERE package = package.
      result = <entry>-instance.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_all.

    LOOP AT tree ASSIGNING FIELD-SYMBOL(<entry>).
      INSERT <entry>-instance INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD clear.

    CLEAR tree.

  ENDMETHOD.


  METHOD exists.

    result = xsdbool( line_exists( tree[ name = name ] ) ).

  ENDMETHOD.


  METHOD add_edge_out.

    INSERT edge INTO TABLE edges_out.

  ENDMETHOD.


  METHOD add_edge_in.

    INSERT edge INTO TABLE edges_in.

  ENDMETHOD.


  METHOD satisfies.

    TRY.
        result = /apmg/cl_apm_semver_functions=>satisfies(
          version = version
          range   = spec ).
      CATCH /apmg/cx_apm_error.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD add_error.

    INSERT message INTO TABLE errors.

  ENDMETHOD.


  METHOD get_all_dependencies.

    " Combine all dependency types
    APPEND LINES OF deps_prod TO result.
    APPEND LINES OF deps_dev TO result.
    APPEND LINES OF deps_peer TO result.
    APPEND LINES OF deps_optional TO result.

  ENDMETHOD.

ENDCLASS.
