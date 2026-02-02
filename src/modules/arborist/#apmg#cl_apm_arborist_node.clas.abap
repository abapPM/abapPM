CLASS /apmg/cl_apm_arborist_node DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Arborist - Node
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
        package TYPE /apmg/if_apm_package_json=>ty_package-package
        name    TYPE /apmg/if_apm_types=>ty_name
        version TYPE /apmg/if_apm_types=>ty_version.

    METHODS get_name
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_name.

    METHODS get_version
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_version.

    METHODS get_package
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_package_json=>ty_package-package.

    METHODS set_dependencies
      IMPORTING
        deps_prod     TYPE /apmg/if_apm_types=>ty_dependencies
        deps_dev      TYPE /apmg/if_apm_types=>ty_dependencies
        deps_peer     TYPE /apmg/if_apm_types=>ty_dependencies
        deps_optional TYPE /apmg/if_apm_types=>ty_dependencies.

    METHODS add_edge_out
      IMPORTING
        edge TYPE REF TO object.

    METHODS add_edge_in
      IMPORTING
        edge TYPE REF TO object.

    METHODS add_error
      IMPORTING
        message TYPE string.

    CLASS-METHODS clear.

    CLASS-METHODS get_or_create
      IMPORTING
        package       TYPE /apmg/if_apm_package_json=>ty_package-package
        name          TYPE /apmg/if_apm_types=>ty_name
        version       TYPE /apmg/if_apm_types=>ty_version
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_edge  TYPE REF TO object,
      ty_edges TYPE STANDARD TABLE OF ty_edge WITH KEY table_line.

    TYPES:
      "! A node represents a package that is installed on this system, either as a global package,
      "! or as a modules of another package (bundle).
      BEGIN OF ty_node,
        package       TYPE /apmg/if_apm_package_json=>ty_package-package,
        name          TYPE /apmg/if_apm_package_json=>ty_package-name,
        version       TYPE /apmg/if_apm_package_json=>ty_package-version,
        deps_prod     TYPE /apmg/if_apm_types=>ty_dependencies,
        deps_dev      TYPE /apmg/if_apm_types=>ty_dependencies,
        deps_peer     TYPE /apmg/if_apm_types=>ty_dependencies,
        deps_optional TYPE /apmg/if_apm_types=>ty_dependencies,
        bundle        TYPE abap_bool,
        dev           TYPE abap_bool,
        optional      TYPE abap_bool,
        dev_optional  TYPE abap_bool,
        peer          TYPE abap_bool,
        edges_out     TYPE ty_edges,
        edges_in      TYPE ty_edges,
        errors        TYPE string_table,
      END OF ty_node.

    TYPES:
      BEGIN OF ty_node_entry,
        package TYPE /apmg/if_apm_package_json=>ty_package-package,
        node    TYPE REF TO /apmg/cl_apm_arborist_node,
      END OF ty_node_entry,
      ty_nodes TYPE HASHED TABLE OF ty_node_entry WITH UNIQUE KEY package.

    DATA node TYPE ty_node.

    CLASS-DATA nodes TYPE ty_nodes.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_node IMPLEMENTATION.

  METHOD constructor.
    node-package = package.
    node-name = name.
    node-version = version.
  ENDMETHOD.

  METHOD get_name.
    result = node-name.
  ENDMETHOD.

  METHOD get_version.
    result = node-version.
  ENDMETHOD.

  METHOD get_package.
    result = node-package.
  ENDMETHOD.

  METHOD set_dependencies.
    node-deps_prod = deps_prod.
    node-deps_dev = deps_dev.
    node-deps_peer = deps_peer.
    node-deps_optional = deps_optional.
  ENDMETHOD.

  METHOD add_edge_out.
    INSERT edge INTO TABLE node-edges_out.
  ENDMETHOD.

  METHOD add_edge_in.
    INSERT edge INTO TABLE node-edges_in.
  ENDMETHOD.

  METHOD add_error.
    INSERT message INTO TABLE node-errors.
  ENDMETHOD.

  METHOD clear.
    CLEAR nodes.
  ENDMETHOD.

  METHOD get_or_create.
    READ TABLE nodes ASSIGNING FIELD-SYMBOL(<node_entry>) WITH TABLE KEY package = package.
    IF sy-subrc = 0.
      result = <node_entry>-node.
    ELSE.
      result = NEW /apmg/cl_apm_arborist_node(
        package = package
        name    = name
        version = version ).

      DATA(node_entry) = VALUE ty_node_entry(
        package = package
        node    = result ).
      INSERT node_entry INTO TABLE nodes.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
