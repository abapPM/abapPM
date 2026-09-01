CLASS /apmg/cl_apm_arborist_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Arborist - Tree
*
* Instance-scoped container for package nodes. Replaces the former
* singleton storage on cl_arborist_node.
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_node_ref  TYPE REF TO /apmg/cl_apm_arborist_node,
      ty_node_refs TYPE STANDARD TABLE OF ty_node_ref WITH KEY table_line.

    TYPES:
      BEGIN OF ty_node_entry,
        name     TYPE /apmg/if_apm_types=>ty_name,
        package  TYPE /apmg/if_apm_types=>ty_devclass,
        instance TYPE REF TO /apmg/cl_apm_arborist_node,
      END OF ty_node_entry,
      ty_node_entries TYPE HASHED TABLE OF ty_node_entry WITH UNIQUE KEY name.

    "! Clear all nodes
    METHODS clear.

    "! Check if a node exists by name
    METHODS exists
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Get a node by name
    METHODS get_by_name
      IMPORTING
        !name         TYPE /apmg/if_apm_types=>ty_name
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

    "! Get a node by SAP package
    METHODS get_by_package
      IMPORTING
        !package      TYPE /apmg/if_apm_types=>ty_devclass
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

    "! Get all nodes
    METHODS get_all
      RETURNING
        VALUE(result) TYPE ty_node_refs.

    "! Add a node from manifest (returns existing if name already present)
    METHODS add_node
      IMPORTING
        !package      TYPE /apmg/if_apm_types=>ty_devclass OPTIONAL
        !manifest     TYPE /apmg/if_apm_types=>ty_package_json
        !installed    TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

    "! Remove a node by name
    METHODS remove_node
      IMPORTING
        !name TYPE /apmg/if_apm_types=>ty_name.

    "! Remove multiple nodes by name
    METHODS remove_nodes
      IMPORTING
        !names TYPE string_table.

    "! Clear all edges on every node
    METHODS clear_all_edges.

    "! Deep-clone this tree including edges
    METHODS clone
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_tree.

    "! Get root nodes (no incoming edges)
    METHODS get_roots
      RETURNING
        VALUE(result) TYPE ty_node_refs.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA nodes TYPE ty_node_entries.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_tree IMPLEMENTATION.


  METHOD add_node.

    IF exists( manifest-name ).
      result = get_by_name( manifest-name ).
      RETURN.
    ENDIF.

    result = NEW /apmg/cl_apm_arborist_node(
      package   = package
      manifest  = manifest
      installed = installed ).

    INSERT VALUE #(
      name     = manifest-name
      package  = package
      instance = result ) INTO TABLE nodes.

  ENDMETHOD.


  METHOD clear.

    CLEAR nodes.

  ENDMETHOD.


  METHOD clear_all_edges.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node_entry>).
      <node_entry>-instance->clear_edges( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD clone.

    result = NEW /apmg/cl_apm_arborist_tree( ).

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<clone_entry>).
      result->add_node(
        package   = <clone_entry>-package
        manifest  = <clone_entry>-instance->get_manifest( )
        installed = <clone_entry>-instance->installed ).
    ENDLOOP.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<edge_entry>).
      DATA(source) = <edge_entry>-instance.
      DATA(target) = result->get_by_name( <edge_entry>-name ).
      IF target IS NOT BOUND.
        CONTINUE.
      ENDIF.

      LOOP AT source->edges_out ASSIGNING FIELD-SYMBOL(<edge>).
        /apmg/cl_apm_arborist_edge=>create(
          tree = result
          from = target
          type = <edge>->type
          name = <edge>->name
          spec = <edge>->spec ).
      ENDLOOP.

      target->copy_errors( source ).
      target->set_max_satisfying( source->max_satisfying_version ).
    ENDLOOP.

  ENDMETHOD.


  METHOD exists.

    result = xsdbool( line_exists( nodes[ name = name ] ) ).

  ENDMETHOD.


  METHOD get_all.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<list_entry>).
      INSERT <list_entry>-instance INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_by_name.

    READ TABLE nodes ASSIGNING FIELD-SYMBOL(<read_entry>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <read_entry>-instance.
    ENDIF.

  ENDMETHOD.


  METHOD get_by_package.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<package_entry>) WHERE package = package.
      result = <package_entry>-instance.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_roots.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<root_entry>).
      IF <root_entry>-instance->edges_in IS INITIAL.
        INSERT <root_entry>-instance INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD remove_node.

    DELETE nodes WHERE name = name.

  ENDMETHOD.


  METHOD remove_nodes.

    LOOP AT names ASSIGNING FIELD-SYMBOL(<remove_name>).
      remove_node( <remove_name> ).
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
