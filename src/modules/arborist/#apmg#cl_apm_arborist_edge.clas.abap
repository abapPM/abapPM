CLASS /apmg/cl_apm_arborist_edge DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Arborist - Edge
*
* An Edge represents a dependency relationship. Each node has an
* edgesIn set and an edgesOut set. Each edge has a type which specifies
* what kind of dependency it represents.
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* An Edge represents a dependency relationship. Each node has an edgesIn
* set, and an edgesOut map. Each edge has a type which specifies what
* kind of dependency it represents. edge.from is a reference to the node
* that has the dependency, and edge.to is a reference to the node that
* requires the dependency.
************************************************************************

  PUBLIC SECTION.

    "! Source node (the package that has the dependency)
    DATA from TYPE REF TO /apmg/cl_apm_arborist_node READ-ONLY.
    "! Dependency type (prod, dev, optional, peer)
    DATA type TYPE /apmg/if_apm_arborist=>ty_dependency_type READ-ONLY.
    "! Name of the required package
    DATA name TYPE /apmg/if_apm_types=>ty_name READ-ONLY.
    "! Version spec/range required
    DATA spec TYPE /apmg/if_apm_types=>ty_spec READ-ONLY.
    "! Target node (the package that satisfies the dependency)
    DATA to TYPE REF TO /apmg/cl_apm_arborist_node READ-ONLY.
    "! Is the dependency valid (satisfies spec)
    DATA valid TYPE abap_bool READ-ONLY.
    "! Error type if not valid
    DATA error TYPE /apmg/if_apm_arborist=>ty_error_type READ-ONLY.

    "! Factory method to create an edge
    CLASS-METHODS create
      IMPORTING
        !from         TYPE REF TO /apmg/cl_apm_arborist_node
        !type         TYPE /apmg/if_apm_arborist=>ty_dependency_type
        !name         TYPE /apmg/if_apm_types=>ty_name
        !spec         TYPE /apmg/if_apm_types=>ty_spec
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_edge.

    "! Constructor
    METHODS constructor
      IMPORTING
        !from TYPE REF TO /apmg/cl_apm_arborist_node
        !type TYPE /apmg/if_apm_arborist=>ty_dependency_type
        !name TYPE /apmg/if_apm_types=>ty_name
        !spec TYPE /apmg/if_apm_types=>ty_spec.

    "! Resolve the target node and validate
    METHODS resolve.

    "! Check if the dependency is missing
    METHODS is_missing
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Check if the dependency is invalid (wrong version)
    METHODS is_invalid
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Get error description
    METHODS get_error_description
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_edge IMPLEMENTATION.


  METHOD constructor.

    me->from = from.
    me->type = type.
    me->name = name.
    me->spec = spec.

    " Resolve target node immediately
    resolve( ).

  ENDMETHOD.


  METHOD create.

    result = NEW #(
      from = from
      type = type
      name = name
      spec = spec ).

    " Add edge to source node's outgoing edges
    IF from IS BOUND.
      from->add_edge_out( result ).
    ENDIF.

    " Add edge to target node's incoming edges
    IF result->to IS BOUND.
      result->to->add_edge_in( result ).
    ENDIF.

  ENDMETHOD.


  METHOD get_error_description.

    CASE error.
      WHEN /apmg/if_apm_arborist=>c_error_type-missing.
        result = |Dependency { name }@{ spec } is not installed|.
      WHEN /apmg/if_apm_arborist=>c_error_type-invalid.
        IF to IS BOUND.
          result = |Dependency { name }@{ spec } not satisfied by installed { to->version }|.
        ELSE.
          result = |Dependency { name }@{ spec } is invalid|.
        ENDIF.
      WHEN /apmg/if_apm_arborist=>c_error_type-peer_local.
        result = |Peer dependency { name }@{ spec } should be installed at root level|.
      WHEN /apmg/if_apm_arborist=>c_error_type-detached.
        result = |Dependency { name } is detached from the tree|.
      WHEN OTHERS.
        result = ''.
    ENDCASE.

  ENDMETHOD.


  METHOD is_invalid.

    result = xsdbool( error = /apmg/if_apm_arborist=>c_error_type-invalid ).

  ENDMETHOD.


  METHOD is_missing.

    result = xsdbool( error = /apmg/if_apm_arborist=>c_error_type-missing ).

  ENDMETHOD.


  METHOD resolve.

    " Try to find the target node in the global tree
    to = /apmg/cl_apm_arborist_node=>get_by_name( name ).

    IF to IS NOT BOUND.
      " Dependency is missing
      valid = abap_false.
      error = /apmg/if_apm_arborist=>c_error_type-missing.
    ELSE.
      " Check if installed version satisfies the spec
      valid = to->satisfies( spec ).
      IF valid = abap_false.
        error = /apmg/if_apm_arborist=>c_error_type-invalid.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
