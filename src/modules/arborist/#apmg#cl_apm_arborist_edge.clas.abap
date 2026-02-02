CLASS /apmg/cl_apm_arborist_edge DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* Arborist - Edge
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        from          TYPE REF TO /apmg/cl_apm_arborist_node
        type          TYPE /apmg/if_apm_arborist=>ty_dependency_type
        name          TYPE /apmg/if_apm_types=>ty_name
        spec          TYPE /apmg/if_apm_types=>ty_spec
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_edge.

    METHODS constructor
      IMPORTING
        from TYPE REF TO /apmg/cl_apm_arborist_node
        type TYPE /apmg/if_apm_arborist=>ty_dependency_type
        name TYPE /apmg/if_apm_types=>ty_name
        spec TYPE /apmg/if_apm_types=>ty_spec.

    METHODS set_target
      IMPORTING
        to TYPE REF TO /apmg/cl_apm_arborist_node.

    METHODS mark_missing.

    METHODS set_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      "! An Edge represents a dependency relationship. Each node has an edgesIn set, and an edgesOut map.
      "! Each edge has a type which specifies what kind of dependency it represents. edge.from is a reference
      "! to the node that has the dependency, and edge.to is a reference to the node that requires the dependency.
      BEGIN OF ty_edge,
        from  TYPE REF TO /apmg/cl_apm_arborist_node,
        type  TYPE /apmg/if_apm_arborist=>ty_dependency_type,
        name  TYPE /apmg/if_apm_types=>ty_name,
        spec  TYPE /apmg/if_apm_types=>ty_spec,
        to    TYPE REF TO /apmg/cl_apm_arborist_node,
        valid TYPE abap_bool, "satisfies spec
        error TYPE /apmg/if_apm_arborist=>ty_error_type,
      END OF ty_edge.

    DATA edge TYPE ty_edge.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_edge IMPLEMENTATION.


  METHOD constructor.

    edge-from  = from.
    edge-type  = type.
    edge-name  = name.
    edge-spec  = spec.
    edge-valid = abap_false.

  ENDMETHOD.

  METHOD set_target.
    edge-to = to.

    IF to IS NOT BOUND.
      mark_missing( ).
      RETURN.
    ENDIF.

    edge-valid = abap_true.
    CLEAR edge-error.
  ENDMETHOD.

  METHOD mark_missing.
    edge-valid = abap_false.
    edge-error = /apmg/if_apm_arborist=>c_error_type-missing.
  ENDMETHOD.

  METHOD set_invalid.
    edge-valid = abap_false.
    edge-error = /apmg/if_apm_arborist=>c_error_type-invalid.
  ENDMETHOD.


  METHOD create.

    result = NEW #(
      from = from
      type = type
      name = name
      spec = spec ).

  ENDMETHOD.
ENDCLASS.
