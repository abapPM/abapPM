INTERFACE /apmg/if_apm_arborist_diff PUBLIC.

************************************************************************
* Arborist - Diff
*
* Tree representing the difference between actual and ideal trees.
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

  TYPES:
    ty_diff_ref  TYPE REF TO /apmg/if_apm_arborist_diff,
    ty_diff_refs TYPE STANDARD TABLE OF ty_diff_ref WITH KEY table_line.

  "! Node in the actual tree (null for ADD)
  METHODS get_actual
    RETURNING
      VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

  "! Node in the ideal tree (null for REMOVE)
  METHODS get_ideal
    RETURNING
      VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_node.

  "! Diff action: ADD, CHANGE, REMOVE, or initial for synthetic root
  METHODS get_action
    RETURNING
      VALUE(result) TYPE /apmg/if_apm_arborist=>ty_diff_action.

  "! Parent diff node
  METHODS get_parent
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_arborist_diff.

  "! Child diff nodes
  METHODS get_children
    RETURNING
      VALUE(result) TYPE ty_diff_refs.

  "! Leaf diff nodes under this branch
  METHODS get_leaves
    RETURNING
      VALUE(result) TYPE ty_diff_refs.

  "! Ideal nodes that do not change in this branch
  METHODS get_unchanged
    RETURNING
      VALUE(result) TYPE /apmg/if_apm_arborist=>ty_node_refs.

  "! Actual nodes removed in this branch
  METHODS get_removed
    RETURNING
      VALUE(result) TYPE /apmg/if_apm_arborist=>ty_node_refs.

  "! Get each reachable change once, deepest dependency first; equal depths by name
  METHODS get_changes
    IMPORTING
      !name         TYPE /apmg/if_apm_types=>ty_name
    RETURNING
      VALUE(result) TYPE ty_diff_refs.

ENDINTERFACE.
