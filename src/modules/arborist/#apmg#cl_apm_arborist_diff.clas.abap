CLASS /apmg/cl_apm_arborist_diff DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Arborist - Diff
*
* Tree representing the difference between actual and ideal trees.
* Follows @npmcli/arborist diff semantics (ADD, CHANGE, REMOVE).
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* https://github.com/npm/cli/blob/latest/workspaces/arborist/docs/diff.md
************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_diff_ref  TYPE REF TO /apmg/cl_apm_arborist_diff,
      ty_diff_refs TYPE STANDARD TABLE OF ty_diff_ref WITH KEY table_line.

    "! Node in the actual tree (null for ADD)
    DATA actual TYPE REF TO /apmg/cl_apm_arborist_node READ-ONLY.
    "! Node in the ideal tree (null for REMOVE)
    DATA ideal TYPE REF TO /apmg/cl_apm_arborist_node READ-ONLY.
    "! Diff action: ADD, CHANGE, REMOVE, or initial for synthetic root
    DATA action TYPE /apmg/if_apm_arborist=>ty_diff_action READ-ONLY.
    "! Parent diff node
    DATA parent TYPE REF TO /apmg/cl_apm_arborist_diff READ-ONLY.
    "! Child diff nodes
    DATA children TYPE ty_diff_refs READ-ONLY.
    "! Leaf diff nodes under this branch
    DATA leaves TYPE ty_diff_refs READ-ONLY.
    "! Ideal nodes that do not change in this branch
    DATA unchanged TYPE /apmg/cl_apm_arborist_node=>ty_node_refs READ-ONLY.
    "! Actual nodes removed in this branch
    DATA removed TYPE /apmg/cl_apm_arborist_node=>ty_node_refs READ-ONLY.

    "! Calculate diff between actual and ideal trees
    CLASS-METHODS calculate
      IMPORTING
        !actual       TYPE REF TO /apmg/cl_apm_arborist_tree
        !ideal        TYPE REF TO /apmg/cl_apm_arborist_tree
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_arborist_diff.

    METHODS constructor
      IMPORTING
        !actual TYPE REF TO /apmg/cl_apm_arborist_node OPTIONAL
        !ideal  TYPE REF TO /apmg/cl_apm_arborist_node OPTIONAL
        !action TYPE /apmg/if_apm_arborist=>ty_diff_action OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS set_parent
      IMPORTING
        !parent TYPE REF TO /apmg/cl_apm_arborist_diff.

    CLASS-METHODS get_action
      IMPORTING
        !actual       TYPE REF TO /apmg/cl_apm_arborist_node
        !ideal        TYPE REF TO /apmg/cl_apm_arborist_node
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_arborist=>ty_diff_action.

    CLASS-METHODS get_prod_children
      IMPORTING
        !node         TYPE REF TO /apmg/cl_apm_arborist_node
        !tree         TYPE REF TO /apmg/cl_apm_arborist_tree
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_arborist_node=>ty_node_refs.

    METHODS build_children
      IMPORTING
        !actual_tree TYPE REF TO /apmg/cl_apm_arborist_tree
        !ideal_tree  TYPE REF TO /apmg/cl_apm_arborist_tree.

    METHODS merge_child_results
      IMPORTING
        !child TYPE REF TO /apmg/cl_apm_arborist_diff.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_diff IMPLEMENTATION.


  METHOD build_children.

    DATA(actual_node) = actual.
    DATA(ideal_node)  = ideal.

    DATA(actual_kids) = get_prod_children( node = actual_node tree = actual_tree ).
    DATA(ideal_kids)  = get_prod_children( node = ideal_node tree = ideal_tree ).

    DATA(child_names) = VALUE string_table( ).
    LOOP AT actual_kids INTO DATA(actual_kid).
      INSERT actual_kid->name INTO TABLE child_names.
    ENDLOOP.
    LOOP AT ideal_kids INTO DATA(ideal_kid).
      INSERT ideal_kid->name INTO TABLE child_names.
    ENDLOOP.
    SORT child_names.
    DELETE ADJACENT DUPLICATES FROM child_names.

    DATA(children_result) = VALUE ty_diff_refs( ).
    DATA(unchanged_result) = VALUE /apmg/cl_apm_arborist_node=>ty_node_refs( ).
    DATA(removed_result) = VALUE /apmg/cl_apm_arborist_node=>ty_node_refs( ).
    DATA(leaves_result) = VALUE ty_diff_refs( ).

    LOOP AT child_names ASSIGNING FIELD-SYMBOL(<name>).
      DATA(actual_child) = actual_tree->get_by_name( <name> ).
      DATA(ideal_child)  = ideal_tree->get_by_name( <name> ).

      IF actual_child IS NOT BOUND AND ideal_child IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(child_action) = get_action(
        actual = actual_child
        ideal  = ideal_child ).

      IF child_action IS NOT INITIAL.
        IF child_action = /apmg/if_apm_arborist=>c_diff_action-remove.
          APPEND actual_child TO removed_result.
        ENDIF.

        DATA(child_diff) = NEW /apmg/cl_apm_arborist_diff(
          actual = actual_child
          ideal  = ideal_child
          action = child_action ).

        child_diff->build_children(
          actual_tree = actual_tree
          ideal_tree  = ideal_tree ).

        child_diff->set_parent( me ).
        APPEND child_diff TO children_result.
        merge_child_results( child_diff ).

      ELSE.
        APPEND ideal_child TO unchanged_result.
        DATA(sub) = NEW /apmg/cl_apm_arborist_diff(
          actual = actual_child
          ideal  = ideal_child ).
        sub->build_children(
          actual_tree = actual_tree
          ideal_tree  = ideal_tree ).
        APPEND LINES OF sub->children TO children_result.
        APPEND LINES OF sub->leaves TO leaves_result.
        APPEND LINES OF sub->unchanged TO unchanged_result.
        APPEND LINES OF sub->removed TO removed_result.
        LOOP AT sub->children INTO DATA(sub_child).
          sub_child->set_parent( me ).
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    children = children_result.
    unchanged = unchanged_result.
    removed = removed_result.
    leaves = leaves_result.

    IF children IS INITIAL AND action IS NOT INITIAL.
      APPEND me TO leaves.
    ENDIF.

  ENDMETHOD.


  METHOD calculate.

    result = NEW /apmg/cl_apm_arborist_diff( ).

    IF actual IS NOT BOUND OR ideal IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(actual_roots) = actual->get_roots( ).
    DATA(ideal_roots)  = ideal->get_roots( ).

    DATA(root_names) = VALUE string_table( ).
    LOOP AT actual_roots INTO DATA(actual_root).
      INSERT actual_root->name INTO TABLE root_names.
    ENDLOOP.
    LOOP AT ideal_roots INTO DATA(ideal_root).
      INSERT ideal_root->name INTO TABLE root_names.
    ENDLOOP.
    SORT root_names.
    DELETE ADJACENT DUPLICATES FROM root_names.

    LOOP AT root_names ASSIGNING FIELD-SYMBOL(<name>).
      DATA(actual_node) = actual->get_by_name( <name> ).
      DATA(ideal_node)  = ideal->get_by_name( <name> ).

      IF actual_node IS NOT BOUND AND ideal_node IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(root_action) = get_action(
        actual = actual_node
        ideal  = ideal_node ).

      IF root_action IS NOT INITIAL.
        IF root_action = /apmg/if_apm_arborist=>c_diff_action-remove.
          APPEND actual_node TO result->removed.
        ENDIF.

        DATA(root_diff) = NEW /apmg/cl_apm_arborist_diff(
          actual = actual_node
          ideal  = ideal_node
          action = root_action ).

        root_diff->build_children(
          actual_tree = actual
          ideal_tree  = ideal ).

        root_diff->set_parent( result ).
        APPEND root_diff TO result->children.
        result->merge_child_results( root_diff ).

      ELSE.
        DATA(sub_diff) = NEW /apmg/cl_apm_arborist_diff(
          actual = actual_node
          ideal  = ideal_node ).

        sub_diff->build_children(
          actual_tree = actual
          ideal_tree  = ideal ).

        APPEND LINES OF sub_diff->children TO result->children.
        APPEND LINES OF sub_diff->leaves TO result->leaves.
        APPEND LINES OF sub_diff->unchanged TO result->unchanged.
        APPEND LINES OF sub_diff->removed TO result->removed.
        LOOP AT sub_diff->children INTO DATA(root_child).
          root_child->set_parent( result ).
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    me->actual = actual.
    me->ideal  = ideal.
    me->action = action.

  ENDMETHOD.


  METHOD get_action.

    IF ideal IS NOT BOUND.
      result = /apmg/if_apm_arborist=>c_diff_action-remove.
      RETURN.
    ENDIF.

    IF actual IS NOT BOUND.
      result = /apmg/if_apm_arborist=>c_diff_action-add.
      RETURN.
    ENDIF.

    IF ideal->get_target_version( ) <> actual->version.
      result = /apmg/if_apm_arborist=>c_diff_action-change.
    ENDIF.

  ENDMETHOD.


  METHOD get_prod_children.

    IF node IS NOT BOUND OR tree IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT node->edges_out ASSIGNING FIELD-SYMBOL(<edge>).
      IF <edge>->type <> /apmg/if_apm_arborist=>c_dependency_type-prod.
        CONTINUE.
      ENDIF.
      DATA(child) = tree->get_by_name( <edge>->name ).
      IF child IS BOUND.
        INSERT child INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD merge_child_results.

    APPEND LINES OF child->leaves TO leaves.
    APPEND LINES OF child->unchanged TO unchanged.
    APPEND LINES OF child->removed TO removed.

  ENDMETHOD.


  METHOD set_parent.

    me->parent = parent.

  ENDMETHOD.


ENDCLASS.
