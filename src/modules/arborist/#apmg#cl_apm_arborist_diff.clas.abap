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

    INTERFACES /apmg/if_apm_arborist_diff.

    TYPES:
      ty_diff_ref  TYPE REF TO /apmg/if_apm_arborist_diff,
      ty_diff_refs TYPE STANDARD TABLE OF ty_diff_ref WITH KEY table_line.

    "! Node in the actual tree (null for ADD)
    DATA actual TYPE REF TO /apmg/cl_apm_arborist_node READ-ONLY.
    "! Node in the ideal tree (null for REMOVE)
    DATA ideal TYPE REF TO /apmg/cl_apm_arborist_node READ-ONLY.
    "! Diff action: ADD, CHANGE, REMOVE, or initial for synthetic root
    DATA action TYPE /apmg/if_apm_arborist=>ty_diff_action READ-ONLY.
    "! Parent diff node
    DATA parent TYPE REF TO /apmg/if_apm_arborist_diff READ-ONLY.
    "! Child diff nodes
    DATA children TYPE ty_diff_refs READ-ONLY.
    "! Leaf diff nodes under this branch
    DATA leaves TYPE ty_diff_refs READ-ONLY.
    "! Ideal nodes that do not change in this branch
    DATA unchanged TYPE /apmg/if_apm_arborist=>ty_node_refs READ-ONLY.
    "! Actual nodes removed in this branch
    DATA removed TYPE /apmg/if_apm_arborist=>ty_node_refs READ-ONLY.

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

    TYPES:
      BEGIN OF ty_depth_entry,
        name  TYPE /apmg/if_apm_types=>ty_name,
        depth TYPE i,
      END OF ty_depth_entry,
      ty_depth_entries TYPE HASHED TABLE OF ty_depth_entry WITH UNIQUE KEY name.

    TYPES:
      BEGIN OF ty_ordered_change,
        name  TYPE /apmg/if_apm_types=>ty_name,
        depth TYPE i,
        diff  TYPE REF TO /apmg/if_apm_arborist_diff,
      END OF ty_ordered_change,
      ty_ordered_changes TYPE STANDARD TABLE OF ty_ordered_change WITH EMPTY KEY.

    DATA actual_tree TYPE REF TO /apmg/cl_apm_arborist_tree.
    DATA ideal_tree TYPE REF TO /apmg/cl_apm_arborist_tree.

    METHODS set_parent
      IMPORTING
        !parent TYPE REF TO /apmg/if_apm_arborist_diff.

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
        VALUE(result) TYPE /apmg/if_apm_arborist=>ty_node_refs.

    METHODS build_children
      IMPORTING
        !actual_tree TYPE REF TO /apmg/cl_apm_arborist_tree
        !ideal_tree  TYPE REF TO /apmg/cl_apm_arborist_tree.

    METHODS merge_child_results
      IMPORTING
        !child TYPE REF TO /apmg/if_apm_arborist_diff.

    METHODS collect_dependency_depths
      IMPORTING
        !node   TYPE REF TO /apmg/cl_apm_arborist_node
        !depth  TYPE i
      CHANGING
        !depths TYPE ty_depth_entries
        !path   TYPE string_table.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_diff IMPLEMENTATION.


  METHOD /apmg/if_apm_arborist_diff~get_action.

    result = action.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_actual.

    result = actual.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_changes.

    DATA(root) = me.
    WHILE root->parent IS BOUND.
      root = CAST /apmg/cl_apm_arborist_diff( root->parent ).
    ENDWHILE.

    DATA(depths) = VALUE ty_depth_entries( ).
    DATA(path) = VALUE string_table( ).

    IF root->ideal_tree IS BOUND.
      root->collect_dependency_depths(
        EXPORTING
          node   = root->ideal_tree->get_by_name( name )
          depth  = 0
        CHANGING
          depths = depths
          path   = path ).
    ENDIF.

    CLEAR path.
    IF root->actual_tree IS BOUND.
      root->collect_dependency_depths(
        EXPORTING
          node   = root->actual_tree->get_by_name( name )
          depth  = 0
        CHANGING
          depths = depths
          path   = path ).
    ENDIF.

    DATA(ordered_changes) = VALUE ty_ordered_changes( ).
    LOOP AT depths ASSIGNING FIELD-SYMBOL(<depth>).
      DATA(actual_node) = root->actual_tree->get_by_name( <depth>-name ).
      DATA(ideal_node) = root->ideal_tree->get_by_name( <depth>-name ).
      DATA(change_action) = get_action(
        actual = actual_node
        ideal  = ideal_node ).
      IF change_action IS NOT INITIAL.
        DATA(change) = NEW /apmg/cl_apm_arborist_diff(
          actual = actual_node
          ideal  = ideal_node
          action = change_action ).
        change->set_parent( root ).
        APPEND VALUE #(
          name  = <depth>-name
          depth = <depth>-depth
          diff  = change ) TO ordered_changes.
      ENDIF.
    ENDLOOP.

    SORT ordered_changes BY depth DESCENDING name ASCENDING.
    LOOP AT ordered_changes ASSIGNING FIELD-SYMBOL(<ordered_change>).
      APPEND <ordered_change>-diff TO result.
    ENDLOOP.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_children.

    result = children.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_ideal.

    result = ideal.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_leaves.

    result = leaves.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_parent.

    result = parent.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_removed.

    result = removed.

  ENDMETHOD.


  METHOD /apmg/if_apm_arborist_diff~get_unchanged.

    result = unchanged.

  ENDMETHOD.


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
    DATA(unchanged_result) = VALUE /apmg/if_apm_arborist=>ty_node_refs( ).
    DATA(removed_result) = VALUE /apmg/if_apm_arborist=>ty_node_refs( ).
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
          CAST /apmg/cl_apm_arborist_diff( sub_child )->set_parent( me ).
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

    result->actual_tree = actual.
    result->ideal_tree = ideal.

    DATA(node_names) = VALUE string_table( ).
    LOOP AT actual->get_all( ) INTO DATA(actual_entry).
      INSERT actual_entry->name INTO TABLE node_names.
    ENDLOOP.
    LOOP AT ideal->get_all( ) INTO DATA(ideal_entry).
      INSERT ideal_entry->name INTO TABLE node_names.
    ENDLOOP.
    SORT node_names.
    DELETE ADJACENT DUPLICATES FROM node_names.

    LOOP AT node_names ASSIGNING FIELD-SYMBOL(<name>).
      DATA(actual_node) = actual->get_by_name( <name> ).
      DATA(ideal_node)  = ideal->get_by_name( <name> ).
      DATA(node_action) = get_action(
        actual = actual_node
        ideal  = ideal_node ).

      IF node_action IS NOT INITIAL.
        IF node_action = /apmg/if_apm_arborist=>c_diff_action-remove.
          APPEND actual_node TO result->removed.
        ENDIF.

        DATA(node_diff) = NEW /apmg/cl_apm_arborist_diff(
          actual = actual_node
          ideal  = ideal_node
          action = node_action ).
        node_diff->set_parent( result ).
        APPEND node_diff TO result->children.
        APPEND node_diff TO result->leaves.
      ELSEIF ideal_node IS BOUND.
        APPEND ideal_node TO result->unchanged.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD collect_dependency_depths.

    IF node IS NOT BOUND OR line_exists( path[ table_line = node->name ] ).
      RETURN.
    ENDIF.

    READ TABLE depths ASSIGNING FIELD-SYMBOL(<depth>)
      WITH TABLE KEY name = node->name.
    IF sy-subrc = 0.
      IF depth > <depth>-depth.
        <depth>-depth = depth.
      ENDIF.
    ELSE.
      INSERT VALUE #(
        name  = node->name
        depth = depth ) INTO TABLE depths.
    ENDIF.

    APPEND node->name TO path.
    LOOP AT node->edges_out ASSIGNING FIELD-SYMBOL(<edge>).
      IF <edge>->type <> /apmg/if_apm_arborist=>c_dependency_type-peer
          AND <edge>->to IS BOUND.
        collect_dependency_depths(
          EXPORTING
            node   = <edge>->to
            depth  = depth + 1
          CHANGING
            depths = depths
            path   = path ).
      ENDIF.
    ENDLOOP.
    DELETE path WHERE table_line = node->name.

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

    APPEND LINES OF child->get_leaves( ) TO leaves.
    APPEND LINES OF child->get_unchanged( ) TO unchanged.
    APPEND LINES OF child->get_removed( ) TO removed.

  ENDMETHOD.


  METHOD set_parent.

    me->parent = parent.

  ENDMETHOD.
ENDCLASS.
