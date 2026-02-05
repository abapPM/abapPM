INTERFACE /apmg/if_apm_arborist PUBLIC.

************************************************************************
* Arborist
*
* Copyright 2025 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Similar to @npmcli/arborist
*
* https://www.npmjs.com/package/@npmcli/arborist
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    ty_dependency_type TYPE string,
    ty_error_type      TYPE string.

  CONSTANTS:
    BEGIN OF c_dependency_type,
      prod     TYPE ty_dependency_type VALUE 'prod',
      dev      TYPE ty_dependency_type VALUE 'dev',
      optional TYPE ty_dependency_type VALUE 'optional',
      peer     TYPE ty_dependency_type VALUE 'peer',
    END OF c_dependency_type.

  CONSTANTS:
    BEGIN OF c_error_type,
      detached   TYPE ty_error_type VALUE 'DETACHED',
      missing    TYPE ty_error_type VALUE 'MISSING',
      peer_local TYPE ty_error_type VALUE 'PEER LOCAL',
      invalid    TYPE ty_error_type VALUE 'INVALID',
    END OF c_error_type.

  TYPES:
    ty_node_ref  TYPE REF TO /apmg/cl_apm_arborist_node,
    ty_node_refs TYPE STANDARD TABLE OF ty_node_ref WITH KEY table_line.

  TYPES:
    "! Log entry for tree issues
    BEGIN OF ty_log_entry,
      type    TYPE string,
      message TYPE string,
      name    TYPE string,
      version TYPE string,
      spec    TYPE string,
    END OF ty_log_entry,
    ty_log TYPE STANDARD TABLE OF ty_log_entry WITH EMPTY KEY.

  CONSTANTS:
    BEGIN OF c_log_type,
      info     TYPE string VALUE 'INFO',
      warning  TYPE string VALUE 'WARNING',
      error    TYPE string VALUE 'ERROR',
      circular TYPE string VALUE 'CIRCULAR',
      depth    TYPE string VALUE 'DEPTH',
    END OF c_log_type.

  " READING

  "! Reads the installed packages and builds the actual tree
  METHODS load_actual_tree
    RETURNING
      VALUE(result) TYPE ty_node_refs.

  "! Read just what the package-lock.abap.json says (FUTURE)
  METHODS load_virtual_tree.

  " OPTIMIZING AND DESIGNING

  "! Build an ideal tree from package.abap.json and various lockfiles
  METHODS build_ideal_tree.

  " WRITING

  "! Make the idealTree be the thing that's persisted
  METHODS reify_tree.

  "! Get the log of issues found during tree building
  METHODS get_log
    RETURNING
      VALUE(result) TYPE ty_log.

  "! Get all nodes in the tree
  METHODS get_tree
    RETURNING
      VALUE(result) TYPE ty_node_refs.

ENDINTERFACE.
