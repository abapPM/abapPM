CLASS /apmg/cl_apm_arborist DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Arborist
*
* Inspect and manage package trees. In ABAP, there's only one global
* tree containing all packages managed by apm.
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
        registry TYPE string.

    " READING

    "! Reads the installed packages
    METHODS load_actual_tree.
    "! Read just what the package-lock.abap.json says (FUTURE)
    METHODS load_virtual_tree.

    " OPTIMIZING AND DESIGNING

    "! Build an ideal tree from package.abap.json and various lockfiles
    METHODS build_ideal_tree.

    " WRITING

    "! Make the idealTree be the thing that's persisted
    METHODS reify_tree.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA registry TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_arborist IMPLEMENTATION.


  METHOD build_ideal_tree.

  ENDMETHOD.


  METHOD constructor.

    me->registry = registry.

  ENDMETHOD.


  METHOD load_actual_tree.

  ENDMETHOD.


  METHOD load_virtual_tree.

  ENDMETHOD.


  METHOD reify_tree.

  ENDMETHOD.
ENDCLASS.
