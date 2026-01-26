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

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_edge  TYPE REF TO /apmg/cl_apm_arborist_edge,
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
      END OF ty_node,
      ty_nodes TYPE STANDARD TABLE OF ty_node
        WITH UNIQUE HASHED KEY package COMPONENTS package
        WITH NON-UNIQUE SORTED KEY name COMPONENTS name version.

    CLASS-DATA nodes TYPE ty_nodes.

ENDCLASS.



CLASS /apmg/cl_apm_arborist_node IMPLEMENTATION.
ENDCLASS.
