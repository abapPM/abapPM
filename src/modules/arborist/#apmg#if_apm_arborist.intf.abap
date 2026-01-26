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

ENDINTERFACE.
