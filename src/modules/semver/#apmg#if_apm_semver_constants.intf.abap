INTERFACE /apmg/if_apm_semver_constants PUBLIC.

************************************************************************
* SemVer Constants
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
* Based on node semver package v7.7.1 (February 2025)
* https://github.com/npm/node-semver/releases/tag/v7.7.1
* not included yet:
* https://github.com/npm/node-semver/compare/v7.7.1...main
************************************************************************

  " Package version
  CONSTANTS version TYPE string VALUE '7.7.1' ##NEEDED.

  " Note: this is the semver.org version of the spec that it implements
  " Not necessarily the package version of this code.
  CONSTANTS semver_spec_version TYPE string VALUE '2.0.0'.

  CONSTANTS max_length TYPE i VALUE 256.
  CONSTANTS max_safe_integer TYPE i VALUE 999999998. " JS: int8

  " Max safe segment length for coercion.
  CONSTANTS max_safe_component_length TYPE i VALUE 9. " JS: 16 for int8

  " Max safe length for a build identifier. The max length minus 6 characters for
  " the shortest version with a build 0.0.0+BUILD.
  CONSTANTS max_safe_build_length TYPE i VALUE 250.

  CONSTANTS:
    BEGIN OF release_types,
      major      TYPE string VALUE 'major',
      premajor   TYPE string VALUE 'premajor',
      minor      TYPE string VALUE 'minor',
      preminor   TYPE string VALUE 'preminor',
      patch      TYPE string VALUE 'patch',
      prepatch   TYPE string VALUE 'prepatch',
      prerelease TYPE string VALUE 'prerelease',
      release    TYPE string VALUE 'release',
    END OF release_types.

ENDINTERFACE.
