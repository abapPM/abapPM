CLASS zcl_abappm_semver_fixtures DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Fixtures
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_comparator_intersection,
        c0     TYPE string,
        c1     TYPE string,
        res    TYPE abap_bool,
        incpre TYPE abap_bool,
      END OF ty_comparator_intersection,
      ty_comparator_intersections TYPE STANDARD TABLE OF ty_comparator_intersection WITH KEY c0 c1 res incpre.

    CLASS-METHODS comparator_intersection
      RETURNING
        VALUE(result) TYPE ty_comparator_intersections.

    TYPES:
      BEGIN OF ty_comparison,
        v0    TYPE string,
        v1    TYPE string,
        loose TYPE abap_bool,
      END OF ty_comparison,
      ty_comparisons TYPE STANDARD TABLE OF ty_comparison WITH KEY v0 v1 loose.

    CLASS-METHODS comparisons
      RETURNING
        VALUE(result) TYPE ty_comparisons.

    TYPES:
      BEGIN OF ty_equality,
        v0    TYPE string,
        v1    TYPE string,
        loose TYPE abap_bool,
      END OF ty_equality,
      ty_equalitys TYPE STANDARD TABLE OF ty_equality WITH KEY v0 v1 loose.

    CLASS-METHODS equality
      RETURNING
        VALUE(result) TYPE ty_equalitys.

    TYPES:
      BEGIN OF ty_increment,
        version         TYPE string,
        release         TYPE string,
        res             TYPE string,
        loose           TYPE abap_bool,
        incpre          TYPE abap_bool,
        identifier      TYPE string,
        identifier_base TYPE string,
      END OF ty_increment,
      ty_increments TYPE STANDARD TABLE OF ty_increment
        WITH KEY version release res loose incpre identifier identifier_base.

    CLASS-METHODS increments
      RETURNING
        VALUE(result) TYPE ty_increments.

    TYPES:
      BEGIN OF ty_invalid_version,
        value  TYPE string,
        reason TYPE string,
        loose  TYPE abap_bool,
      END OF ty_invalid_version,
      ty_invalid_versions TYPE STANDARD TABLE OF ty_invalid_version WITH KEY value reason loose.

    CLASS-METHODS invalid_versions
      RETURNING
        VALUE(result) TYPE ty_invalid_versions.

    TYPES:
      BEGIN OF ty_range,
        range   TYPE string,
        version TYPE string,
        loose   TYPE abap_bool,
        incpre  TYPE abap_bool,
      END OF ty_range,
      ty_ranges TYPE STANDARD TABLE OF ty_range WITH KEY range version loose incpre.

    CLASS-METHODS range_exclude
      RETURNING
        VALUE(result) TYPE ty_ranges.

    CLASS-METHODS range_include
      RETURNING
        VALUE(result) TYPE ty_ranges.

    TYPES:
      BEGIN OF ty_range_intersection,
        r0  TYPE string,
        r1  TYPE string,
        res TYPE abap_bool,
      END OF ty_range_intersection,
      ty_range_intersections TYPE STANDARD TABLE OF ty_range_intersection WITH KEY r0 r1 res.

    CLASS-METHODS range_intersection
      RETURNING
        VALUE(result) TYPE ty_range_intersections.

    TYPES:
      BEGIN OF ty_range_parse,
        range  TYPE string,
        res    TYPE string,
        loose  TYPE abap_bool,
        incpre TYPE abap_bool,
      END OF ty_range_parse,
      ty_range_parses TYPE STANDARD TABLE OF ty_range_parse WITH KEY range res loose incpre.

    CLASS-METHODS range_parse
      RETURNING
        VALUE(result) TYPE ty_range_parses.

    TYPES:
      BEGIN OF ty_version_range,
        range   TYPE string,
        version TYPE string,
        loose   TYPE abap_bool,
        incpre  TYPE abap_bool,
      END OF ty_version_range,
      ty_version_ranges TYPE STANDARD TABLE OF ty_version_range WITH KEY range version loose incpre.

    CLASS-METHODS version_gt_range
      RETURNING
        VALUE(result) TYPE ty_version_ranges.

    CLASS-METHODS version_lt_range
      RETURNING
        VALUE(result) TYPE ty_version_ranges.

    CLASS-METHODS version_not_gt_range
      RETURNING
        VALUE(result) TYPE ty_version_ranges.

    CLASS-METHODS version_not_lt_range
      RETURNING
        VALUE(result) TYPE ty_version_ranges.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_semver_fixtures IMPLEMENTATION.


  METHOD comparator_intersection.
    " [c0, c1, expected intersection, includePrerelease]

    result = VALUE #(
      " One is a Version
      ( c0 = '1.3.0' c1 = '>=1.3.0' res = abap_true )
      ( c0 = '1.3.0' c1 = '>1.3.0' res = abap_false )
      ( c0 = '>=1.3.0' c1 = '1.3.0' res = abap_true )
      ( c0 = '>1.3.0' c1 = '1.3.0' res = abap_false )
      " Same direction increasing
      ( c0 = '>1.3.0' c1 = '>1.2.0' res = abap_true )
      ( c0 = '>1.2.0' c1 = '>1.3.0' res = abap_true )
      ( c0 = '>=1.2.0' c1 = '>1.3.0' res = abap_true )
      ( c0 = '>1.2.0' c1 = '>=1.3.0' res = abap_true )
      " Same direction decreasing
      ( c0 = '<1.3.0' c1 = '<1.2.0' res = abap_true )
      ( c0 = '<1.2.0' c1 = '<1.3.0' res = abap_true )
      ( c0 = '<=1.2.0' c1 = '<1.3.0' res = abap_true )
      ( c0 = '<1.2.0' c1 = '<=1.3.0' res = abap_true )
      " Different directions, same semver and inclusive operator
      ( c0 = '>=1.3.0' c1 = '<=1.3.0' res = abap_true )
      ( c0 = '>=v1.3.0' c1 = '<=1.3.0' res = abap_true )
      ( c0 = '>=1.3.0' c1 = '>=1.3.0' res = abap_true )
      ( c0 = '<=1.3.0' c1 = '<=1.3.0' res = abap_true )
      ( c0 = '<=1.3.0' c1 = '<=v1.3.0' res = abap_true )
      ( c0 = '>1.3.0' c1 = '<=1.3.0' res = abap_false )
      ( c0 = '>=1.3.0' c1 = '<1.3.0' res = abap_false )
      " Opposite matching directions
      ( c0 = '>1.0.0' c1 = '<2.0.0' res = abap_true )
      ( c0 = '>=1.0.0' c1 = '<2.0.0' res = abap_true )
      ( c0 = '>=1.0.0' c1 = '<=2.0.0' res = abap_true )
      ( c0 = '>1.0.0' c1 = '<=2.0.0' res = abap_true )
      ( c0 = '<=2.0.0' c1 = '>1.0.0' res = abap_true )
      ( c0 = '<=1.0.0' c1 = '>=2.0.0' res = abap_false )
      ( c0 = '' c1 = '' res = abap_true )
      ( c0 = '' c1 = '>1.0.0' res = abap_true )
      ( c0 = '<=2.0.0' c1 = '' res = abap_true )
      ( c0 = '<0.0.0' c1 = '<0.1.0' res = abap_false )
      ( c0 = '<0.1.0' c1 = '<0.0.0' res = abap_false )
      ( c0 = '<0.0.0-0' c1 = '<0.1.0' res = abap_false )
      ( c0 = '<0.1.0' c1 = '<0.0.0-0' res = abap_false )
      ( c0 = '<0.0.0-0' c1 = '<0.1.0' res = abap_false incpre = abap_true )
      ( c0 = '<0.1.0' c1 = '<0.0.0-0' res = abap_false incpre = abap_true ) ).

  ENDMETHOD.


  METHOD comparisons.
    " [version1, version2, options]

    " version1 should be greater than version2
    " used by the cmp, eq, gt, lt, and neq tests
    result = VALUE #(
      ( v0 = '0.0.0' v1 = '0.0.0-foo' )
      ( v0 = '0.0.1' v1 = '0.0.0' )
      ( v0 = '1.0.0' v1 = '0.9.9' )
      ( v0 = '0.10.0' v1 = '0.9.0' )
      ( v0 = '0.99.0' v1 = '0.10.0' )
      ( v0 = '2.0.0' v1 = '1.2.3' loose = abap_false )
      ( v0 = 'v0.0.0' v1 = '0.0.0-foo' loose = abap_true )
      ( v0 = 'v0.0.1' v1 = '0.0.0' loose = abap_true )
      ( v0 = 'v1.0.0' v1 = '0.9.9' loose = abap_true )
      ( v0 = 'v0.10.0' v1 = '0.9.0' loose = abap_true )
      ( v0 = 'v0.99.0' v1 = '0.10.0' loose = abap_true )
      ( v0 = 'v2.0.0' v1 = '1.2.3' loose = abap_true )
      ( v0 = '0.0.0' v1 = 'v0.0.0-foo' loose = abap_true )
      ( v0 = '0.0.1' v1 = 'v0.0.0' loose = abap_true )
      ( v0 = '1.0.0' v1 = 'v0.9.9' loose = abap_true )
      ( v0 = '0.10.0' v1 = 'v0.9.0' loose = abap_true )
      ( v0 = '0.99.0' v1 = 'v0.10.0' loose = abap_true )
      ( v0 = '2.0.0' v1 = 'v1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = '1.2.3-asdf' )
      ( v0 = '1.2.3' v1 = '1.2.3-4' )
      ( v0 = '1.2.3' v1 = '1.2.3-4-foo' )
      ( v0 = '1.2.3-5-foo' v1 = '1.2.3-5' )
      ( v0 = '1.2.3-5' v1 = '1.2.3-4' )
      ( v0 = '1.2.3-5-foo' v1 = '1.2.3-5-Foo' )
      ( v0 = '3.0.0' v1 = '2.7.2+asdf' )
      ( v0 = '1.2.3-a.10' v1 = '1.2.3-a.5' )
      ( v0 = '1.2.3-a.b' v1 = '1.2.3-a.5' )
      ( v0 = '1.2.3-a.b' v1 = '1.2.3-a' )
      ( v0 = '1.2.3-a.b.c.10.d.5' v1 = '1.2.3-a.b.c.5.d.100' )
      ( v0 = '1.2.3-r2' v1 = '1.2.3-r100' )
      ( v0 = '1.2.3-r100' v1 = '1.2.3-R2' ) ).

  ENDMETHOD.


  METHOD equality.
    " [version1, version2, expected result]

    " version1 should be equivalent to version2
    result = VALUE #(
      ( v0 = '1.2.3' v1 = 'v1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = '=1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = 'v 1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = '= 1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = ' v1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = ' =1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = ' v 1.2.3' loose = abap_true )
      ( v0 = '1.2.3' v1 = ' = 1.2.3' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = 'v1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = '=1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = 'v 1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = '= 1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = ' v1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = ' =1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = ' v 1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-0' v1 = ' = 1.2.3-0' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = 'v1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = '=1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = 'v 1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = '= 1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = ' v1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = ' =1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = ' v 1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-1' v1 = ' = 1.2.3-1' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = 'v1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = '=1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = 'v 1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = '= 1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = ' v1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = ' =1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = ' v 1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta' v1 = ' = 1.2.3-beta' loose = abap_true )
      ( v0 = '1.2.3-beta+build' v1 = ' = 1.2.3-beta+otherbuild' loose = abap_true )
      ( v0 = '1.2.3+build' v1 = ' = 1.2.3+otherbuild' loose = abap_true )
      ( v0 = '1.2.3-beta+build' v1 = '1.2.3-beta+otherbuild' )
      ( v0 = '1.2.3+build' v1 = '1.2.3+otherbuild' )
      ( v0 = '  v1.2.3+build' v1 = '1.2.3+otherbuild' ) ).

  ENDMETHOD.


  METHOD increments.
    " [version, inc, result, options, identifier, identifier_base]

    " inc(version, re, options, identifier, identifier_base) -> result
    result = VALUE #(
      ( version = '1.2.3' release = 'major' res = '2.0.0' )
      ( version = '1.2.3' release = 'minor' res = '1.3.0' )
      ( version = '1.2.3' release = 'patch' res = '1.2.4' )
      ( version = '1.2.3tag' release = 'major' res = '2.0.0' loose = abap_true )
      ( version = '1.2.3-tag' release = 'major' res = '2.0.0' )
      ( version = '1.2.3' release = 'fake' res = '' )
      ( version = '1.2.0-0' release = 'patch' res = '1.2.0' )
      ( version = 'fake' release = 'major' res = '' )
      ( version = '1.2.3-4' release = 'major' res = '2.0.0' )
      ( version = '1.2.3-4' release = 'minor' res = '1.3.0' )
      ( version = '1.2.3-4' release = 'patch' res = '1.2.3' )
      ( version = '1.2.3-alpha.0.beta' release = 'major' res = '2.0.0' )
      ( version = '1.2.3-alpha.0.beta' release = 'minor' res = '1.3.0' )
      ( version = '1.2.3-alpha.0.beta' release = 'patch' res = '1.2.3' )
      ( version = '1.2.4' release = 'prerelease' res = '1.2.5-0' )
      ( version = '1.2.3-0' release = 'prerelease' res = '1.2.3-1' )
      ( version = '1.2.3-alpha.0' release = 'prerelease' res = '1.2.3-alpha.1' )
      ( version = '1.2.3-alpha.1' release = 'prerelease' res = '1.2.3-alpha.2' )
      ( version = '1.2.3-alpha.2' release = 'prerelease' res = '1.2.3-alpha.3' )
      ( version = '1.2.3-alpha.0.beta' release = 'prerelease' res = '1.2.3-alpha.1.beta' )
      ( version = '1.2.3-alpha.1.beta' release = 'prerelease' res = '1.2.3-alpha.2.beta' )
      ( version = '1.2.3-alpha.2.beta' release = 'prerelease' res = '1.2.3-alpha.3.beta' )
      ( version = '1.2.3-alpha.10.0.beta' release = 'prerelease' res = '1.2.3-alpha.10.1.beta' )
      ( version = '1.2.3-alpha.10.1.beta' release = 'prerelease' res = '1.2.3-alpha.10.2.beta' )
      ( version = '1.2.3-alpha.10.2.beta' release = 'prerelease' res = '1.2.3-alpha.10.3.beta' )
      ( version = '1.2.3-alpha.10.beta.0' release = 'prerelease' res = '1.2.3-alpha.10.beta.1' )
      ( version = '1.2.3-alpha.10.beta.1' release = 'prerelease' res = '1.2.3-alpha.10.beta.2' )
      ( version = '1.2.3-alpha.10.beta.2' release = 'prerelease' res = '1.2.3-alpha.10.beta.3' )
      ( version = '1.2.3-alpha.9.beta' release = 'prerelease' res = '1.2.3-alpha.10.beta' )
      ( version = '1.2.3-alpha.10.beta' release = 'prerelease' res = '1.2.3-alpha.11.beta' )
      ( version = '1.2.3-alpha.11.beta' release = 'prerelease' res = '1.2.3-alpha.12.beta' )
      ( version = '1.2.0' release = 'prepatch' res = '1.2.1-0' )
      ( version = '1.2.0-1' release = 'prepatch' res = '1.2.1-0' )
      ( version = '1.2.0' release = 'preminor' res = '1.3.0-0' )
      ( version = '1.2.3-1' release = 'preminor' res = '1.3.0-0' )
      ( version = '1.2.0' release = 'premajor' res = '2.0.0-0' )
      ( version = '1.2.3-1' release = 'premajor' res = '2.0.0-0' )
      ( version = '1.2.0-1' release = 'minor' res = '1.2.0' )
      ( version = '1.0.0-1' release = 'major' res = '1.0.0' )
      " identifier
      ( version = '1.2.3' release = 'major' res = '2.0.0' identifier = 'dev' )
      ( version = '1.2.3' release = 'minor' res = '1.3.0' identifier = 'dev' )
      ( version = '1.2.3' release = 'patch' res = '1.2.4' identifier = 'dev' )
      ( version = '1.2.3tag' release = 'major' res = '2.0.0' loose = abap_true identifier = 'dev' )
      ( version = '1.2.3-tag' release = 'major' res = '2.0.0' identifier = 'dev' )
      ( version = '1.2.3' release = 'fake' res = '' identifier = 'dev' )
      ( version = '1.2.0-0' release = 'patch' res = '1.2.0' identifier = 'dev' )
      ( version = 'fake' release = 'major' res = '' identifier = 'dev' )
      ( version = '1.2.3-4' release = 'major' res = '2.0.0' identifier = 'dev' )
      ( version = '1.2.3-4' release = 'minor' res = '1.3.0' identifier = 'dev' )
      ( version = '1.2.3-4' release = 'patch' res = '1.2.3' identifier = 'dev' )
      ( version = '1.2.3-alpha.0.beta' release = 'major' res = '2.0.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.0.beta' release = 'minor' res = '1.3.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.0.beta' release = 'patch' res = '1.2.3' identifier = 'dev' )
      ( version = '1.2.4' release = 'prerelease' res = '1.2.5-dev.0' identifier = 'dev' )
      ( version = '1.2.3-0' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.0' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.0' release = 'prerelease' res = '1.2.3-alpha.1' identifier = 'alpha' )
      ( version = '1.2.3-alpha.0.beta' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.0.beta' release = 'prerelease' res = '1.2.3-alpha.1.beta' identifier = 'alpha' )
      ( version = '1.2.3-alpha.10.0.beta' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.10.0.beta' release = 'prerelease' res = '1.2.3-alpha.10.1.beta' identifier = 'alpha' )
      ( version = '1.2.3-alpha.10.1.beta' release = 'prerelease' res = '1.2.3-alpha.10.2.beta' identifier = 'alpha' )
      ( version = '1.2.3-alpha.10.2.beta' release = 'prerelease' res = '1.2.3-alpha.10.3.beta' identifier = 'alpha' )
      ( version = '1.2.3-alpha.10.beta.0' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.10.beta.0' release = 'prerelease' res = '1.2.3-alpha.10.beta.1' identifier = 'alpha' )
      ( version = '1.2.3-alpha.10.beta.1' release = 'prerelease' res = '1.2.3-alpha.10.beta.2' identifier = 'alpha' )
      ( version = '1.2.3-alpha.10.beta.2' release = 'prerelease' res = '1.2.3-alpha.10.beta.3' identifier = 'alpha' )
      ( version = '1.2.3-alpha.9.beta' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' )
      ( version = '1.2.3-alpha.9.beta' release = 'prerelease' res = '1.2.3-alpha.10.beta' identifier = 'alpha' )
      ( version = '1.2.3-alpha.10.beta' release = 'prerelease' res = '1.2.3-alpha.11.beta' identifier = 'alpha' )
      ( version = '1.2.3-alpha.11.beta' release = 'prerelease' res = '1.2.3-alpha.12.beta' identifier = 'alpha' )
      ( version = '1.2.0' release = 'prepatch' res = '1.2.1-dev.0' identifier = 'dev' )
      ( version = '1.2.0-1' release = 'prepatch' res = '1.2.1-dev.0' identifier = 'dev' )
      ( version = '1.2.0' release = 'preminor' res = '1.3.0-dev.0' identifier = 'dev' )
      ( version = '1.2.3-1' release = 'preminor' res = '1.3.0-dev.0' identifier = 'dev' )
      ( version = '1.2.0' release = 'premajor' res = '2.0.0-dev.0' identifier = 'dev' )
      ( version = '1.2.3-1' release = 'premajor' res = '2.0.0-dev.0' identifier = 'dev' )
      ( version = '1.2.3-1' release = 'premajor' res = '2.0.0-dev.1' identifier = 'dev' identifier_base = '1' )
      ( version = '1.2.0-1' release = 'minor' res = '1.2.0' identifier = 'dev' )
      ( version = '1.0.0-1' release = 'major' res = '1.0.0' identifier = 'dev' )
      ( version = '1.2.3-dev.bar' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' )
      " prerelease
      ( version = '1.2.3-0' release = 'prerelease' res = '1.2.3-1.0' identifier = '1' )
      ( version = '1.2.3-1.0' release = 'prerelease' res = '1.2.3-1.1' identifier = '1' )
      ( version = '1.2.3-1.1' release = 'prerelease' res = '1.2.3-1.2' identifier = '1' )
      ( version = '1.2.3-1.1' release = 'prerelease' res = '1.2.3-2.0' identifier = '2' )
      ( version = '1.2.0-1' release = 'prerelease' res = '1.2.0-alpha.0' identifier = 'alpha' identifier_base = '0' )
      ( version = '1.2.1' release = 'prerelease' res = '1.2.2-alpha.0' identifier = 'alpha' identifier_base = '0' )
      ( version = '0.2.0' release = 'prerelease' res = '0.2.1-alpha.0' identifier = 'alpha' identifier_base = '0' )
      ( version = '1.2.2' release = 'prerelease' res = '1.2.3-alpha.1' identifier = 'alpha' identifier_base = '1' )
      ( version = '1.2.3' release = 'prerelease' res = '1.2.4-alpha.1' identifier = 'alpha' identifier_base = '1' )
      ( version = '1.2.4' release = 'prerelease' res = '1.2.5-alpha.1' identifier = 'alpha' identifier_base = '1' )
      ( version = '1.2.0' release = 'prepatch' res = '1.2.1-dev.1' identifier = 'dev' identifier_base = '1' )
      ( version = '1.2.0-1' release = 'prepatch' res = '1.2.1-dev.1' identifier = 'dev' identifier_base = '1' )
      ( version = '1.2.0' release = 'premajor' res = '2.0.0-dev.0' identifier = 'dev' identifier_base = '0' )
      ( version = '1.2.3-1' release = 'premajor' res = '2.0.0-dev.0' identifier = 'dev' identifier_base = '0' )
      ( version = '1.2.3-dev.bar' release = 'prerelease' res = '1.2.3-dev.0' identifier = 'dev' identifier_base = '0' )
      ( version = '1.2.3-dev.bar' release = 'prerelease' res = '1.2.3-dev.1' identifier = 'dev' identifier_base = '1' )
      ( version = '1.2.3-dev.bar' release = 'prerelease' res = '1.2.3-dev.bar.0' identifier = '' identifier_base = '0' )
      ( version = '1.2.3-dev.bar' release = 'prerelease' res = '1.2.3-dev.bar.1' identifier = '' identifier_base = '1' )
      ( version = '1.2.0' release = 'preminor' res = '1.3.0-dev.1' identifier = 'dev' identifier_base = '1' )
      ( version = '1.2.3-1' release = 'preminor' res = '1.3.0-dev.0' identifier = 'dev' )
      ( version = '1.2.0' release = 'prerelease' res = '1.2.1-1' identifier = '' identifier_base = '1' )
      ( version = '1.2.0-1' release = 'prerelease' res = '1.2.0-alpha' identifier = 'alpha' identifier_base = 'false' )
      ( version = '1.2.1' release = 'prerelease' res = '1.2.2-alpha' identifier = 'alpha' identifier_base = 'false' )
      ( version = '1.2.2' release = 'prerelease' res = '1.2.3-alpha' identifier = 'alpha' identifier_base = 'false' )
      ( version = '1.2.0' release = 'prepatch' res = '1.2.1-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.0-1' release = 'prepatch' res = '1.2.1-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.0' release = 'premajor' res = '2.0.0-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.3-1' release = 'premajor' res = '2.0.0-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.3-dev.bar' release = 'prerelease' res = '1.2.3-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.3-dev.bar' release = 'prerelease' res = '1.2.3-dev.baz' identifier = 'dev.baz' identifier_base = 'false' )
      ( version = '1.2.0' release = 'preminor' res = '1.3.0-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.3-1' release = 'preminor' res = '1.3.0-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.3-dev' release = 'prerelease' res = '' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.0-dev' release = 'premajor' res = '2.0.0-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.0-dev' release = 'preminor' res = '1.3.0-beta' identifier = 'beta' identifier_base = 'false' )
      ( version = '1.2.0-dev' release = 'prepatch' res = '1.2.1-dev' identifier = 'dev' identifier_base = 'false' )
      ( version = '1.2.0' release = 'prerelease' res = '' identifier = '' identifier_base = 'false' )
      ( version = '1.0.0-rc.1+build.4' release = 'prerelease' res = '1.0.0-rc.2' identifier = 'rc' identifier_base = 'false' ) ).

  ENDMETHOD.


  METHOD invalid_versions.
    " [value, reason, options]

    " none of these are semvers
    result = VALUE #(
      ( value = |{ repeat( val = '1' occ = zif_abappm_semver_constants=>max_length ) }.0.0| reason = 'too long' )
      ( value = |{ zif_abappm_semver_constants=>max_safe_integer }0.0.0| reason = 'too big' )
      ( value = |0.{ zif_abappm_semver_constants=>max_safe_integer }0.0| reason = 'too big' )
      ( value = |0.0.{ zif_abappm_semver_constants=>max_safe_integer }0| reason = 'too big' )
      ( value = 'hello, world' reason = 'not a version' )
      ( value = 'hello, world' reason = 'even loose, it''s still junk' loose = abap_true )
      ( value = 'xyz' reason = 'even loose as an opt, same' loose = abap_true )
      ( value = 'NOT VALID' reason = 'nothing like a version' )
      ( value = '1.2.3.4' reason = 'patch of a patch' )
      ( value = '1.2' reason = 'no patch' )
      ( value = '1' reason = 'no minor' )
      ( value = '' reason = 'no data' ) ).
    " The following test cases can't happen in ABAP due to type system
    " ( value = /a regexp/ reason = 'regexp is not a string' )
    " ( value = /1.2.3/ reason = 'semver-ish regexp is not a string' )
    " ( value = { toString: () => '1.2.3' } reason = 'obj with a tostring is not a string' )

  ENDMETHOD.


  METHOD range_exclude.
    " [range, version, options]

    " version should not be included by range
    result = VALUE #(
      ( range = '1.0.0 - 2.0.0' version = '2.2.3' )
      ( range = '1.2.3+asdf - 2.4.3+asdf' version = '1.2.3-pre.2' )
      ( range = '1.2.3+asdf - 2.4.3+asdf' version = '2.4.3-alpha' )
      ( range = '^1.2.3+build' version = '2.0.0' )
      ( range = '^1.2.3+build' version = '1.2.0' )
      ( range = '^1.2.3' version = '1.2.3-pre' )
      ( range = '^1.2' version = '1.2.0-pre' )
      ( range = '>1.2' version = '1.3.0-beta' )
      ( range = '<=1.2.3' version = '1.2.3-beta' )
      ( range = '^1.2.3' version = '1.2.3-beta' )
      ( range = '=0.7.x' version = '0.7.0-asdf' )
      ( range = '>=0.7.x' version = '0.7.0-asdf' )
      ( range = '<=0.7.x' version = '0.7.0-asdf' )
      "( range = '1' version = '1.0.0beta' { loose: 420 } )
      ( range = '<1' version = '1.0.0beta' loose = abap_true )
      ( range = '< 1' version = '1.0.0beta' loose = abap_true )
      ( range = '1.0.0' version = '1.0.1' )
      ( range = '>=1.0.0' version = '0.0.0' )
      ( range = '>=1.0.0' version = '0.0.1' )
      ( range = '>=1.0.0' version = '0.1.0' )
      ( range = '>1.0.0' version = '0.0.1' )
      ( range = '>1.0.0' version = '0.1.0' )
      ( range = '<=2.0.0' version = '3.0.0' )
      ( range = '<=2.0.0' version = '2.9999.9999' )
      ( range = '<=2.0.0' version = '2.2.9' )
      ( range = '<2.0.0' version = '2.9999.9999' )
      ( range = '<2.0.0' version = '2.2.9' )
      ( range = '>=0.1.97' version = 'v0.1.93' loose = abap_true )
      ( range = '>=0.1.97' version = '0.1.93' )
      ( range = '0.1.20 || 1.2.4' version = '1.2.3' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.0.3' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.2.2' )
      "( range = '2.x.x' version = '1.1.3' { loose: NaN } )
      ( range = '2.x.x' version = '3.1.3' )
      ( range = '1.2.x' version = '1.3.3' )
      ( range = '1.2.x || 2.x' version = '3.1.3' )
      ( range = '1.2.x || 2.x' version = '1.1.3' )
      ( range = '2.*.*' version = '1.1.3' )
      ( range = '2.*.*' version = '3.1.3' )
      ( range = '1.2.*' version = '1.3.3' )
      ( range = '1.2.* || 2.*' version = '3.1.3' )
      ( range = '1.2.* || 2.*' version = '1.1.3' )
      ( range = '2' version = '1.1.2' )
      ( range = '2.3' version = '2.4.1' )
      ( range = '~0.0.1' version = '0.1.0-alpha' )
      ( range = '~0.0.1' version = '0.1.0' )
      ( range = '~2.4' version = '2.5.0' ) " >=2.4.0 <2.5.0
      ( range = '~2.4' version = '2.3.9' )
      ( range = '~>3.2.1' version = '3.3.2' ) " >=3.2.1 <3.3.0
      ( range = '~>3.2.1' version = '3.2.0' ) " >=3.2.1 <3.3.0
      ( range = '~1' version = '0.2.3' ) " >=1.0.0 <2.0.0
      ( range = '~>1' version = '2.2.3' )
      ( range = '~1.0' version = '1.1.0' ) " >=1.0.0 <1.1.0
      ( range = '<1' version = '1.0.0' )
      ( range = '>=1.2' version = '1.1.1' )
      ( range = '1' version = '2.0.0beta' loose = abap_true )
      ( range = '~v0.5.4-beta' version = '0.5.4-alpha' )
      ( range = '=0.7.x' version = '0.8.2' )
      ( range = '>=0.7.x' version = '0.6.2' )
      ( range = '<0.7.x' version = '0.7.2' )
      ( range = '<1.2.3' version = '1.2.3-beta' )
      ( range = '=1.2.3' version = '1.2.3-beta' )
      ( range = '>1.2' version = '1.2.8' )
      ( range = '^0.0.1' version = '0.0.2-alpha' )
      ( range = '^0.0.1' version = '0.0.2' )
      ( range = '^1.2.3' version = '2.0.0-alpha' )
      ( range = '^1.2.3' version = '1.2.2' )
      ( range = '^1.2' version = '1.1.9' )
      ( range = '*' version = 'v1.2.3-foo' loose = abap_true )
      " invalid versions never satisfy, but shouldn't throw
      ( range = '*' version = 'not a version' )
      ( range = '>=2' version = 'glorp' )
      ( range = '>=2' version = '' )
      " incpre
      ( range = '2.x' version = '3.0.0-pre.0' incpre = abap_true )
      ( range = '^1.0.0' version = '1.0.0-rc1' incpre = abap_true )
      ( range = '^1.0.0' version = '2.0.0-rc1' incpre = abap_true )
      ( range = '^1.2.3-rc2' version = '2.0.0' incpre = abap_true )
      ( range = '^1.0.0' version = '2.0.0-rc1' )
      " from to
      ( range = '1 - 2' version = '3.0.0-pre' incpre = abap_true )
      ( range = '1 - 2' version = '2.0.0-pre' )
      ( range = '1 - 2' version = '1.0.0-pre' )
      ( range = '1.0 - 2' version = '1.0.0-pre' )
      " prerelease
      ( range = '1.1.x' version = '1.0.0-a' )
      ( range = '1.1.x' version = '1.1.0-a' )
      ( range = '1.1.x' version = '1.2.0-a' )
      ( range = '1.1.x' version = '1.2.0-a' incpre = abap_true )
      ( range = '1.1.x' version = '1.0.0-a' incpre = abap_true )
      ( range = '1.x' version = '1.0.0-a' )
      ( range = '1.x' version = '1.1.0-a' )
      ( range = '1.x' version = '1.2.0-a' )
      ( range = '1.x' version = '0.0.0-a' incpre = abap_true )
      ( range = '1.x' version = '2.0.0-a' incpre = abap_true )
      ( range = '>=1.0.0 <1.1.0' version = '1.1.0' )
      ( range = '>=1.0.0 <1.1.0' version = '1.1.0' incpre = abap_true )
      ( range = '>=1.0.0 <1.1.0' version = '1.1.0-pre' )
      ( range = '>=1.0.0 <1.1.0-pre' version = '1.1.0-pre' )
      ( range = '== 1.0.0 || foo' version = '2.0.0' loose = abap_true ) ).

  ENDMETHOD.


  METHOD range_include.
    " [range, version, options]

    " version should be included by range
    result = VALUE #(
      ( range = '1.0.0 - 2.0.0' version = '1.2.3' )
      ( range = '^1.2.3+build' version = '1.2.3' )
      ( range = '^1.2.3+build' version = '1.3.0' )
      ( range = '1.2.3-pre+asdf - 2.4.3-pre+asdf' version = '1.2.3' )
      ( range = '1.2.3pre+asdf - 2.4.3-pre+asdf' version = '1.2.3' loose = abap_true )
      ( range = '1.2.3-pre+asdf - 2.4.3pre+asdf' version = '1.2.3' loose = abap_true )
      ( range = '1.2.3pre+asdf - 2.4.3pre+asdf' version = '1.2.3' loose = abap_true )
      ( range = '1.2.3-pre+asdf - 2.4.3-pre+asdf' version = '1.2.3-pre.2' )
      ( range = '1.2.3-pre+asdf - 2.4.3-pre+asdf' version = '2.4.3-alpha' )
      ( range = '1.2.3+asdf - 2.4.3+asdf' version = '1.2.3' )
      ( range = '1.0.0' version = '1.0.0' )
      ( range = '>=*' version = '0.2.4' )
      ( range = '' version = '1.0.0' )
      ( range = '*' version = '1.2.3' )
      " The following test cases can't happen in ABAP due to type system
      "( range = '*' version = 'v1.2.3', { loose: 123 } )
      "( range = '>=1.0.0' version = '1.0.0', /asdf/ )
      "( range = '>=1.0.0' version = '1.0.1', { loose: null } )
      "( range = '>=1.0.0' version = '1.1.0', { loose: 0 } )
      "( range = '>1.0.0' version = '1.0.1', { loose: undefined } )
      ( range = '>1.0.0' version = '1.1.0' )
      ( range = '<=2.0.0' version = '2.0.0' )
      ( range = '<=2.0.0' version = '1.9999.9999' )
      ( range = '<=2.0.0' version = '0.2.9' )
      ( range = '<2.0.0' version = '1.9999.9999' )
      ( range = '<2.0.0' version = '0.2.9' )
      ( range = '>= 1.0.0' version = '1.0.0' )
      ( range = '>=  1.0.0' version = '1.0.1' )
      ( range = '>=   1.0.0' version = '1.1.0' )
      ( range = '> 1.0.0' version = '1.0.1' )
      ( range = '>  1.0.0' version = '1.1.0' )
      ( range = '<=   2.0.0' version = '2.0.0' )
      ( range = '<= 2.0.0' version = '1.9999.9999' )
      ( range = '<=  2.0.0' version = '0.2.9' )
      ( range = '<    2.0.0' version = '1.9999.9999' )
      ( range = |<\t2.0.0| version = '0.2.9' )
      ( range = '>=0.1.97' version = 'v0.1.97' loose = abap_true )
      ( range = '>=0.1.97' version = '0.1.97' )
      ( range = '0.1.20 || 1.2.4' version = '1.2.4' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.0.0' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.2.3' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.2.4' )
      ( range = '||' version = '1.3.4' )
      ( range = '2.x.x' version = '2.1.3' )
      ( range = '1.2.x' version = '1.2.3' )
      ( range = '1.2.x || 2.x' version = '2.1.3' )
      ( range = '1.2.x || 2.x' version = '1.2.3' )
      ( range = 'x' version = '1.2.3' )
      ( range = '2.*.*' version = '2.1.3' )
      ( range = '1.2.*' version = '1.2.3' )
      ( range = '1.2.* || 2.*' version = '2.1.3' )
      ( range = '1.2.* || 2.*' version = '1.2.3' )
      ( range = '*' version = '1.2.3' )
      ( range = '2' version = '2.1.2' )
      ( range = '2.3' version = '2.3.1' )
      ( range = '~0.0.1' version = '0.0.1' )
      ( range = '~0.0.1' version = '0.0.2' )
      ( range = '~x' version = '0.0.9' ) " >=2.4.0 <2.5.0
      ( range = '~2' version = '2.0.9' ) " >=2.4.0 <2.5.0
      ( range = '~2.4' version = '2.4.0' ) " >=2.4.0 <2.5.0
      ( range = '~2.4' version = '2.4.5' )
      ( range = '~>3.2.1' version = '3.2.2' ) " >=3.2.1 <3.3.0,
      ( range = '~1' version = '1.2.3' ) " >=1.0.0 <2.0.0
      ( range = '~>1' version = '1.2.3' )
      ( range = '~> 1' version = '1.2.3' )
      ( range = '~1.0' version = '1.0.2' ) " >=1.0.0 <1.1.0,
      ( range = '~ 1.0' version = '1.0.2' )
      ( range = '~ 1.0.3' version = '1.0.12' )
      ( range = '~ 1.0.3alpha' version = '1.0.12' loose = abap_true )
      ( range = '>=1' version = '1.0.0' )
      ( range = '>= 1' version = '1.0.0' )
      ( range = '<1.2' version = '1.1.1' )
      ( range = '< 1.2' version = '1.1.1' )
      ( range = '~v0.5.4-pre' version = '0.5.5' )
      ( range = '~v0.5.4-pre' version = '0.5.4' )
      ( range = '=0.7.x' version = '0.7.2' )
      ( range = '<=0.7.x' version = '0.7.2' )
      ( range = '>=0.7.x' version = '0.7.2' )
      ( range = '<=0.7.x' version = '0.6.2' )
      ( range = '~1.2.1 >=1.2.3' version = '1.2.3' )
      ( range = '~1.2.1 =1.2.3' version = '1.2.3' )
      ( range = '~1.2.1 1.2.3' version = '1.2.3' )
      ( range = '~1.2.1 >=1.2.3 1.2.3' version = '1.2.3' )
      ( range = '~1.2.1 1.2.3 >=1.2.3' version = '1.2.3' )
      ( range = '>=1.2.1 1.2.3' version = '1.2.3' )
      ( range = '1.2.3 >=1.2.1' version = '1.2.3' )
      ( range = '>=1.2.3 >=1.2.1' version = '1.2.3' )
      ( range = '>=1.2.1 >=1.2.3' version = '1.2.3' )
      ( range = '>=1.2' version = '1.2.8' )
      ( range = '^1.2.3' version = '1.8.1' )
      ( range = '^0.1.2' version = '0.1.2' )
      ( range = '^0.1' version = '0.1.2' )
      ( range = '^0.0.1' version = '0.0.1' )
      ( range = '^1.2' version = '1.4.2' )
      ( range = '^1.2 ^1' version = '1.4.2' )
      ( range = '^1.2.3-alpha' version = '1.2.3-pre' )
      ( range = '^1.2.0-alpha' version = '1.2.0-pre' )
      ( range = '^0.0.1-alpha' version = '0.0.1-beta' )
      ( range = '^0.0.1-alpha' version = '0.0.1' )
      ( range = '^0.1.1-alpha' version = '0.1.1-beta' )
      ( range = '^x' version = '1.2.3' )
      ( range = 'x - 1.0.0' version = '0.9.7' )
      ( range = 'x - 1.x' version = '0.9.7' )
      ( range = '1.0.0 - x' version = '1.9.7' )
      ( range = '1.x - x' version = '1.9.7' )
      ( range = '<=7.x' version = '7.9.9' )
      ( range = '2.x' version = '2.0.0-pre.0' incpre = abap_true )
      ( range = '2.x' version = '2.1.0-pre.0' incpre = abap_true )
      ( range = '1.1.x' version = '1.1.0-a' incpre = abap_true )
      ( range = '1.1.x' version = '1.1.1-a' incpre = abap_true )
      ( range = '*' version = '1.0.0-rc1' incpre = abap_true )
      ( range = '^1.0.0-0' version = '1.0.1-rc1' incpre = abap_true )
      ( range = '^1.0.0-rc2' version = '1.0.1-rc1' incpre = abap_true )
      ( range = '^1.0.0' version = '1.0.1-rc1' incpre = abap_true )
      ( range = '^1.0.0' version = '1.1.0-rc1' incpre = abap_true )
      ( range = '1 - 2' version = '2.0.0-pre' incpre = abap_true )
      ( range = '1 - 2' version = '1.0.0-pre' incpre = abap_true )
      ( range = '1.0 - 2' version = '1.0.0-pre' incpre = abap_true )
      ( range = '=0.7.x' version = '0.7.0-asdf' incpre = abap_true )
      ( range = '>=0.7.x' version = '0.7.0-asdf' incpre = abap_true )
      ( range = '<=0.7.x' version = '0.7.0-asdf' incpre = abap_true )
      ( range = '>=1.0.0 <=1.1.0' version = '1.1.0-pre' incpre = abap_true ) ).

  ENDMETHOD.


  METHOD range_intersection.
    " [r0, r1, expected intersection]

    result = VALUE #(
      ( r0 = '1.3.0 || <1.0.0 >2.0.0' r1 = '1.3.0 || <1.0.0 >2.0.0' res = abap_true )
      ( r0 = '<1.0.0 >2.0.0' r1 = '>0.0.0' res = abap_false )
      ( r0 = '>0.0.0' r1 = '<1.0.0 >2.0.0' res = abap_false )
      ( r0 = '<1.0.0 >2.0.0' r1 = '>1.4.0 <1.6.0' res = abap_false )
      ( r0 = '<1.0.0 >2.0.0' r1 = '>1.4.0 <1.6.0 || 2.0.0' res = abap_false )
      ( r0 = '>1.0.0 <=2.0.0' r1 = '2.0.0' res = abap_true )
      ( r0 = '<1.0.0 >=2.0.0' r1 = '2.1.0' res = abap_false )
      ( r0 = '<1.0.0 >=2.0.0' r1 = '>1.4.0 <1.6.0 || 2.0.0' res = abap_false )
      ( r0 = '1.5.x' r1 = '<1.5.0 || >=1.6.0' res = abap_false )
      ( r0 = '<1.5.0 || >=1.6.0' r1 = '1.5.x' res = abap_false )
      ( r0 = '<1.6.16 || >=1.7.0 <1.7.11 || >=1.8.0 <1.8.2'
        r1 = '>=1.6.16 <1.7.0 || >=1.7.11 <1.8.0 || >=1.8.2' res = abap_false )
      ( r0 = '<=1.6.16 || >=1.7.0 <1.7.11 || >=1.8.0 <1.8.2'
        r1 = '>=1.6.16 <1.7.0 || >=1.7.11 <1.8.0 || >=1.8.2' res = abap_true )
      ( r0 = '>=1.0.0' r1 = '<=1.0.0' res = abap_true )
      ( r0 = '>1.0.0 <1.0.0' r1 = '<=0.0.0' res = abap_false )
      ( r0 = '*' r1 = '0.0.1' res = abap_true )
      ( r0 = '*' r1 = '>=1.0.0' res = abap_true )
      ( r0 = '*' r1 = '>1.0.0' res = abap_true )
      ( r0 = '*' r1 = '~1.0.0' res = abap_true )
      ( r0 = '*' r1 = '<1.6.0' res = abap_true )
      ( r0 = '*' r1 = '<=1.6.0' res = abap_true )
      ( r0 = '1.*' r1 = '0.0.1' res = abap_false )
      ( r0 = '1.*' r1 = '2.0.0' res = abap_false )
      ( r0 = '1.*' r1 = '1.0.0' res = abap_true )
      ( r0 = '1.*' r1 = '<2.0.0' res = abap_true )
      ( r0 = '1.*' r1 = '>1.0.0' res = abap_true )
      ( r0 = '1.*' r1 = '<=1.0.0' res = abap_true )
      ( r0 = '1.*' r1 = '^1.0.0' res = abap_true )
      ( r0 = '1.0.*' r1 = '0.0.1' res = abap_false )
      ( r0 = '1.0.*' r1 = '<0.0.1' res = abap_false )
      ( r0 = '1.0.*' r1 = '>0.0.1' res = abap_true )
      ( r0 = '*' r1 = '1.3.0 || <1.0.0 >2.0.0' res = abap_true )
      ( r0 = '1.3.0 || <1.0.0 >2.0.0' r1 = '*' res = abap_true )
      ( r0 = '1.*' r1 = '1.3.0 || <1.0.0 >2.0.0' res = abap_true )
      ( r0 = 'x' r1 = '0.0.1' res = abap_true )
      ( r0 = 'x' r1 = '>=1.0.0' res = abap_true )
      ( r0 = 'x' r1 = '>1.0.0' res = abap_true )
      ( r0 = 'x' r1 = '~1.0.0' res = abap_true )
      ( r0 = 'x' r1 = '<1.6.0' res = abap_true )
      ( r0 = 'x' r1 = '<=1.6.0' res = abap_true )
      ( r0 = '1.x' r1 = '0.0.1' res = abap_false )
      ( r0 = '1.x' r1 = '2.0.0' res = abap_false )
      ( r0 = '1.x' r1 = '1.0.0' res = abap_true )
      ( r0 = '1.x' r1 = '<2.0.0' res = abap_true )
      ( r0 = '1.x' r1 = '>1.0.0' res = abap_true )
      ( r0 = '1.x' r1 = '<=1.0.0' res = abap_true )
      ( r0 = '1.x' r1 = '^1.0.0' res = abap_true )
      ( r0 = '1.0.x' r1 = '0.0.1' res = abap_false )
      ( r0 = '1.0.x' r1 = '<0.0.1' res = abap_false )
      ( r0 = '1.0.x' r1 = '>0.0.1' res = abap_true )
      ( r0 = 'x' r1 = '1.3.0 || <1.0.0 >2.0.0' res = abap_true )
      ( r0 = '1.3.0 || <1.0.0 >2.0.0' r1 = 'x' res = abap_true )
      ( r0 = '1.x' r1 = '1.3.0 || <1.0.0 >2.0.0' res = abap_true )
      ( r0 = '*' r1 = '*' res = abap_true )
      ( r0 = 'x' r1 = '' res = abap_true ) ).

  ENDMETHOD.


  METHOD range_parse.
    " [range, canonical result, options]

    " '' result means it's not a valid range
    " '*' is the return value from functions.validRange(), but
    " new Range().range will be '' in those cases
    result = VALUE #(
      ( range = '1.0.0 - 2.0.0' res = '>=1.0.0 <=2.0.0' )
      ( range = '1.0.0 - 2.0.0' res = '>=1.0.0-0 <2.0.1-0' incpre = abap_true )
      ( range = '1 - 2' res = '>=1.0.0 <3.0.0-0' )
      ( range = '1 - 2' res = '>=1.0.0-0 <3.0.0-0' incpre = abap_true )
      ( range = '1.0 - 2.0' res = '>=1.0.0 <2.1.0-0' )
      ( range = '1.0 - 2.0' res = '>=1.0.0-0 <2.1.0-0' incpre = abap_true )
      ( range = '1.0.0' res = '1.0.0' loose = abap_false )
      ( range = '>=*' res = '*' )
      ( range = '' res = '*' )
      ( range = '*' res = '*' )
      ( range = '>=1.0.0' res = '>=1.0.0' )
      ( range = '>1.0.0' res = '>1.0.0' )
      ( range = '<=2.0.0' res = '<=2.0.0' )
      ( range = '1' res = '>=1.0.0 <2.0.0-0' )
      ( range = '<2.0.0' res = '<2.0.0' )
      ( range = '>= 1.0.0' res = '>=1.0.0' )
      ( range = '>=  1.0.0' res = '>=1.0.0' )
      ( range = '>=   1.0.0' res = '>=1.0.0' )
      ( range = '> 1.0.0' res = '>1.0.0' )
      ( range = '>  1.0.0' res = '>1.0.0' )
      ( range = '<=   2.0.0' res = '<=2.0.0' )
      ( range = '<= 2.0.0' res = '<=2.0.0' )
      ( range = '<=  2.0.0' res = '<=2.0.0' )
      ( range = '<    2.0.0' res = '<2.0.0' )
      ( range = |<\t2.0.0| res = '<2.0.0' )
      ( range = '>=0.1.97' res = '>=0.1.97' )
      ( range = '0.1.20 || 1.2.4' res = '0.1.20||1.2.4' )
      ( range = '>=0.2.3 || <0.0.1' res = '>=0.2.3||<0.0.1' )
      ( range = '||' res = '*' )
      ( range = '2.x.x' res = '>=2.0.0 <3.0.0-0' )
      ( range = '1.2.x' res = '>=1.2.0 <1.3.0-0' )
      ( range = '1.2.x || 2.x' res = '>=1.2.0 <1.3.0-0||>=2.0.0 <3.0.0-0' )
      ( range = 'x' res = '*' )
      ( range = '2.*.*' res = '>=2.0.0 <3.0.0-0' )
      ( range = '1.2.*' res = '>=1.2.0 <1.3.0-0' )
      ( range = '1.2.* || 2.*' res = '>=1.2.0 <1.3.0-0||>=2.0.0 <3.0.0-0' )
      ( range = '2' res = '>=2.0.0 <3.0.0-0' )
      ( range = '2.3' res = '>=2.3.0 <2.4.0-0' )
      ( range = '~2.4' res = '>=2.4.0 <2.5.0-0' )
      ( range = '~>3.2.1' res = '>=3.2.1 <3.3.0-0' )
      ( range = '~1' res = '>=1.0.0 <2.0.0-0' )
      ( range = '~>1' res = '>=1.0.0 <2.0.0-0' )
      ( range = '~> 1' res = '>=1.0.0 <2.0.0-0' )
      ( range = '~1.0' res = '>=1.0.0 <1.1.0-0' )
      ( range = '~ 1.0' res = '>=1.0.0 <1.1.0-0' )
      ( range = '^0' res = '<1.0.0-0' )
      ( range = '^ 1' res = '>=1.0.0 <2.0.0-0' )
      ( range = '^0.1' res = '>=0.1.0 <0.2.0-0' )
      ( range = '^1.0' res = '>=1.0.0 <2.0.0-0' )
      ( range = '^1.2' res = '>=1.2.0 <2.0.0-0' )
      ( range = '^0.0.1' res = '>=0.0.1 <0.0.2-0' )
      ( range = '^0.0.1-beta' res = '>=0.0.1-beta <0.0.2-0' )
      ( range = '^0.1.2' res = '>=0.1.2 <0.2.0-0' )
      ( range = '^1.2.3' res = '>=1.2.3 <2.0.0-0' )
      ( range = '^1.2.3-beta.4' res = '>=1.2.3-beta.4 <2.0.0-0' )
      ( range = '<1' res = '<1.0.0-0' )
      ( range = '< 1' res = '<1.0.0-0' )
      ( range = '>=1' res = '>=1.0.0' )
      ( range = '>= 1' res = '>=1.0.0' )
      ( range = '<1.2' res = '<1.2.0-0' )
      ( range = '< 1.2' res = '<1.2.0-0' )
      ( range = '>01.02.03' res = '>1.2.3' loose = abap_true )
      ( range = '>01.02.03' res = '' )
      ( range = '~1.2.3beta' res = '>=1.2.3-beta <1.3.0-0' loose = abap_true )
      ( range = '~1.2.3beta' res = '' )
      ( range = '^ 1.2 ^ 1' res = '>=1.2.0 <2.0.0-0 >=1.0.0' )
      ( range = '1.2 - 3.4.5' res = '>=1.2.0 <=3.4.5' )
      ( range = '1.2.3 - 3.4' res = '>=1.2.3 <3.5.0-0' )
      ( range = '1.2 - 3.4' res = '>=1.2.0 <3.5.0-0' )
      ( range = '>1' res = '>=2.0.0' )
      ( range = '>1.2' res = '>=1.3.0' )
      ( range = '>X' res = '<0.0.0-0' )
      ( range = '<X' res = '<0.0.0-0' )
      ( range = '<x <* || >* 2.x' res = '<0.0.0-0' )
      ( range = '>x 2.x || * || <x' res = '*' )
      ( range = '>=09090' res = '' )
      ( range = '>=09090' res = '>=9090.0.0' loose = abap_true )
      ( range = '>=09090-0' res = '' incpre = abap_true )
      ( range = '>=09090-0' res = '' loose = abap_true incpre = abap_true )
      ( range = |^{ zif_abappm_semver_constants=>max_safe_integer }.0.0| res = '' )
      ( range = |={ zif_abappm_semver_constants=>max_safe_integer }.0.0|
          res = |{ zif_abappm_semver_constants=>max_safe_integer }.0.0| )
      ( range = |^{ zif_abappm_semver_constants=>max_safe_integer - 1 }.0.0|
          res = |>={ zif_abappm_semver_constants=>max_safe_integer - 1 }.0.0 | &&
                |<{ zif_abappm_semver_constants=>max_safe_integer }.0.0-0| ) ).

  ENDMETHOD.


  METHOD version_gt_range.
    " [range, version, options]

    " Version should be greater than range
    result = VALUE #(
      ( range = '~1.2.2' version = '1.3.0' )
      ( range = '~0.6.1-1' version = '0.7.1-1' )
      ( range = '1.0.0 - 2.0.0' version = '2.0.1' )
      ( range = '1.0.0' version = '1.0.1-beta1' )
      ( range = '1.0.0' version = '2.0.0' )
      ( range = '<=2.0.0' version = '2.1.1' )
      ( range = '<=2.0.0' version = '3.2.9' )
      ( range = '<2.0.0' version = '2.0.0' )
      ( range = '0.1.20 || 1.2.4' version = '1.2.5' )
      ( range = '2.x.x' version = '3.0.0' )
      ( range = '1.2.x' version = '1.3.0' )
      ( range = '1.2.x || 2.x' version = '3.0.0' )
      ( range = '2.*.*' version = '5.0.1' )
      ( range = '1.2.*' version = '1.3.3' )
      ( range = '1.2.* || 2.*' version = '4.0.0' )
      ( range = '2' version = '3.0.0' )
      ( range = '2.3' version = '2.4.2' )
      ( range = '~2.4' version = '2.5.0' ) " >=2.4.0 <2.5.0
      ( range = '~2.4' version = '2.5.5' )
      ( range = '~>3.2.1' version = '3.3.0' ) " >=3.2.1 <3.3.0
      ( range = '~1' version = '2.2.3' ) " >=1.0.0 <2.0.0
      ( range = '~>1' version = '2.2.4' )
      ( range = '~> 1' version = '3.2.3' )
      ( range = '~1.0' version = '1.1.2' ) " >=1.0.0 <1.1.0
      ( range = '~ 1.0' version = '1.1.0' )
      ( range = '<1.2' version = '1.2.0' )
      ( range = '< 1.2' version = '1.2.1' )
      ( range = '1' version = '2.0.0beta' loose = abap_true )
      ( range = '~v0.5.4-pre' version = '0.6.0' )
      ( range = '~v0.5.4-pre' version = '0.6.1-pre' )
      ( range = '=0.7.x' version = '0.8.0' )
      ( range = '=0.7.x' version = '0.8.0-asdf' )
      ( range = '<0.7.x' version = '0.7.0' )
      ( range = '1.0.0 - 2.0.0' version = '2.2.3' )
      ( range = '1.0.0' version = '1.0.1' )
      ( range = '<=2.0.0' version = '3.0.0' )
      ( range = '<=2.0.0' version = '2.9999.9999' )
      ( range = '<=2.0.0' version = '2.2.9' )
      ( range = '<2.0.0' version = '2.9999.9999' )
      ( range = '<2.0.0' version = '2.2.9' )
      ( range = '2.x.x' version = '3.1.3' )
      ( range = '1.2.x' version = '1.3.3' )
      ( range = '1.2.x || 2.x' version = '3.1.3' )
      ( range = '2.*.*' version = '3.1.3' )
      ( range = '1.2.* || 2.*' version = '3.1.3' )
      ( range = '2' version = '3.1.2' )
      ( range = '2.3' version = '2.4.1' )
      ( range = '~>3.2.1' version = '3.3.2' ) " >=3.2.1 <3.3.0
      ( range = '~>1' version = '2.2.3' )
      ( range = '~1.0' version = '1.1.0' ) " >=1.0.0 <1.1.0
      ( range = '<1' version = '1.0.0' )
      ( range = '<1' version = '1.0.0beta' loose = abap_true )
      ( range = '< 1' version = '1.0.0beta' loose = abap_true )
      ( range = '=0.7.x' version = '0.8.2' )
      ( range = '<0.7.x' version = '0.7.2' )
      ( range = '0.7.x' version = '0.7.2-beta' ) ).

  ENDMETHOD.


  METHOD version_lt_range.
    " [range, version, options]

    " Version should be less than range
    result = VALUE #(
      ( range = '~1.2.2' version = '1.2.1' )
      ( range = '~0.6.1-1' version = '0.6.1-0' )
      ( range = '1.0.0 - 2.0.0' version = '0.0.1' )
      ( range = '1.0.0-beta.2' version = '1.0.0-beta.1' )
      ( range = '1.0.0' version = '0.0.0' )
      ( range = '>=2.0.0' version = '1.1.1' )
      ( range = '>=2.0.0' version = '1.2.9' )
      ( range = '>2.0.0' version = '2.0.0' )
      ( range = '0.1.20 || 1.2.4' version = '0.1.5' )
      ( range = '2.x.x' version = '1.0.0' )
      ( range = '1.2.x' version = '1.1.0' )
      ( range = '1.2.x || 2.x' version = '1.0.0' )
      ( range = '2.*.*' version = '1.0.1' )
      ( range = '1.2.*' version = '1.1.3' )
      ( range = '1.2.* || 2.*' version = '1.1.9999' )
      ( range = '2' version = '1.0.0' )
      ( range = '2.3' version = '2.2.2' )
      ( range = '~2.4' version = '2.3.0' ) " >=2.4.0 <2.5.0
      ( range = '~2.4' version = '2.3.5' )
      ( range = '~>3.2.1' version = '3.2.0' ) " >=3.2.1 <3.3.0
      ( range = '~1' version = '0.2.3' ) " >=1.0.0 <2.0.0
      ( range = '~>1' version = '0.2.4' )
      ( range = '~> 1' version = '0.2.3' )
      ( range = '~1.0' version = '0.1.2' ) " >=1.0.0 <1.1.0
      ( range = '~ 1.0' version = '0.1.0' )
      ( range = '>1.2' version = '1.2.0' )
      ( range = '> 1.2' version = '1.2.1' )
      ( range = '1' version = '0.0.0beta' loose = abap_true )
      ( range = '~v0.5.4-pre' version = '0.5.4-alpha' )
      ( range = '=0.7.x' version = '0.6.0' )
      ( range = '=0.7.x' version = '0.6.0-asdf' )
      ( range = '>=0.7.x' version = '0.6.0' )
      ( range = '1.0.0 - 2.0.0' version = '0.2.3' )
      ( range = '1.0.0' version = '0.0.1' )
      ( range = '>=2.0.0' version = '1.0.0' )
      ( range = '>=2.0.0' version = '1.9999.9999' )
      ( range = '>2.0.0' version = '1.2.9' )
      ( range = '2.x.x' version = '1.1.3' )
      ( range = '1.2.x' version = '1.1.3' )
      ( range = '1.2.x || 2.x' version = '1.1.3' )
      ( range = '2.*.*' version = '1.1.3' )
      ( range = '1.2.* || 2.*' version = '1.1.3' )
      ( range = '2' version = '1.9999.9999' )
      ( range = '2.3' version = '2.2.1' )
      ( range = '~>3.2.1' version = '2.3.2' ) " >=3.2.1 <3.3.0
      ( range = '~>1' version = '0.2.3' )
      ( range = '~1.0' version = '0.0.0' ) " >=1.0.0 <1.1.0
      ( range = '>1' version = '1.0.0' )
      ( range = '2' version = '1.0.0beta' loose = abap_true )
      ( range = '>1' version = '1.0.0beta' loose = abap_true )
      ( range = '> 1' version = '1.0.0beta' loose = abap_true )
      ( range = '=0.7.x' version = '0.6.2' )
      ( range = '=0.7.x' version = '0.7.0-asdf' )
      ( range = '^1' version = '1.0.0-0' )
      ( range = '>=0.7.x' version = '0.7.0-asdf' )
      ( range = '1' version = '1.0.0beta' loose = abap_true )
      ( range = '>=0.7.x' version = '0.6.2' )
      ( range = '>1.2.3' version = '1.3.0-alpha' ) ).

  ENDMETHOD.


  METHOD version_not_gt_range.
    " [range, version, options]

    " Version should NOT be greater than range
    result = VALUE #(
      ( range = '~0.6.1-1' version = '0.6.1-1' )
      ( range = '1.0.0 - 2.0.0' version = '1.2.3' )
      ( range = '1.0.0 - 2.0.0' version = '0.9.9' )
      ( range = '1.0.0' version = '1.0.0' )
      ( range = '>=*' version = '0.2.4' )
      ( range = '' version = '1.0.0' loose = abap_true )
      ( range = '*' version = '1.2.3' )
      ( range = '*' version = 'v1.2.3-foo' )
      ( range = '>=1.0.0' version = '1.0.0' )
      ( range = '>=1.0.0' version = '1.0.1' )
      ( range = '>=1.0.0' version = '1.1.0' )
      ( range = '>1.0.0' version = '1.0.1' )
      ( range = '>1.0.0' version = '1.1.0' )
      ( range = '<=2.0.0' version = '2.0.0' )
      ( range = '<=2.0.0' version = '1.9999.9999' )
      ( range = '<=2.0.0' version = '0.2.9' )
      ( range = '<2.0.0' version = '1.9999.9999' )
      ( range = '<2.0.0' version = '0.2.9' )
      ( range = '>= 1.0.0' version = '1.0.0' )
      ( range = '>=  1.0.0' version = '1.0.1' )
      ( range = '>=   1.0.0' version = '1.1.0' )
      ( range = '> 1.0.0' version = '1.0.1' )
      ( range = '>  1.0.0' version = '1.1.0' )
      ( range = '<=   2.0.0' version = '2.0.0' )
      ( range = '<= 2.0.0' version = '1.9999.9999' )
      ( range = '<=  2.0.0' version = '0.2.9' )
      ( range = '<    2.0.0' version = '1.9999.9999' )
      ( range = |<\t2.0.0| version = '0.2.9' )
      ( range = '>=0.1.97' version = 'v0.1.97' )
      ( range = '>=0.1.97' version = '0.1.97' )
      ( range = '0.1.20 || 1.2.4' version = '1.2.4' )
      ( range = '0.1.20 || >1.2.4' version = '1.2.4' )
      ( range = '0.1.20 || 1.2.4' version = '1.2.3' )
      ( range = '0.1.20 || 1.2.4' version = '0.1.20' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.0.0' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.2.3' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.2.4' )
      ( range = '||' version = '1.3.4' )
      ( range = '2.x.x' version = '2.1.3' )
      ( range = '1.2.x' version = '1.2.3' )
      ( range = '1.2.x || 2.x' version = '2.1.3' )
      ( range = '1.2.x || 2.x' version = '1.2.3' )
      ( range = 'x' version = '1.2.3' )
      ( range = '2.*.*' version = '2.1.3' )
      ( range = '1.2.*' version = '1.2.3' )
      ( range = '1.2.* || 2.*' version = '2.1.3' )
      ( range = '1.2.* || 2.*' version = '1.2.3' )
      ( range = '2' version = '2.1.2' )
      ( range = '2.3' version = '2.3.1' )
      ( range = '~2.4' version = '2.4.0' ) " >=2.4.0 <2.5.0
      ( range = '~2.4' version = '2.4.5' )
      ( range = '~>3.2.1' version = '3.2.2' ) " >=3.2.1 <3.3.0
      ( range = '~1' version = '1.2.3' ) " >=1.0.0 <2.0.0
      ( range = '~>1' version = '1.2.3' )
      ( range = '~> 1' version = '1.2.3' )
      ( range = '~1.0' version = '1.0.2' ) " >=1.0.0 <1.1.0
      ( range = '~ 1.0' version = '1.0.2' )
      ( range = '>=1' version = '1.0.0' )
      ( range = '>= 1' version = '1.0.0' )
      ( range = '<1.2' version = '1.1.1' )
      ( range = '< 1.2' version = '1.1.1' )
      ( range = '1' version = '1.0.0beta' loose = abap_true )
      ( range = '~v0.5.4-pre' version = '0.5.5' )
      ( range = '~v0.5.4-pre' version = '0.5.4' )
      ( range = '=0.7.x' version = '0.7.2' )
      ( range = '>=0.7.x' version = '0.7.2' )
      ( range = '=0.7.x' version = '0.7.0-asdf' )
      ( range = '>=0.7.x' version = '0.7.0-asdf' )
      ( range = '<=0.7.x' version = '0.6.2' )
      ( range = '>0.2.3 >0.2.4 <=0.2.5' version = '0.2.5' )
      ( range = '>=0.2.3 <=0.2.4' version = '0.2.4' )
      ( range = '1.0.0 - 2.0.0' version = '2.0.0' )
      ( range = '^1' version = '0.0.0-0' )
      ( range = '^3.0.0' version = '2.0.0' )
      ( range = '^1.0.0 || ~2.0.1' version = '2.0.0' )
      ( range = '^0.1.0 || ~3.0.1 || 5.0.0' version = '3.2.0' )
      ( range = '^0.1.0 || ~3.0.1 || 5.0.0' version = '1.0.0beta' loose = abap_true )
      ( range = '^0.1.0 || ~3.0.1 || 5.0.0' version = '5.0.0-0' loose = abap_true )
      ( range = '^0.1.0 || ~3.0.1 || >4 <=5.0.0' version = '3.5.0' )
      ( range = '0.7.x' version = '0.7.2-beta' incpre = abap_true ) ).

  ENDMETHOD.


  METHOD version_not_lt_range.
    " [range, version, options]

    " Version should NOT be less than range
    result = VALUE #(
      ( range = '~ 1.0' version = '1.1.0' )
      ( range = '~0.6.1-1' version = '0.6.1-1' )
      ( range = '1.0.0 - 2.0.0' version = '1.2.3' )
      ( range = '1.0.0 - 2.0.0' version = '2.9.9' )
      ( range = '1.0.0' version = '1.0.0' )
      ( range = '>=*' version = '0.2.4' )
      ( range = '' version = '1.0.0' loose = abap_true )
      ( range = '*' version = '1.2.3' )
      ( range = '>=1.0.0' version = '1.0.0' )
      ( range = '>=1.0.0' version = '1.0.1' )
      ( range = '>=1.0.0' version = '1.1.0' )
      ( range = '>1.0.0' version = '1.0.1' )
      ( range = '>1.0.0' version = '1.1.0' )
      ( range = '<=2.0.0' version = '2.0.0' )
      ( range = '<=2.0.0' version = '1.9999.9999' )
      ( range = '<=2.0.0' version = '0.2.9' )
      ( range = '<2.0.0' version = '1.9999.9999' )
      ( range = '<2.0.0' version = '0.2.9' )
      ( range = '>= 1.0.0' version = '1.0.0' )
      ( range = '>=  1.0.0' version = '1.0.1' )
      ( range = '>=   1.0.0' version = '1.1.0' )
      ( range = '> 1.0.0' version = '1.0.1' )
      ( range = '>  1.0.0' version = '1.1.0' )
      ( range = '<=   2.0.0' version = '2.0.0' )
      ( range = '<= 2.0.0' version = '1.9999.9999' )
      ( range = '<=  2.0.0' version = '0.2.9' )
      ( range = '<    2.0.0' version = '1.9999.9999' )
      ( range = |<\t2.0.0| version = '0.2.9' )
      ( range = '>=0.1.97' version = 'v0.1.97' )
      ( range = '>=0.1.97' version = '0.1.97' )
      ( range = '0.1.20 || 1.2.4' version = '1.2.4' )
      ( range = '0.1.20 || >1.2.4' version = '1.2.4' )
      ( range = '0.1.20 || 1.2.4' version = '1.2.3' )
      ( range = '0.1.20 || 1.2.4' version = '0.1.20' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.0.0' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.2.3' )
      ( range = '>=0.2.3 || <0.0.1' version = '0.2.4' )
      ( range = '||' version = '1.3.4' )
      ( range = '2.x.x' version = '2.1.3' )
      ( range = '1.2.x' version = '1.2.3' )
      ( range = '1.2.x || 2.x' version = '2.1.3' )
      ( range = '1.2.x || 2.x' version = '1.2.3' )
      ( range = 'x' version = '1.2.3' )
      ( range = '2.*.*' version = '2.1.3' )
      ( range = '1.2.*' version = '1.2.3' )
      ( range = '1.2.* || 2.*' version = '2.1.3' )
      ( range = '1.2.* || 2.*' version = '1.2.3' )
      ( range = '2' version = '2.1.2' )
      ( range = '2.3' version = '2.3.1' )
      ( range = '~2.4' version = '2.4.0' ) " >=2.4.0 <2.5.0
      ( range = '~2.4' version = '2.4.5' )
      ( range = '~>3.2.1' version = '3.2.2' ) " >=3.2.1 <3.3.0
      ( range = '~1' version = '1.2.3' ) " >=1.0.0 <2.0.0
      ( range = '~>1' version = '1.2.3' )
      ( range = '~> 1' version = '1.2.3' )
      ( range = '~1.0' version = '1.0.2' ) " >=1.0.0 <1.1.0
      ( range = '~ 1.0' version = '1.0.2' )
      ( range = '>=1' version = '1.0.0' )
      ( range = '>= 1' version = '1.0.0' )
      ( range = '<1.2' version = '1.1.1' )
      ( range = '< 1.2' version = '1.1.1' )
      ( range = '~v0.5.4-pre' version = '0.5.5' )
      ( range = '~v0.5.4-pre' version = '0.5.4' )
      ( range = '=0.7.x' version = '0.7.2' )
      ( range = '>=0.7.x' version = '0.7.2' )
      ( range = '<=0.7.x' version = '0.6.2' )
      ( range = '>0.2.3 >0.2.4 <=0.2.5' version = '0.2.5' )
      ( range = '>=0.2.3 <=0.2.4' version = '0.2.4' )
      ( range = '1.0.0 - 2.0.0' version = '2.0.0' )
      ( range = '^3.0.0' version = '4.0.0' )
      ( range = '^1.0.0 || ~2.0.1' version = '2.0.0' )
      ( range = '^0.1.0 || ~3.0.1 || 5.0.0' version = '3.2.0' )
      ( range = '^0.1.0 || ~3.0.1 || 5.0.0' version = '1.0.0beta' loose = abap_true )
      ( range = '^0.1.0 || ~3.0.1 || 5.0.0' version = '5.0.0-0' loose = abap_true )
      ( range = '^0.1.0 || ~3.0.1 || >4 <=5.0.0' version = '3.5.0' )
      ( range = '^1.0.0alpha' version = '1.0.0beta' loose = abap_true )
      ( range = '~1.0.0alpha' version = '1.0.0beta' loose = abap_true )
      ( range = '^1.0.0-alpha' version = '1.0.0beta' loose = abap_true )
      ( range = '~1.0.0-alpha' version = '1.0.0beta' loose = abap_true )
      ( range = '^1.0.0-alpha' version = '1.0.0-beta' )
      ( range = '~1.0.0-alpha' version = '1.0.0-beta' )
      ( range = '=0.1.0' version = '1.0.0' )
      ( range = '>1.2.3' version = '1.3.0-alpha' incpre = abap_true ) ).

  ENDMETHOD.
ENDCLASS.
