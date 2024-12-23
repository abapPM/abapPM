CLASS ZCL_ABAPPM_SEMVER_UTILS DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Utilities
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS is_numeric
      IMPORTING
        !data         TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS trim
      IMPORTING
        !data         TYPE clike
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS version_trim
      IMPORTING
        !data         TYPE clike
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPPM_SEMVER_UTILS IMPLEMENTATION.


  METHOD is_numeric.
    " Unsigned number (could be bigger than int4 or even int8)

    TRY.
        result = xsdbool( |{ data }| CO '0123456789' ).
      CATCH cx_root.
        " can't be converted to string/numeric
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD trim.
    " Remove leading and trailing tab, cr, lf and spaces Like JavaScript trim
    result = condense(
      val = replace(
        val   = data
        regex = `[\t\n\r]`
        with  = ` `
        occ   = 0 )
      del = ` ` ).
  ENDMETHOD.


  METHOD version_trim.
    " POSIX: Remove whitespace after "v" or "=" to avoid issue with greedy regex
    result = replace(
      val   = trim( data )
      regex = ZCL_ABAPPM_SEMVER_RE=>TOKEN-VTRIM-SRC
      with  = ZCL_ABAPPM_SEMVER_RE=>VERSION_TRIM_REPLACE
      occ   = ZCL_ABAPPM_SEMVER_RE=>TOKEN-VTRIM-OCC ).
  ENDMETHOD.
ENDCLASS.
