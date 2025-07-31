CLASS /apmg/cl_apm_semver_utils DEFINITION
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



CLASS /apmg/cl_apm_semver_utils IMPLEMENTATION.


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
      regex = /apmg/cl_apm_semver_re=>token-vtrim-src
      with  = /apmg/cl_apm_semver_re=>version_trim_replace
      occ   = /apmg/cl_apm_semver_re=>token-vtrim-occ ).
  ENDMETHOD.
ENDCLASS.
