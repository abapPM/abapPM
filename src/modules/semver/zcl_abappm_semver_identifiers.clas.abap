CLASS ZCL_ABAPPM_SEMVER_IDENTIFIERS DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Identifiers
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS compare_identifiers
      IMPORTING
        !a            TYPE any
        !b            TYPE any
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS rcompare_identifiers
      IMPORTING
        !a            TYPE any
        !b            TYPE any
      RETURNING
        VALUE(result) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPPM_SEMVER_IDENTIFIERS IMPLEMENTATION.


  METHOD compare_identifiers.

    DATA(anum) = ZCL_ABAPPM_SEMVER_UTILS=>IS_NUMERIC( a ).
    DATA(bnum) = ZCL_ABAPPM_SEMVER_UTILS=>IS_NUMERIC( b ).

    IF anum = abap_true AND bnum = abap_true.
      DATA(aval) = CONV decfloat34( a ).
      DATA(bval) = CONV decfloat34( b ).
      IF aval = bval.
        result = 0.
      ELSEIF aval < bval.
        result = -1.
      ELSE.
        result = +1.
      ENDIF.
    ELSE.
      IF a = b.
        result = 0.
      ELSEIF anum = abap_true AND bnum = abap_false.
        result = -1.
      ELSEIF anum = abap_false AND bnum = abap_true.
        result = +1.
      ELSEIF a < b.
        result = -1.
      ELSE.
        result = +1.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD rcompare_identifiers.
    result = compare_identifiers( a = b b = a ).
  ENDMETHOD.
ENDCLASS.
