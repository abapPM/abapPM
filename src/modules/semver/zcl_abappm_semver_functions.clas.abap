CLASS ZCL_ABAPPM_SEMVER_FUNCTIONS DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Functions
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS clean
      IMPORTING
        version       TYPE string
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS cmp
      IMPORTING
        a             TYPE any
        op            TYPE string
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS coerce
      IMPORTING
        version       TYPE string
        rtl           TYPE abap_bool DEFAULT abap_false
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZCL_ABAPPM_SEMVER
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS compare
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS compare_build
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS compare_loose
      IMPORTING
        a             TYPE any
        b             TYPE any
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS diff
      IMPORTING
        version_1     TYPE any
        version_2     TYPE any
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS eq
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS gt
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS gte
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS inc
      IMPORTING
        version         TYPE any
        release         TYPE string
        identifier      TYPE string OPTIONAL
        identifier_base TYPE string OPTIONAL
        loose           TYPE abap_bool DEFAULT abap_false
        incpre          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE REF TO ZCL_ABAPPM_SEMVER.

    CLASS-METHODS lt
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS lte
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS major
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS minor
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS neq
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS parse
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
        throw_errors  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZCL_ABAPPM_SEMVER
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS patch
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS prerelease
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS rcompare
      IMPORTING
        a             TYPE any
        b             TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE i
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS rsort
      IMPORTING
        list          TYPE string_table
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS sort
      IMPORTING
        list          TYPE string_table
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS satisfies
      IMPORTING
        version       TYPE any
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS valid
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS equality
      IMPORTING
        a             TYPE any
        b             TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

ENDCLASS.



CLASS ZCL_ABAPPM_SEMVER_FUNCTIONS IMPLEMENTATION.


  METHOD clean.

    DATA(vers) = replace(
      val   = ZCL_ABAPPM_SEMVER_UTILS=>VERSION_TRIM( version )
      regex = '^[=v]+'
      with  = '' ).

    DATA(semver) = parse( version = vers loose = loose incpre = incpre ).

    CHECK semver IS BOUND.

    result = semver->version.

  ENDMETHOD.


  METHOD cmp.

    CASE op.
      WHEN '==='.
        result = equality( a = a b = b ).
      WHEN '!=='.
        result = xsdbool( NOT equality( a = a b = b ) ).
      WHEN '' OR '=' OR '=='.
        result = eq( a = a b = b loose = loose incpre = incpre ).
      WHEN '!=' OR '<>'.
        result = neq( a = a b = b loose = loose incpre = incpre ).
      WHEN '>'.
        result = gt( a = a b = b loose = loose incpre = incpre ).
      WHEN '>='.
        result = gte( a = a b = b loose = loose incpre = incpre ).
      WHEN '<'.
        result = lt( a = a b = b loose = loose incpre = incpre ).
      WHEN '<='.
        result = lte( a = a b = b loose = loose incpre = incpre ).
      WHEN OTHERS.
        ZCX_ABAPPM_SEMVER_ERROR=>RAISE( |Invalid operator: { op }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD coerce.

    TYPES:
      BEGIN OF ty_match,
        major      TYPE string,
        minor      TYPE string,
        patch      TYPE string,
        prerelease TYPE string,
        build      TYPE string,
        offset     TYPE i,
        length     TYPE i,
        endpos     TYPE i,
      END OF ty_match.

    DATA matches TYPE STANDARD TABLE OF ty_match.

    " cl_abap_matcher has a problem with '1.2.3.4.5.6' so we use FIND REGEX

    IF rtl = abap_false.
      DATA(r) = COND #(
        WHEN incpre = abap_true
        THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-COERCEFULL-SAFE_SRC
        ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-COERCE-SAFE_SRC ).

      FIND REGEX r IN version SUBMATCHES DATA(rest) DATA(major) DATA(minor) DATA(patch) DATA(prerelease) DATA(build).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ELSE.
      " Find the right-most coercible string that does not share
      " a terminus with a more left-ward coercible string.
      " Eg, '1.2.3.4' wants to coerce '2.3.4', not '3.4' or '4'
      r = COND #(
        WHEN incpre = abap_true
        THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-COERCERTLFULL-SAFE_SRC
        ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-COERCERTL-SAFE_SRC ).

      DATA(offset) = 0.
      DO.
        FIND REGEX r IN version+offset(*) SUBMATCHES rest major minor patch prerelease build.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        INSERT INITIAL LINE INTO TABLE matches ASSIGNING FIELD-SYMBOL(<match>).
        <match>-major      = major.
        <match>-minor      = minor.
        <match>-patch      = patch.
        <match>-prerelease = prerelease.
        <match>-build      = build.

        DATA(match) = |{ major }|
             && |{ COND #( WHEN minor IS NOT INITIAL THEN '.' && minor ) }|
             && |{ COND #( WHEN patch IS NOT INITIAL THEN '.' && patch ) }|
             && |{ COND #( WHEN prerelease IS NOT INITIAL THEN '-' && prerelease ) }|
             && |{ COND #( WHEN build IS NOT INITIAL THEN '+' && build ) }|.

        <match>-offset = offset.
        <match>-length = strlen( match ).
        <match>-endpos = <match>-offset + <match>-length.
        FIND REGEX '^\d' IN version+offset(*) MATCH OFFSET DATA(next_offset).
        offset += next_offset + 1.
        IF offset >= strlen( version ).
          EXIT.
        ENDIF.
      ENDDO.
      SORT matches BY endpos DESCENDING length DESCENDING.
      READ TABLE matches ASSIGNING <match> INDEX 1.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      major      = <match>-major.
      minor      = <match>-minor.
      patch      = <match>-patch.
      prerelease = <match>-prerelease.
      build      = <match>-build.
    ENDIF.

    IF minor IS INITIAL.
      minor = '0'.
    ENDIF.

    IF patch IS INITIAL.
      patch = '0'.
    ENDIF.

    IF incpre = abap_true AND prerelease IS NOT INITIAL.
      prerelease = |-{ prerelease }|.
    ELSE.
      prerelease = ''.
    ENDIF.

    IF incpre = abap_true AND build IS NOT INITIAL.
      build = |+{ build }|.
    ELSE.
      build = ''.
    ENDIF.

    result = parse( version = |{ major }.{ minor }.{ patch }{ prerelease }{ build }| loose = loose incpre = incpre ).

  ENDMETHOD.


  METHOD compare.

    DATA(semver_a) = ZCL_ABAPPM_SEMVER=>CREATE( version = a loose = loose incpre = incpre ).
    DATA(semver_b) = ZCL_ABAPPM_SEMVER=>CREATE( version = b loose = loose incpre = incpre ).

    CHECK semver_a IS BOUND AND semver_b IS BOUND.

    result = semver_a->compare( semver_b ).

  ENDMETHOD.


  METHOD compare_build.

    DATA(semver_a) = ZCL_ABAPPM_SEMVER=>CREATE( version = a loose = loose incpre = incpre ).
    DATA(semver_b) = ZCL_ABAPPM_SEMVER=>CREATE( version = b loose = loose incpre = incpre ).

    CHECK semver_a IS BOUND AND semver_b IS BOUND.

    result = semver_a->compare( semver_b ).
    IF result = 0.
      result = semver_a->compare_build( semver_b ).
    ENDIF.

  ENDMETHOD.


  METHOD compare_loose.
    result = compare( a = a b = b loose = abap_true incpre = incpre ).
  ENDMETHOD.


  METHOD diff.

    DATA(v1) = parse( version = version_1 throw_errors = abap_true ).
    DATA(v2) = parse( version = version_2 throw_errors = abap_true ).

    DATA(comparison) = v1->compare( v2 ).

    IF comparison = 0.
      RETURN.
    ENDIF.

    DATA(v1_higher)    = xsdbool( comparison > 0 ).
    DATA(high_version) = COND #( WHEN v1_higher = abap_true THEN v1 ELSE v2 ).
    DATA(low_version)  = COND #( WHEN v1_higher = abap_true THEN v2 ELSE v1 ).
    DATA(high_has_pre) = xsdbool( high_version->prerelease IS NOT INITIAL ).
    DATA(low_has_pre)  = xsdbool( low_version->prerelease IS NOT INITIAL ).

    IF low_has_pre = abap_true AND high_has_pre = abap_false.
      " Going from prerelease -> no prerelease requires some special casing

      " If the low version has only a major, then it will always be a major
      " Some examples:
      " 1.0.0-1 -> 1.0.0
      " 1.0.0-1 -> 1.1.1
      " 1.0.0-1 -> 2.0.0
      IF low_version->patch IS INITIAL AND low_version->minor IS INITIAL.
        result = 'major'.
        RETURN.
      ENDIF.

      " Otherwise it can be determined by checking the high version

      IF high_version->patch IS NOT INITIAL.
        " anything higher than a patch bump would result in the wrong version
        result = 'patch'.
        RETURN.
      ENDIF.

      IF high_version->minor IS NOT INITIAL.
        " anything higher than a minor bump would result in the wrong version
        result = 'minor'.
        RETURN.
      ENDIF.

      " bumping major/minor/patch all have same result
      result = 'major'.
      RETURN.
    ENDIF.

    " add the `pre` prefix if we are going to a prerelease version
    DATA(prefix) = COND #( WHEN high_has_pre = abap_true THEN 'pre' ELSE '' ).

    IF v1->major <> v2->major.
      result = prefix && 'major'.
      RETURN.
    ENDIF.

    IF v1->minor <> v2->minor.
      result = prefix && 'minor'.
      RETURN.
    ENDIF.

    IF v1->patch <> v2->patch.
      result = prefix && 'patch'.
      RETURN.
    ENDIF.

    " high and low are preleases
    result = 'prerelease'.

  ENDMETHOD.


  METHOD eq.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) = 0 ).
  ENDMETHOD.


  METHOD equality.

    DATA semver_a TYPE REF TO ZCL_ABAPPM_SEMVER.
    DATA semver_b TYPE REF TO ZCL_ABAPPM_SEMVER.

    IF a IS BOUND AND a IS INSTANCE OF ZCL_ABAPPM_SEMVER AND b IS BOUND AND b IS INSTANCE OF ZCL_ABAPPM_SEMVER.
      semver_a ?= a.
      semver_b ?= b.
      result = xsdbool( semver_a->version = semver_b->version ).
    ELSE.
      ZCX_ABAPPM_SEMVER_ERROR=>RAISE( |Invalid parameter type| ).
    ENDIF.

  ENDMETHOD.


  METHOD gt.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) > 0 ).
  ENDMETHOD.


  METHOD gte.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) >= 0 ).
  ENDMETHOD.


  METHOD inc.

    DATA semver TYPE REF TO ZCL_ABAPPM_SEMVER.

    TRY.
        " Create new semver object
        DATA(kind) = cl_abap_typedescr=>describe_by_data( version )->type_kind.

        IF kind = cl_abap_typedescr=>typekind_oref AND version IS INSTANCE OF ZCL_ABAPPM_SEMVER.
          semver ?= version.
          result = ZCL_ABAPPM_SEMVER=>CREATE( version = semver->version loose = loose incpre = incpre ).
        ELSE.
          result = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = loose incpre = incpre ).
        ENDIF.

        CHECK result IS BOUND.

        result->inc( release = release identifier = identifier identifier_base = identifier_base ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.


  METHOD lt.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) < 0 ).
  ENDMETHOD.


  METHOD lte.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) <= 0 ).
  ENDMETHOD.


  METHOD major.

    DATA(semver) = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = loose ).

    CHECK semver IS BOUND.

    result = semver->major.

  ENDMETHOD.


  METHOD minor.

    DATA(semver) = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = loose ).

    CHECK semver IS BOUND.

    result = semver->minor.

  ENDMETHOD.


  METHOD neq.
    result = xsdbool( compare( a = a b = b loose = loose incpre = incpre ) <> 0 ).
  ENDMETHOD.


  METHOD parse.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( version )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND version IS INSTANCE OF ZCL_ABAPPM_SEMVER.
      result ?= version.
      RETURN.
    ENDIF.

    TRY.
        result = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = loose incpre = incpre ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR INTO DATA(error).
        IF throw_errors = abap_false.
          RETURN.
        ENDIF.

        RAISE EXCEPTION error.
    ENDTRY.

  ENDMETHOD.


  METHOD patch.

    DATA(semver) = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = loose ).

    CHECK semver IS BOUND.

    result = semver->patch.

  ENDMETHOD.


  METHOD prerelease.

    DATA(semver) = parse( version = version loose = loose incpre = incpre ).

    CHECK semver IS BOUND.

    result = semver->prerelease.

  ENDMETHOD.


  METHOD rcompare.
    result = compare( a = b b = a loose = loose incpre = incpre ).
  ENDMETHOD.


  METHOD rsort.

    result = list.

    DATA(i) = 1.
    WHILE i < lines( result ).
      DATA(j) = 1.
      WHILE j <= lines( result ) - i.
        IF compare_build( b = result[ j ] a = result[ j + 1 ] loose = loose incpre = incpre ) > 0.
          DATA(temp)      = result[ j ].
          result[ j ]     = result[ j + 1 ].
          result[ j + 1 ] = temp.
        ENDIF.
        j += 1.
      ENDWHILE.
      i += 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD satisfies.

    TRY.
        DATA(semrange) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = range loose = loose incpre = incpre ).

        IF semrange IS BOUND.
          result = semrange->test( version ).
        ENDIF.
      CATCH ZCX_ABAPPM_SEMVER_ERROR.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD sort.

    result = list.

    DATA(i) = 1.
    WHILE i < lines( result ).
      DATA(j) = 1.
      WHILE j <= lines( result ) - i.
        IF compare_build( a = result[ j ] b = result[ j + 1 ] loose = loose incpre = incpre ) > 0.
          DATA(temp)      = result[ j ].
          result[ j ]     = result[ j + 1 ].
          result[ j + 1 ] = temp.
        ENDIF.
        j += 1.
      ENDWHILE.
      i += 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD valid.

    TRY.
        DATA(semver) = parse( version = version loose = loose incpre = incpre ).

        CHECK semver IS BOUND.

        result = semver->version.
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
