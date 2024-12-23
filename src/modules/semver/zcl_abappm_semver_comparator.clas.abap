CLASS ZCL_ABAPPM_SEMVER_COMPARATOR DEFINITION
  PUBLIC
  CREATE PRIVATE.

************************************************************************
* SemVer Comparator
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-DATA any_semver TYPE REF TO ZCL_ABAPPM_SEMVER.

    DATA:
      operator TYPE string READ-ONLY,
      value    TYPE string READ-ONLY,
      semver   TYPE REF TO ZCL_ABAPPM_SEMVER READ-ONLY.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING
        comp   TYPE string
        loose  TYPE abap_bool DEFAULT abap_false
        incpre TYPE abap_bool DEFAULT abap_false
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS create
      IMPORTING
        comp          TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZCL_ABAPPM_SEMVER_COMPARATOR
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    METHODS parse
      IMPORTING
        comp TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

    METHODS test
      IMPORTING
        version       TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS intersects
      IMPORTING
        comp          TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA options TYPE ZIF_ABAPPM_SEMVER_OPTIONS=>TY_OPTIONS.

ENDCLASS.



CLASS ZCL_ABAPPM_SEMVER_COMPARATOR IMPLEMENTATION.


  METHOD class_constructor.

    TRY.
        any_semver = ZCL_ABAPPM_SEMVER=>CREATE( '9999.9999.9999' ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
    ENDTRY.

    " any_semver must be valid
    ASSERT any_semver IS BOUND.

  ENDMETHOD.


  METHOD constructor.

    options-loose  = loose.
    options-incpre = incpre.

    parse( ZCL_ABAPPM_SEMVER_UTILS=>TRIM( comp ) ).

    IF semver = any_semver.
      value = ''.
    ELSE.
      value = operator && semver->version.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( comp )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND comp IS INSTANCE OF ZCL_ABAPPM_SEMVER_COMPARATOR.

      result = comp.

      IF result->options-loose = loose AND result->options-incpre = incpre.
        RETURN.
      ENDIF.

      result = NEW ZCL_ABAPPM_SEMVER_COMPARATOR( comp = |{ result->value }| loose = loose incpre = incpre ).

    ELSEIF kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.

      result = NEW ZCL_ABAPPM_SEMVER_COMPARATOR( comp = |{ comp }| loose = loose incpre = incpre ).

    ELSE.
      ZCX_ABAPPM_SEMVER_ERROR=>RAISE( 'Invalid parameter type' ).
    ENDIF.

  ENDMETHOD.


  METHOD intersects.

    IF comp IS INITIAL.
      ZCX_ABAPPM_SEMVER_ERROR=>RAISE( 'A comparator is required' ).
    ENDIF.

    DATA(semcomp) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( comp ).

    CHECK semcomp IS BOUND.

    IF operator = ''.
      IF value = ''.
        result = abap_true.
      ELSE.
        DATA(semrange) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = semcomp->value loose = loose incpre = incpre ).

        CHECK semrange IS BOUND.

        result = semrange->test( value ).
      ENDIF.
    ELSEIF semcomp->operator = ''.
      IF semcomp->value = ''.
        result = abap_true.
      ELSE.
        semrange = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = value loose = loose incpre = incpre ).

        CHECK semrange IS BOUND.

        result = semrange->test( semcomp->semver ).
      ENDIF.
    ELSE.
      " Special cases where nothing can possibly be lower
      IF incpre = abap_true AND value = '<0.0.0-0' OR semcomp->value = '<0.0.0-0'.
        result = abap_false.
        RETURN.
      ENDIF.
      IF incpre = abap_false AND value CP '<0.0.0*' OR semcomp->value CP '<0.0.0*'.
        result = abap_false.
        RETURN.
      ENDIF.

      " Same direction increasing (> or >=)
      IF operator CP '>*' AND semcomp->operator CP '>*'.
        result = abap_true.
        RETURN.
      ENDIF.
      " Same direction decreasing (< or <=)
      IF operator CP '<*' AND semcomp->operator CP '<*'.
        result = abap_true.
        RETURN.
      ENDIF.

      " same SemVer and both sides are inclusive (<= or >=)
      IF semver->version = semcomp->semver->version AND operator CA '=' AND semcomp->operator CA '='.
        result = abap_true.
        RETURN.
      ENDIF.

      " opposite directions less than
      IF ZCL_ABAPPM_SEMVER_FUNCTIONS=>CMP(
        a      = semver->version
        op     = '<'
        b      = semcomp->semver->version
        loose  = loose
        incpre = incpre ) AND operator CP '>*' AND semcomp->operator CP '<*'.
        result = abap_true.
        RETURN.
      ENDIF.
      " opposite directions greater than
      IF ZCL_ABAPPM_SEMVER_FUNCTIONS=>CMP(
        a      = semver->version
        op     = '>'
        b      = semcomp->semver->version
        loose  = loose
        incpre = incpre ) AND operator CP '<*' AND semcomp->operator CP '>*'.
        result = abap_true.
        RETURN.
      ENDIF.
      result = abap_false.


*      DATA(same_direction_increasing) = xsdbool(
*      ( operator = '>=' OR operator = '>' ) AND
*      ( semcomp->operator = '>=' OR semcomp->operator = '>' ) ).
*      DATA(same_direction_decreasing) = xsdbool(
*      ( operator = '<=' OR operator = '<' ) AND
*      ( semcomp->operator = '<=' OR semcomp->operator = '<' ) ).
*      DATA(same_semver) = xsdbool(
*      semver->version = semcomp->semver->version ).
*      DATA(different_directions_inclusive) = xsdbool(
*      ( operator = '>=' OR operator = '<=' ) AND
*      ( semcomp->operator = '>=' OR semcomp->operator = '<=' ) ).
*      DATA(opposite_directions_less) = xsdbool(
*      zcl_semver_functions=>cmp(
*      a     = semver->version
*      op    = '<'
*      b     = semcomp->semver->version
*      loose = loose ) AND
*      ( operator = '>=' OR operator = '>' ) AND
*      ( semcomp->operator = '<=' OR semcomp->operator = '<' ) ).
*      DATA(opposite_directions_greater) = xsdbool(
*      zcl_semver_functions=>cmp(
*      a     = semver->version
*      op    = '>'
*      b     = semcomp->semver->version
*      loose = loose ) AND
*      ( operator = '<=' OR operator = '<' ) AND
*      ( semcomp->operator = '>=' OR semcomp->operator = '>' ) ).
*
*      result = xsdbool(
*      same_direction_increasing = abap_true OR
*      same_direction_decreasing = abap_true OR
*      ( same_semver = abap_true AND different_directions_inclusive = abap_true ) OR
*      opposite_directions_less = abap_true OR
*      opposite_directions_greater = abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    " initial comparator means anything is allowed
    IF comp IS INITIAL.
      semver = any_semver.
      RETURN.
    ENDIF.

    DATA(r) = COND #(
      WHEN options-loose = abap_true
      THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-COMPARATORLOOSE-SAFE_REGEX
      ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-COMPARATOR-SAFE_REGEX ).

    TRY.
        DATA(m) = r->create_matcher( text = comp ).

        IF NOT m->match( ).
          ZCX_ABAPPM_SEMVER_ERROR=>RAISE( |Invalid comparator: { comp }| ).
        ENDIF.

        operator = m->get_submatch( 1 ).

        IF operator = '='.
          operator = ''.
        ENDIF.

        " if it literally is just '>' or '' then allow anything
        IF m->get_submatch( 2 ) IS INITIAL.
          semver = any_semver.
        ELSE.
          semver = ZCL_ABAPPM_SEMVER=>CREATE( version = m->get_submatch( 2 ) loose = options-loose incpre = options-incpre ).
        ENDIF.

      CATCH cx_sy_matcher.
        ZCX_ABAPPM_SEMVER_ERROR=>RAISE( |Error evaluating regex for { comp }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD test.

    TRY.
        DATA(testver) = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = options-loose incpre = options-incpre ).

        IF semver = any_semver OR testver = any_semver.
          result = abap_true.
        ELSE.
          result = ZCL_ABAPPM_SEMVER_FUNCTIONS=>CMP(
            a     = testver->version
            op    = operator
            b     = semver->version
            loose = options-loose ).
        ENDIF.

      CATCH ZCX_ABAPPM_SEMVER_ERROR.
        result = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD to_string.
    result = value.
  ENDMETHOD.
ENDCLASS.
