CLASS zcl_abappm_semver_ranges DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Ranges
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_hilo       TYPE c LENGTH 1,
      ty_comp_list  TYPE string_table,
      ty_comp_lists TYPE STANDARD TABLE OF string_table WITH DEFAULT KEY.

    CLASS-METHODS gtr
      IMPORTING
        version       TYPE any
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS intersects
      IMPORTING
        r1            TYPE any
        r2            TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS ltr
      IMPORTING
        version       TYPE any
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS max_satisfying
      IMPORTING
        versions      TYPE string_table
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS min_satisfying
      IMPORTING
        versions      TYPE string_table
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS min_version
      IMPORTING
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_semver
      RAISING
        zcx_abappm_error.

    CLASS-METHODS outside
      IMPORTING
        version       TYPE any
        range         TYPE any
        hilo          TYPE ty_hilo OPTIONAL
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS simplify
      IMPORTING
        versions      TYPE string_table
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS subset
      IMPORTING
        sub           TYPE any
        dom           TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error ##NEEDED.

    CLASS-METHODS to_comparators
      IMPORTING
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE ty_comp_lists
      RAISING
        zcx_abappm_error.

    CLASS-METHODS valid_range
      IMPORTING
        range         TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abappm_semver_ranges IMPLEMENTATION.


  METHOD gtr.
    result = outside(
      version = version
      range   = range
      hilo    = '>'
      loose   = loose
      incpre  = incpre ).
  ENDMETHOD.


  METHOD intersects.

    DATA(semrange1) = zcl_abappm_semver_range=>create( range = r1 loose = loose incpre = incpre ).

    DATA(semrange2) = zcl_abappm_semver_range=>create( range = r2 loose = loose incpre = incpre ).

    result = semrange1->intersects( range = semrange2 loose = loose incpre = incpre ).

  ENDMETHOD.


  METHOD ltr.
    result = outside(
      version = version
      range   = range
      hilo    = '<'
      loose   = loose
      incpre  = incpre ).
  ENDMETHOD.


  METHOD max_satisfying.

    DATA:
      max   TYPE string,
      maxsv TYPE REF TO zcl_abappm_semver.

    TRY.
        DATA(semrange) = zcl_abappm_semver_range=>create( range = range loose = loose incpre = incpre ).
      CATCH zcx_abappm_error.
        result = ''.
        RETURN.
    ENDTRY.

    LOOP AT versions ASSIGNING FIELD-SYMBOL(<version>).
      IF semrange->test( <version> ).
        " satisfies(v, range, options)
        IF max IS INITIAL OR maxsv->compare( <version> ) = -1.
          " compare(max, v, true)
          max = <version>.
          maxsv = zcl_abappm_semver=>create( version = max loose = loose incpre = incpre ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = max.

  ENDMETHOD.


  METHOD min_satisfying.

    DATA:
      min   TYPE string,
      minsv TYPE REF TO zcl_abappm_semver.

    TRY.
        DATA(semrange) = zcl_abappm_semver_range=>create( range = range loose = loose incpre = incpre ).
      CATCH zcx_abappm_error.
        result = ''.
        RETURN.
    ENDTRY.

    LOOP AT versions ASSIGNING FIELD-SYMBOL(<version>).
      IF semrange->test( <version> ).
        " satisfies(v, range, options)
        IF min IS INITIAL OR minsv->compare( <version> ) = +1.
          " compare(min, v, true)
          min = <version>.
          minsv = zcl_abappm_semver=>create( version = min loose = loose incpre = incpre ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = min.

  ENDMETHOD.


  METHOD min_version.

    DATA setmin TYPE REF TO zcl_abappm_semver.

    DATA(semrange) = zcl_abappm_semver_range=>create( range = range loose = loose incpre = incpre ).

    DATA(minver) = zcl_abappm_semver=>create( '0.0.0' ).
    IF semrange->test( minver ).
      result = minver.
      RETURN.
    ENDIF.

    minver = zcl_abappm_semver=>create( '0.0.0-0' ).
    IF semrange->test( minver ).
      result = minver.
      RETURN.
    ENDIF.

    CLEAR minver.

    LOOP AT semrange->set ASSIGNING FIELD-SYMBOL(<set>).
      DATA(comparators) = <set>.

      CLEAR setmin.
      LOOP AT comparators ASSIGNING FIELD-SYMBOL(<comparator>).
        " Clone to avoid manipulating the comparator's semver object.
        DATA(compver) = zcl_abappm_semver=>create( <comparator>->semver->version ).

        CASE <comparator>->operator.
          WHEN '>'.
            IF compver->prerelease IS INITIAL.
              compver->inc( 'patch' ).
            ELSE.
              compver->inc( release = 'prepush' identifier_base = '0' ).
            ENDIF.

            IF setmin IS INITIAL OR zcl_abappm_semver_functions=>gt( a = compver b = setmin ).
              setmin = compver.
            ENDIF.

          WHEN '' OR '>='.
            IF setmin IS INITIAL OR zcl_abappm_semver_functions=>gt( a = compver b = setmin ).
              setmin = compver.
            ENDIF.

          WHEN '<' OR '<='.
            " Ignore maximum versions

          WHEN OTHERS.
            zcx_abappm_error=>raise( |Unexpected operation: { <comparator>->operator }| ).
        ENDCASE.
      ENDLOOP.

      IF setmin IS NOT INITIAL AND ( minver IS INITIAL OR zcl_abappm_semver_functions=>gt( a = minver b = setmin ) ).
        minver = setmin.
      ENDIF.
    ENDLOOP.

    IF minver IS NOT INITIAL AND semrange->test( minver ).
      result = minver.
      RETURN.
    ENDIF.

    CLEAR result.

  ENDMETHOD.


  METHOD outside.

    DATA:
      high TYPE REF TO zcl_abappm_semver_comparator,
      low  TYPE REF TO zcl_abappm_semver_comparator.

    DATA(semver) = zcl_abappm_semver=>create( version = version loose = loose incpre = incpre ).

    DATA(semrange) = zcl_abappm_semver_range=>create( range = range loose = loose incpre = incpre ).

    IF hilo NA '<>'.
      zcx_abappm_error=>raise( 'Must provide a hilo val of "<" or ">"' ).
    ENDIF.

    DATA(comp)  = hilo.
    DATA(ecomp) = comp && '='.

    " If it satisfies the range it is not outside
    IF zcl_abappm_semver_functions=>satisfies( version = semver range = semrange loose = loose incpre = incpre ).
      result = abap_false.
      RETURN.
    ENDIF.

    LOOP AT semrange->set ASSIGNING FIELD-SYMBOL(<set>).
      DATA(comparators) = <set>.

      CLEAR: high, low.

      LOOP AT comparators ASSIGNING FIELD-SYMBOL(<comparator>).
        IF <comparator> = zcl_abappm_semver_comparator=>any_semver.
          <comparator> = zcl_abappm_semver_comparator=>create( '>=0.0.0' ).
        ENDIF.

        IF high IS NOT BOUND.
          high = <comparator>.
        ENDIF.
        IF low IS NOT BOUND.
          low = <comparator>.
        ENDIF.

        CASE hilo.
          WHEN '>'.
            IF zcl_abappm_semver_functions=>gt(
              a      = <comparator>->semver
              b      = high->semver
              loose  = loose
              incpre = incpre ).

              high = <comparator>.
            ELSEIF zcl_abappm_semver_functions=>lt(
              a      = <comparator>->semver
              b      = low->semver
              loose  = loose
              incpre = incpre ).

              low = <comparator>.
            ENDIF.
          WHEN '<'.
            IF zcl_abappm_semver_functions=>lt(
              a      = <comparator>->semver
              b      = high->semver
              loose  = loose
              incpre = incpre ).

              high = <comparator>.
            ELSEIF zcl_abappm_semver_functions=>gt(
              a      = <comparator>->semver
              b      = low->semver
              loose  = loose
              incpre = incpre ).

              low = <comparator>.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      " If the edge version comparator has a operator then our version isn't outside it
      IF high->operator = comp OR high->operator = ecomp.
        result = abap_false.
        RETURN.
      ENDIF.

      " If the lowest version comparator has an operator and our version
      " is less than it then it isn't higher than the range
      CASE hilo.
        WHEN '>'.
          IF ( low->operator IS INITIAL OR low->operator = comp ) AND
            zcl_abappm_semver_functions=>lte( a = semver b =  low->semver ).

            result = abap_false.
            RETURN.
          ELSEIF low->operator = ecomp AND zcl_abappm_semver_functions=>lt( a = semver b = low->semver ).
            result = abap_false.
            RETURN.
          ENDIF.
        WHEN '<'.
          IF ( low->operator IS INITIAL OR low->operator = comp ) AND
            zcl_abappm_semver_functions=>gte( a = semver b =  low->semver ).

            result = abap_false.
            RETURN.
          ELSEIF low->operator = ecomp AND zcl_abappm_semver_functions=>gt( a = semver b = low->semver ).
            result = abap_false.
            RETURN.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    result = abap_true.

  ENDMETHOD.


  METHOD simplify.
    " Given a set of versions and a range, create a "simplified" range
    " that includes the same versions that the original range does
    " If the original range is shorter than the simplified one, return that.

    TYPES:
      BEGIN OF ty_min_max,
        min TYPE string,
        max TYPE string,
      END OF ty_min_max,
      ty_set TYPE STANDARD TABLE OF ty_min_max WITH KEY min max.

    DATA minmax TYPE ty_min_max.
    DATA set TYPE ty_set.
    DATA ranges TYPE string_table.

    IF versions IS INITIAL.
      zcx_abappm_error=>raise( 'Empty version list' ).
    ENDIF.

    DATA(semrange) = zcl_abappm_semver_range=>create( range = range loose = loose incpre = incpre ).

    DATA(first) = ``.
    DATA(prev) = ``.

    DATA(v) = zcl_abappm_semver_functions=>sort( list = versions loose = loose incpre = incpre ).

    LOOP AT v ASSIGNING FIELD-SYMBOL(<version>).
      IF zcl_abappm_semver_functions=>satisfies(
        version = <version>
        range   = semrange
        loose   = loose
        incpre  = incpre ).

        prev = <version>.
        IF first IS INITIAL.
          first = <version>.
        ENDIF.
      ELSE.
        IF prev IS NOT INITIAL.
          minmax = VALUE #( min = first max = prev ).
          INSERT minmax INTO TABLE set.
        ENDIF.
        CLEAR prev.
        CLEAR first.
      ENDIF.
    ENDLOOP.

    IF first IS NOT INITIAL.
      minmax = VALUE #( min = first max = `` ).
      INSERT minmax INTO TABLE set.
    ENDIF.

    LOOP AT set ASSIGNING FIELD-SYMBOL(<set>).
      IF <set>-min = <set>-max.
        INSERT <set>-min INTO TABLE ranges.
      ELSEIF <set>-max IS INITIAL AND  v[ 1 ] = <set>-min.
        INSERT |*| INTO TABLE ranges.
      ELSEIF <set>-max IS INITIAL.
        INSERT |>={ <set>-min }| INTO TABLE ranges.
      ELSEIF  v[ 1 ] = <set>-min.
        INSERT |<={ <set>-max }| INTO TABLE ranges.
      ELSE.
        INSERT |{ <set>-min } - { <set>-max }| INTO TABLE ranges.
      ENDIF.
    ENDLOOP.

    DATA(simplified) = concat_lines_of( table = ranges sep = ' || ' ).

    DATA(kind) = cl_abap_typedescr=>describe_by_data( range )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.
      DATA(original) = CONV string( range ).
    ELSE.
      original = semrange->to_string( ).
    ENDIF.

    IF strlen( simplified ) < strlen( original ).
      result = simplified.
    ELSE.
      result = range.
    ENDIF.

  ENDMETHOD.


  METHOD subset.
    " Complex range `r1 || r2 || ...` is a subset of `R1 || R2 || ...` iff:
    " - Every simple range `r1, r2, ...` is a null set, OR
    " - Every simple range `r1, r2, ...` which is not a null set is a subset of
    "   some `R1, R2, ...`
    "
    " Simple range `c1 c2 ...` is a subset of simple range `C1 C2 ...` iff:
    " - If c is only the ANY comparator
    "   - If C is only the ANY comparator, return true
    "   - Else if in prerelease mode, return false
    "   - else replace c with `[>=0.0.0]`
    " - If C is only the ANY comparator
    "   - if in prerelease mode, return true
    "   - else replace C with `[>=0.0.0]`
    " - Let EQ be the set of = comparators in c
    " - If EQ is more than one, return true (null set)
    " - Let GT be the highest > or >= comparator in c
    " - Let LT be the lowest < or <= comparator in c
    " - If GT and LT, and GT.semver > LT.semver, return true (null set)
    " - If any C is a = range, and GT or LT are set, return false
    " - If EQ
    "   - If GT, and EQ does not satisfy GT, return true (null set)
    "   - If LT, and EQ does not satisfy LT, return true (null set)
    "   - If EQ satisfies every C, return true
    "   - Else return false
    " - If GT
    "   - If GT.semver is lower than any > or >= comp in C, return false
    "   - If GT is >=, and GT.semver does not satisfy every C, return false
    "   - If GT.semver has a prerelease, and not in prerelease mode
    "     - If no C has a prerelease and the GT.semver tuple, return false
    " - If LT
    "   - If LT.semver is greater than any < or <= comp in C, return false
    "   - If LT is <=, and LT.semver does not satisfy every C, return false
    "   - If GT.semver has a prerelease, and not in prerelease mode
    "     - If no C has a prerelease and the LT.semver tuple, return false
    " - Else return true

    " https://github.com/npm/node-semver/blob/main/ranges/subset.js
    zcx_abappm_error=>raise( 'TODO' ).

  ENDMETHOD.


  METHOD to_comparators.
    " Mostly just for testing and legacy API reasons

    DATA comp_list TYPE ty_comp_list.

    DATA(semrange) = zcl_abappm_semver_range=>create( range = range loose = loose incpre = incpre ).

    LOOP AT semrange->set ASSIGNING FIELD-SYMBOL(<set>).
      CLEAR comp_list.
      LOOP AT <set> ASSIGNING FIELD-SYMBOL(<comparator>).
        INSERT <comparator>->value INTO TABLE comp_list.
      ENDLOOP.
      INSERT comp_list INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD valid_range.

    TRY.
        " Return '*' instead of '' so that truthiness works.
        " This will throw if it's invalid anyway
        DATA(semrange) = zcl_abappm_semver_range=>create( range = range loose = loose incpre = incpre ).

        IF semrange IS BOUND AND semrange->range( ) IS NOT INITIAL.
          result = semrange->range( ).
        ELSE.
          result = '*'.
        ENDIF.
      CATCH zcx_abappm_error.
        result = ''.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
