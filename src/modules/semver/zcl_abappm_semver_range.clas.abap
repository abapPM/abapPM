CLASS ZCL_ABAPPM_SEMVER_RANGE DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Range
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_comparators TYPE STANDARD TABLE OF REF TO ZCL_ABAPPM_SEMVER_COMPARATOR WITH DEFAULT KEY,
      ty_set         TYPE STANDARD TABLE OF ty_comparators WITH DEFAULT KEY.

    DATA:
      range TYPE string READ-ONLY,
      set   TYPE ty_set READ-ONLY.

    METHODS constructor
      IMPORTING
        !range  TYPE string
        !loose  TYPE abap_bool DEFAULT abap_false
        !incpre TYPE abap_bool DEFAULT abap_false
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS create
      IMPORTING
        !range        TYPE any
        !loose        TYPE abap_bool DEFAULT abap_false
        !incpre       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO ZCL_ABAPPM_SEMVER_RANGE
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    METHODS format
      RETURNING
        VALUE(result) TYPE string.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

    METHODS parse_range
      IMPORTING
        !range_string TYPE string
      RETURNING
        VALUE(result) TYPE ty_comparators
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    METHODS test
      IMPORTING
        !version      TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    METHODS intersects
      IMPORTING
        !range        TYPE REF TO ZCL_ABAPPM_SEMVER_RANGE
        !loose        TYPE abap_bool DEFAULT abap_false
        !incpre       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS no_replace TYPE string VALUE '!replace'.

    TYPES:
      BEGIN OF ty_cache_entry,
        key   TYPE string,
        value TYPE ty_comparators,
      END OF ty_cache_entry,
      ty_cache TYPE HASHED TABLE OF ty_cache_entry WITH UNIQUE KEY key.

    DATA:
      raw     TYPE string,
      cache   TYPE ty_cache,
      options TYPE ZIF_ABAPPM_SEMVER_OPTIONS=>TY_OPTIONS.

    CLASS-METHODS is_any
      IMPORTING
        !comp         TYPE REF TO ZCL_ABAPPM_SEMVER_COMPARATOR
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_null_set
      IMPORTING
        !comp         TYPE REF TO ZCL_ABAPPM_SEMVER_COMPARATOR
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_x
      IMPORTING
        !id           TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_satisfiable
      IMPORTING
        !comparators  TYPE ty_comparators
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS parse_comparator
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS replace_tildes
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS replace_tilde
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS replace_carets
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS replace_caret
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS replace_xranges
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS replace_xrange
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS replace_stars
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS replace_gte0
      IMPORTING
        !comp         TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS replace_hyphen
      IMPORTING
        !range        TYPE string
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS test_set
      IMPORTING
        !comparators  TYPE ty_comparators
        !version      TYPE any
        !loose        TYPE abap_bool
        !incpre       TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string
      RAISING
        ZCX_ABAPPM_SEMVER_ERROR.

    CLASS-METHODS str
      IMPORTING
        !value        TYPE i
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS ZCL_ABAPPM_SEMVER_RANGE IMPLEMENTATION.


  METHOD constructor.

    options-loose  = loose.
    options-incpre = incpre.

    " First reduce all whitespace as much as possible so we do not have to rely
    " on potentially slow regexes like \s*. This is then stored and used for
    " future error messages as well.
    raw = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( range ).

    " First, split on ||
    IF raw IS NOT INITIAL.
      SPLIT raw AT '||' INTO TABLE DATA(ranges).
    ELSE.
      INSERT INITIAL LINE INTO TABLE ranges.
    ENDIF.

    " map the range to a 2d array of comparators
    LOOP AT ranges ASSIGNING FIELD-SYMBOL(<range>).
      INSERT parse_range( ZCL_ABAPPM_SEMVER_UTILS=>TRIM( <range> ) ) INTO TABLE set.
    ENDLOOP.

    " throw out any comparator lists that are empty
    " this generally means that it was not a valid range, which is allowed
    " in loose mode, but will still throw if the WHOLE range is invalid.
    DELETE set WHERE table_line IS INITIAL.

    IF set IS INITIAL.
      ZCX_ABAPPM_SEMVER_ERROR=>RAISE( |Invalid SemVer Range: { raw }| ).
    ENDIF.

    " if we have any that are not the null set, throw out null sets.
    IF lines( set ) > 1.

      " keep the first one, in case they're all null sets
      DATA(first) = set[ 1 ].

      LOOP AT set ASSIGNING FIELD-SYMBOL(<set>).
        IF lines( <set> ) = 1 AND is_null_set( <set>[ 1 ] ).
          DELETE set.
        ENDIF.
      ENDLOOP.

      IF set IS INITIAL.
        set = VALUE #( ( first ) ).
      ELSEIF lines( set ) > 1.
        " if we have any that are *, then the range is just *
        LOOP AT set ASSIGNING <set>.
          IF lines( <set> ) = 1 AND is_any( <set>[ 1 ] ).
            DATA(star) = <set>.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF star IS NOT INITIAL.
          set = VALUE #( ( star ) ).
        ENDIF.
      ENDIF.

    ENDIF.

    format( ).

  ENDMETHOD.


  METHOD create.

    DATA comp TYPE REF TO ZCL_ABAPPM_SEMVER_COMPARATOR.

    DATA(kind) = cl_abap_typedescr=>describe_by_data( range )->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND range IS INSTANCE OF ZCL_ABAPPM_SEMVER_RANGE.

      result ?= range.

      IF result->options-loose = loose AND result->options-incpre = incpre.
        RETURN.
      ENDIF.

      result = NEW ZCL_ABAPPM_SEMVER_RANGE( range = |{ result->raw }| loose = loose incpre = incpre ).

    ELSEIF kind = cl_abap_typedescr=>typekind_oref AND range IS INSTANCE OF ZCL_ABAPPM_SEMVER_COMPARATOR.

      comp ?= range.

      result = NEW ZCL_ABAPPM_SEMVER_RANGE( range = |{ comp->value }| loose = loose incpre = incpre ).

    ELSEIF kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.

      result = NEW ZCL_ABAPPM_SEMVER_RANGE( range = |{ range }| loose = loose incpre = incpre ).

    ELSE.
      ZCX_ABAPPM_SEMVER_ERROR=>RAISE( 'Invalid parameter type' ).
    ENDIF.

  ENDMETHOD.


  METHOD format.

    DATA comps TYPE string_table.
    DATA comp TYPE string.

    LOOP AT set ASSIGNING FIELD-SYMBOL(<set>).
      LOOP AT <set> ASSIGNING FIELD-SYMBOL(<comp>).
        IF sy-tabix = 1.
          comp = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( <comp>->value ).
        ELSE.
          comp = comp && ` ` && ZCL_ABAPPM_SEMVER_UTILS=>TRIM( <comp>->value ).
        ENDIF.
      ENDLOOP.
      INSERT comp INTO TABLE comps.
    ENDLOOP.

    range = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( concat_lines_of( table = comps sep = `||` ) ).

    result = range.

  ENDMETHOD.


  METHOD intersects.

    " some
    LOOP AT set ASSIGNING FIELD-SYMBOL(<this_comparators>).
      IF is_satisfiable( comparators = <this_comparators> loose = loose incpre = incpre ).
        " some
        LOOP AT range->set ASSIGNING FIELD-SYMBOL(<range_comparators>).
          IF is_satisfiable( comparators = <range_comparators> loose = loose incpre = incpre ).
            " every
            result = abap_true.
            LOOP AT <this_comparators> ASSIGNING FIELD-SYMBOL(<this_comparator>).
              " every
              LOOP AT <range_comparators> ASSIGNING FIELD-SYMBOL(<range_comparator>).
                IF NOT <this_comparator>->intersects( comp = <range_comparator> loose = loose incpre = incpre ).
                  result = abap_false.
                  EXIT.
                ENDIF.
              ENDLOOP.
              IF result = abap_false.
                EXIT.
              ENDIF.
            ENDLOOP.
            IF result = abap_true.
              RETURN.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    result = abap_false.

  ENDMETHOD.


  METHOD is_any.
    result = xsdbool( comp->value = '' ).
  ENDMETHOD.


  METHOD is_null_set.
    result = xsdbool( comp->value = '<0.0.0-0' ).
  ENDMETHOD.


  METHOD is_satisfiable.
    " take a set of comparators and determine whether there
    " exists a version which can satisfy it

    DATA remaining_comparators TYPE ty_comparators.

    result = abap_true.

    remaining_comparators = comparators.

    DATA(i) = lines( remaining_comparators ).

    WHILE result = abap_true AND lines( remaining_comparators ) > 0.
      DATA(test_comparator) = remaining_comparators[ i ].

      LOOP AT remaining_comparators ASSIGNING FIELD-SYMBOL(<other_comparator>).
        DATA(semcomp) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( test_comparator ).

        IF semcomp IS BOUND.
          result = semcomp->intersects( comp = <other_comparator> loose = loose incpre = incpre ).
        ELSE.
          result = abap_false.
        ENDIF.

        IF result = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      DELETE remaining_comparators INDEX i.
      i -= 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD is_x.
    result = xsdbool( id IS INITIAL OR to_lower( id ) = 'x'  OR id = '*' ).
  ENDMETHOD.


  METHOD parse_comparator.
    result = comp.
    result = replace_carets( comp = result loose = loose incpre = incpre ).
    result = replace_tildes( comp = result loose = loose incpre = incpre ).
    result = replace_xranges( comp = result loose = loose incpre = incpre ).
    result = replace_stars( comp = result loose = loose incpre = incpre ).
  ENDMETHOD.


  METHOD parse_range.

    DATA:
      cache_entry TYPE LINE OF ty_cache,
      comp_values TYPE string_table,
      comparators TYPE ty_comparators.

    DATA(range) = range_string.

    " memoize range parsing for performance
    " this is a very hot path, and fully deterministic
    DATA(cache_key) = range && ':' && options-loose && ',' && options-incpre  && ',' && options-rtl.

    READ TABLE cache INTO cache_entry WITH TABLE KEY key = cache_key.
    IF sy-subrc = 0.
      result = cache_entry-value.
      RETURN.
    ENDIF.

    " `1.2.3 - 1.2.4` => `>=1.2.3 <=1.2.4`
    range = replace_hyphen( range = range loose = options-loose incpre = options-incpre ).

    " `> 1.2.3 < 1.2.5` => `>1.2.3 <1.2.5`
    range = replace(
      val   = range
      regex = ZCL_ABAPPM_SEMVER_RE=>TOKEN-COMPARATORTRIM-SAFE_SRC
      with  = ZCL_ABAPPM_SEMVER_RE=>COMPARATOR_TRIM_REPLACE
      occ   = ZCL_ABAPPM_SEMVER_RE=>TOKEN-COMPARATORTRIM-OCC ).

    " `~ 1.2.3` => `~1.2.3`
    range = replace(
      val   = range
      regex = ZCL_ABAPPM_SEMVER_RE=>TOKEN-TILDETRIM-SAFE_SRC
      with  = ZCL_ABAPPM_SEMVER_RE=>TILDE_TRIM_REPLACE
      occ   = ZCL_ABAPPM_SEMVER_RE=>TOKEN-TILDETRIM-OCC ).

    " `^ 1.2.3` => `^1.2.3`
    range = replace(
      val   = range
      regex = ZCL_ABAPPM_SEMVER_RE=>TOKEN-CARETTRIM-SAFE_SRC
      with  = ZCL_ABAPPM_SEMVER_RE=>CARET_TRIM_REPLACE
      occ   = ZCL_ABAPPM_SEMVER_RE=>TOKEN-CARETTRIM-OCC ).

    " At this point, the range is completely trimmed and
    " ready to be split into comparators.

    SPLIT range AT ` ` INTO TABLE DATA(comps).

    LOOP AT comps ASSIGNING FIELD-SYMBOL(<comp>).
      <comp> = parse_comparator( comp = <comp> loose = options-loose incpre = options-incpre ).
    ENDLOOP.

    range = condense( concat_lines_of( table = comps sep = ` ` ) ).

    IF range IS INITIAL.
      range = ` `.
    ENDIF.

    SPLIT range AT ` ` INTO TABLE comps.

    LOOP AT comps ASSIGNING <comp>.
      <comp> = replace_gte0( comp = <comp> loose = options-loose incpre = options-incpre ).
    ENDLOOP.

    IF options-loose = abap_true.
      " in loose mode, throw out any that are not valid comparators
      DATA(r) = ZCL_ABAPPM_SEMVER_RE=>TOKEN-COMPARATORLOOSE-SAFE_REGEX.
      LOOP AT comps ASSIGNING <comp>.
        DATA(m) = r->create_matcher( text = <comp> ).
        IF NOT m->match( ).
          DELETE comps.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " if any comparators are the null set, then replace with JUST null set
    " if more than one comparator, remove any * comparators
    " also, don't include the same comparator more than once

    LOOP AT comps ASSIGNING <comp>.
      DATA(semcomp) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( comp = <comp> loose = options-loose incpre = options-incpre ).
      INSERT semcomp INTO TABLE comparators.
    ENDLOOP.

    LOOP AT comparators ASSIGNING FIELD-SYMBOL(<semcomp>).
      IF is_null_set( <semcomp> ).
        result = VALUE #( ( <semcomp> ) ).
        EXIT.
      ENDIF.
      IF <semcomp>->semver IS NOT INITIAL AND NOT line_exists( comp_values[ table_line = <semcomp>->value ] ).
        INSERT <semcomp>->value INTO TABLE comp_values.
        INSERT <semcomp> INTO TABLE result.
      ENDIF.
    ENDLOOP.

    cache_entry-key   = cache_key.
    cache_entry-value = result.
    INSERT cache_entry INTO TABLE cache.

  ENDMETHOD.


  METHOD replace_caret.
    " ^ --> * (any, kinda silly)
    " ^2, ^2.x, ^2.x.x --> >=2.0.0 <3.0.0-0
    " ^2.0, ^2.0.x --> >=2.0.0 <3.0.0-0
    " ^1.2, ^1.2.x --> >=1.2.0 <2.0.0-0
    " ^1.2.3 --> >=1.2.3 <2.0.0-0
    " ^1.2.0 --> >=1.2.0 <2.0.0-0
    " ^0.0.1 --> >=0.0.1 <0.0.2-0
    " ^0.1.0 --> >=0.1.0 <0.2.0-0
    result = comp.

    DATA(r) = COND #(
      WHEN loose = abap_true
      THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-CARETLOOSE-SAFE_REGEX
      ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-CARET-SAFE_REGEX ).

    TRY.
        DATA(m) = r->create_matcher( text = result ).

        WHILE m->find_next( ).
          DATA(ma) = m->get_submatch( 1 ).
          DATA(mi) = m->get_submatch( 2 ).
          DATA(pa) = m->get_submatch( 3 ).
          DATA(pr) = m->get_submatch( 4 ).
          "DATA(bi)  = m->get_submatch( 5 )

          DATA(z) = COND #( WHEN incpre = abap_true THEN '-0' ELSE '' ).

          DATA(with) = no_replace.

          IF is_x( ma ).
            with = ''.
          ELSEIF is_x( mi ).
            with = |>={ ma }.0.0{ z } <{ str( ma + 1 ) }.0.0-0|.
          ELSEIF is_x( pa ).
            IF ma = '0'.
              with = |>={ ma }.{ mi }.0{ z } <{ ma }.{ str( mi + 1 ) }.0-0|.
            ELSE.
              with = |>={ ma }.{ mi }.0{ z } <{ str( ma + 1 ) }.0.0-0|.
            ENDIF.
          ELSEIF pr IS NOT INITIAL.
            IF ma = '0'.
              IF mi = '0'.
                with = |>={ ma }.{ mi }.{ pa }-{ pr } <{ ma }.{ mi }.{ str( pa + 1 ) }-0|.
              ELSE.
                with = |>={ ma }.{ mi }.{ pa }-{ pr } <{ ma }.{ str( mi + 1 ) }.0-0|.
              ENDIF.
            ELSE.
              with = |>={ ma }.{ mi }.{ pa }-{ pr } <{ str( ma + 1 ) }.0.0-0|.
            ENDIF.
          ELSE.
            IF ma = '0'.
              IF mi = '0'.
                with = |>={ ma }.{ mi }.{ pa }{ z } <{ ma }.{ mi }.{ str( pa + 1 )  }-0|.
              ELSE.
                with = |>={ ma }.{ mi }.{ pa }{ z } <{ ma }.{ str( mi + 1 ) }.0-0|.
              ENDIF.
            ELSE.
              with = |>={ ma }.{ mi }.{ pa } <{ str( ma + 1 ) }.0.0-0|.
            ENDIF.
          ENDIF.

          IF with <> no_replace.
            m->replace_found( with ).
          ENDIF.

          result = m->text.
        ENDWHILE.
      CATCH cx_sy_arithmetic_overflow.
        ZCX_ABAPPM_SEMVER_ERROR=>RAISE( 'Overflow' ).
      CATCH cx_sy_regex cx_sy_matcher INTO DATA(error).
        BREAK-POINT.
        " zcx_semver_error=>raise_with_text( error )
    ENDTRY.

  ENDMETHOD.


  METHOD replace_carets.

    SPLIT ZCL_ABAPPM_SEMVER_UTILS=>TRIM( comp ) AT ` ` INTO TABLE DATA(comps).

    LOOP AT comps ASSIGNING FIELD-SYMBOL(<comp>).
      <comp> = replace_caret( comp = <comp> loose = loose incpre = incpre ).
    ENDLOOP.

    result = concat_lines_of( table = comps sep = ` ` ).

  ENDMETHOD.


  METHOD replace_gte0.

    DATA(regex) = COND #(
      WHEN incpre = abap_true
      THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-GTE0PRE-SAFE_SRC
      ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-GTE0-SAFE_SRC ).

    result = replace(
      val   = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( comp )
      regex = regex
      with  = '' ).

  ENDMETHOD.


  METHOD replace_hyphen.
    " 1.2 - 3.4.5 => >=1.2.0 <=3.4.5
    " 1.2.3 - 3.4 => >=1.2.0 <3.5.0-0 Any 3.4.x will do
    " 1.2 - 3.4 => >=1.2.0 <3.5.0-0
    result = range.

    DATA(r) = COND #(
      WHEN loose = abap_true
      THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-HYPHENRANGELOOSE-SAFE_REGEX
      ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-HYPHENRANGE-SAFE_REGEX ).

    TRY.
        DATA(m) = r->create_matcher( text = result ).

        IF m->match( ).

          DATA(from) = m->get_submatch( 1 ).
          DATA(fma)  = m->get_submatch( 2 ).
          DATA(fmi)  = m->get_submatch( 3 ).
          DATA(fpa)  = m->get_submatch( 4 ).
          DATA(fpr)  = m->get_submatch( 5 ).
          "DATA(fbi)  = m->get_submatch( 6 )

          DATA(to)   = m->get_submatch( 7 ).
          DATA(tma)  = m->get_submatch( 8 ).
          DATA(tmi)  = m->get_submatch( 9 ).
          DATA(tpa)  = m->get_submatch( 10 ).
          DATA(tpr)  = m->get_submatch( 11 ).
          "DATA(tbi)  = m->get_submatch( 12 )

          DATA(z) = COND #( WHEN incpre = abap_true THEN '-0' ELSE '' ).

          IF is_x( fma ).
            from = ''.
          ELSEIF is_x( fmi ).
            from = |>={ fma }.0.0{ z }|.
          ELSEIF is_x( fpa ).
            from = |>={ fma }.{ fmi }.0{ z }|.
          ELSEIF fpr IS NOT INITIAL.
            from = |>={ from }|.
          ELSE.
            from = |>={ from }{ z }|.
          ENDIF.

          IF is_x( tma ).
            to = ''.
          ELSEIF is_x( tmi ).
            to = |<{ str( tma + 1 ) }.0.0-0|.
          ELSEIF is_x( tpa ).
            to = |<{ tma }.{ str( tmi + 1 ) }.0-0|.
          ELSEIF tpr IS NOT INITIAL.
            to = |<={ tma }.{ tmi }.{ tpa }-{ tpr }|.
          ELSEIF z IS NOT INITIAL.
            to = |<{ tma }.{ tmi }.{ str( tpa + 1 ) }-0|.
          ELSE.
            to = |<={ to }|.
          ENDIF.

          DATA(with) = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( |{ from } { to }| ).

          m->replace_found( with ).

          result = m->text.
        ENDIF.
      CATCH cx_sy_arithmetic_overflow.
        ZCX_ABAPPM_SEMVER_ERROR=>RAISE( 'Overflow' ).
      CATCH cx_sy_regex cx_sy_matcher INTO DATA(error).
        BREAK-POINT.
        " zcx_semver_error=>raise_with_text( error )
    ENDTRY.

  ENDMETHOD.


  METHOD replace_stars.
    " Because * is AND-ed with everything else in the comparator,
    " and '' means "any version", just remove the *s entirely.

    " Looseness is ignored here.  star is always as loose as it gets!
    result = replace(
      val   = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( comp )
      regex = ZCL_ABAPPM_SEMVER_RE=>TOKEN-STAR-SAFE_SRC
      with  = '' ).

  ENDMETHOD.


  METHOD replace_tilde.
    " ~, ~> --> * (any, kinda silly)
    " ~2, ~2.x, ~2.x.x, ~>2, ~>2.x ~>2.x.x --> >=2.0.0 <3.0.0-0
    " ~2.0, ~2.0.x, ~>2.0, ~>2.0.x --> >=2.0.0 <2.1.0-0
    " ~1.2, ~1.2.x, ~>1.2, ~>1.2.x --> >=1.2.0 <1.3.0-0
    " ~1.2.3, ~>1.2.3 --> >=1.2.3 <1.3.0-0
    " ~1.2.0, ~>1.2.0 --> >=1.2.0 <1.3.0-0
    " ~0.0.1 --> >=0.0.1 <0.1.0-0
    result = comp.

    DATA(r) = COND #(
      WHEN loose = abap_true
      THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-TILDELOOSE-SAFE_REGEX
      ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-TILDE-SAFE_REGEX ).

    TRY.
        DATA(m) = r->create_matcher( text = result ).

        WHILE m->find_next( ).
          DATA(ma) = m->get_submatch( 1 ).
          DATA(mi) = m->get_submatch( 2 ).
          DATA(pa) = m->get_submatch( 3 ).
          DATA(pr) = m->get_submatch( 4 ).
          "DATA(bi)  = m->get_submatch( 5 )

          DATA(with) = no_replace.

          IF is_x( ma ).
            with = ''.
          ELSEIF is_x( mi ).
            with = |>={ ma }.0.0 <{ str( ma + 1 ) }.0.0-0|.
          ELSEIF is_x( pa ).
            " ~1.2 == >=1.2.0 <1.3.0-0
            with = |>={ ma }.{ mi }.0 <{ ma }.{ str( mi + 1 ) }.0-0|.
          ELSEIF pr IS NOT INITIAL.
            with = |>={ ma }.{ mi }.{ pa }-{ pr } <{ ma }.{ str( mi + 1 ) }.0-0|.
          ELSE.
            " ~1.2.3 == >=1.2.3 <1.3.0-0
            with = |>={ ma }.{ mi }.{ pa } <{ ma }.{ str( mi + 1 ) }.0-0|.
          ENDIF.

          IF with <> no_replace.
            m->replace_found( with ).
          ENDIF.

          result = m->text.
        ENDWHILE.
      CATCH cx_sy_arithmetic_overflow.
        ZCX_ABAPPM_SEMVER_ERROR=>RAISE( 'Overflow' ).
      CATCH cx_sy_regex cx_sy_matcher INTO DATA(error).
        BREAK-POINT.
        " zcx_semver_error=>raise_with_text( error )
    ENDTRY.

  ENDMETHOD.


  METHOD replace_tildes.

    SPLIT ZCL_ABAPPM_SEMVER_UTILS=>TRIM( comp ) AT ` ` INTO TABLE DATA(comps).

    LOOP AT comps ASSIGNING FIELD-SYMBOL(<comp>).
      <comp> = replace_tilde( comp = <comp> loose = loose incpre = incpre ).
    ENDLOOP.

    result = concat_lines_of( table = comps sep = ` ` ).

  ENDMETHOD.


  METHOD replace_xrange.

    result = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( comp ).

    DATA(r) = COND #(
      WHEN loose = abap_true
      THEN ZCL_ABAPPM_SEMVER_RE=>TOKEN-XRANGELOOSE-SAFE_REGEX
      ELSE ZCL_ABAPPM_SEMVER_RE=>TOKEN-XRANGE-SAFE_REGEX ).

    TRY.
        DATA(m) = r->create_matcher( text = result ).

        WHILE m->find_next( ).
          DATA(gtlt) = m->get_submatch( 1 ).
          DATA(ma) = m->get_submatch( 2 ).
          DATA(mi) = m->get_submatch( 3 ).
          DATA(pa) = m->get_submatch( 4 ).
          DATA(pr) = m->get_submatch( 5 ).
          "DATA(bi)  = m->get_submatch( 6 )

          DATA(xma) = is_x( ma ).
          DATA(xmi) = xsdbool( xma = abap_true OR is_x( mi ) ).
          DATA(xpa) = xsdbool( xmi = abap_true OR is_x( pa ) ).
          DATA(anyx) = xpa.

          IF gtlt = '=' AND anyx = abap_true.
            gtlt = ''.
          ENDIF.

          " if we're including prereleases in the match, then we need
          " to fix this to -0, the lowest possible prerelease value
          pr = COND #( WHEN incpre = abap_true THEN '-0' ELSE '' ).

          DATA(with) = no_replace.

          IF xma = abap_true.
            IF gtlt = '>' OR gtlt = '<'.
              " nothing is allowed.
              with = '<0.0.0-0'.
            ELSE.
              " nothing is forbidden.
              with = '*'.
            ENDIF.
          ELSEIF gtlt IS NOT INITIAL AND anyx  = abap_true.
            " we know patch is an x, because we have any x at all.
            " replace X with 0.
            IF xmi = abap_true.
              mi = `0`.
            ENDIF.
            pa = `0`.

            IF gtlt = '>'.
              " >1 => >=2.0.0.
              " >1.2 => >=1.3.0.
              gtlt = '>='.
              IF xmi = abap_true.
                ma = str( ma + 1 ).
                mi = `0`.
                pa = `0`.
              ELSE.
                mi = str( mi + 1 ).
                pa = `0`.
              ENDIF.
            ELSEIF gtlt = '<='.
              " <=0.7.x is actually <0.8.0, since any 0.7.x should.
              " pass.  Similarly, <=7.x is actually <8.0.0, etc.
              gtlt = '<'.
              IF xmi = abap_true.
                ma = str( ma + 1 ).
              ELSE.
                mi = str( mi + 1 ).
              ENDIF.
            ENDIF.

            IF gtlt = '<'.
              pr = '-0'.
            ENDIF.

            with = |{ gtlt }{ ma }.{ mi }.{ pa }{ pr }|.
          ELSEIF xmi = abap_true.
            with = |>={ ma }.0.0{ pr } <{ str( ma + 1 ) }.0.0-0|.
          ELSEIF xpa = abap_true.
            with = |>={ ma }.{ mi }.0{ pr } <{ ma }.{ str( mi + 1 ) }.0-0|.
          ENDIF.

          IF with <> no_replace.
            m->replace_found( with ).
          ENDIF.

          result = m->text.
        ENDWHILE.
      CATCH cx_sy_arithmetic_overflow.
        ZCX_ABAPPM_SEMVER_ERROR=>RAISE( 'Overflow' ).
      CATCH cx_sy_regex cx_sy_matcher INTO DATA(error).
        BREAK-POINT.
        " zcx_semver_error=>raise_with_text( error )
    ENDTRY.

  ENDMETHOD.


  METHOD replace_xranges.

    SPLIT ZCL_ABAPPM_SEMVER_UTILS=>TRIM( comp ) AT ` ` INTO TABLE DATA(comps).

    LOOP AT comps ASSIGNING FIELD-SYMBOL(<comp>).
      <comp> = replace_xrange( comp = <comp> loose = loose incpre = incpre ).
    ENDLOOP.

    result = concat_lines_of( table = comps sep = ` ` ).

  ENDMETHOD.


  METHOD str.
    " strip trailing space from integer
    result = condense( |{ value }| ).
  ENDMETHOD.


  METHOD test.

    TRY.
        DATA(semver) = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = options-loose incpre = options-incpre ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR.
        result = abap_false.
        RETURN.
    ENDTRY.

    CHECK semver IS BOUND.

    LOOP AT set ASSIGNING FIELD-SYMBOL(<comparators>).
      IF test_set( comparators = <comparators> version = semver loose = options-loose incpre = options-incpre ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    result = abap_false.

  ENDMETHOD.


  METHOD test_set.

    DATA(semver) = ZCL_ABAPPM_SEMVER=>CREATE( version = version loose = loose incpre = incpre ).

    CHECK semver IS BOUND.

    LOOP AT comparators ASSIGNING FIELD-SYMBOL(<comparator>).
      IF <comparator>->test( semver ) = abap_false.
        result = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF semver->prerelease IS NOT INITIAL AND incpre = abap_false.
      " Find the set of versions that are allowed to have prereleases
      " For example, ^1.2.3-pr.1 desugars to >=1.2.3-pr.1 <2.0.0
      " That should allow `1.2.3-pr.2` to pass.
      " However, `1.2.4-alpha.notready` should NOT be allowed,
      " even though it's within the range set by the comparators.
      LOOP AT comparators ASSIGNING <comparator>.
        IF <comparator>->semver = ZCL_ABAPPM_SEMVER_COMPARATOR=>ANY_SEMVER.
          CONTINUE.
        ENDIF.

        IF <comparator>->semver->prerelease IS NOT INITIAL AND
           <comparator>->semver->major = semver->major AND
           <comparator>->semver->minor = semver->minor AND
           <comparator>->semver->patch = semver->patch.
          result = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.

      " Version has a -pre, but it's not one of the ones we like.
      result = abap_false.
      RETURN.
    ENDIF.

    result = abap_true.

  ENDMETHOD.


  METHOD to_string.
    result = range.
  ENDMETHOD.
ENDCLASS.
