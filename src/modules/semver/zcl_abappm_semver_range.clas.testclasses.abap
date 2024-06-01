CLASS ltcl_semver_range DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      range_include FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      range_intersect FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      range_exclude FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      range_parse FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      empty_comparator FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      create_from_comparator FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      create_from_range FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      strict_vs_loose FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      to_string FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR.

ENDCLASS.

CLASS ZCL_ABAPPM_SEMVER_RANGE DEFINITION LOCAL FRIENDS ltcl_semver_range.

CLASS ltcl_semver_range IMPLEMENTATION.

  METHOD range_include.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>RANGE_INCLUDE( ) INTO DATA(range_include).
      DATA(msg) = |{ range_include-range } { range_include-version } { range_include-loose } { range_include-incpre }|.
      DATA(r) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
        range  = range_include-range
        loose  = range_include-loose
        incpre = range_include-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = r->test( range_include-version )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD range_exclude.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>RANGE_EXCLUDE( ) INTO DATA(range_exclude).
      DATA(msg) = |{ range_exclude-range } { range_exclude-version } { range_exclude-loose } { range_exclude-incpre }|.
      DATA(r) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
        range  = range_exclude-range
        loose  = range_exclude-loose
        incpre = range_exclude-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = r->test( range_exclude-version )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD range_intersect.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>RANGE_INTERSECTION( ) INTO DATA(range_intersection).
      DATA(msg) = |{ range_intersection-r0 } { range_intersection-r1 }|.
      DATA(r0) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range_intersection-r0 ).
      DATA(r1) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range_intersection-r1 ).

      cl_abap_unit_assert=>assert_equals(
        act = r0->intersects( r1 )
        exp = range_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = r1->intersects( r0 )
        exp = range_intersection-res
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD range_parse.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>RANGE_PARSE( ) INTO DATA(range_parse).
      DATA(msg) = |{ range_parse-range } { range_parse-loose } { range_parse-incpre }|.

      IF range_parse-res IS INITIAL.
        TRY.
            DATA(r) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
              range  = range_parse-range
              loose  = range_parse-loose
              incpre = range_parse-incpre ).
            cl_abap_unit_assert=>fail( msg = msg ).
          CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
        ENDTRY.
      ELSE.
        r = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
          range  = range_parse-range
          loose  = range_parse-loose
          incpre = range_parse-incpre ).

        DATA(res) = COND #( WHEN r->range IS INITIAL THEN `*` ELSE r->range ).

        cl_abap_unit_assert=>assert_equals(
          act = res
          exp = range_parse-res
          msg = msg ).

        DATA(e) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
          range  = range_parse-res
          loose  = range_parse-loose
          incpre = range_parse-incpre ).

        cl_abap_unit_assert=>assert_equals(
          act = r->range
          exp = e->range
          msg = msg ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD empty_comparator.

    TRY.
        DATA(r) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
          range  = 'sadf||asdf'
          loose  = abap_true ).
        cl_abap_unit_assert=>fail( ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD create_from_comparator.

    DATA(c) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( '>=1.2.3' ).

    DATA(r) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( c ).

    cl_abap_unit_assert=>assert_equals(
      act = r->raw
      exp = c->value ).

  ENDMETHOD.

  METHOD create_from_range.

    DATA(loose) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
      range  = '1.2.3'
      loose  = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = loose loose = abap_true )->range
      exp = loose->range ).

    cl_abap_unit_assert=>assert_differs(
      act = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = loose )->options
      exp = loose->options ).

    DATA(incpre) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE(
      range  = '1.2.3'
      incpre = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = incpre incpre = abap_true )->range
      exp = incpre->range ).

    cl_abap_unit_assert=>assert_differs(
      act = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = incpre )->options
      exp = incpre->options ).

  ENDMETHOD.

  METHOD strict_vs_loose.

    TRY.
        DATA(loose) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( '>=01.02.03' ).
        cl_abap_unit_assert=>fail( ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = '>=01.02.03' loose = abap_true )->range
      exp = '>=1.2.3' ).

    TRY.
        loose = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( '~1.02.03beta' ).
        cl_abap_unit_assert=>fail( ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range = '~1.02.03beta' loose = abap_true )->range
      exp = '>=1.2.3-beta <1.3.0-0' ).

  ENDMETHOD.

  METHOD to_string.

    DATA(semrange) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( '>= v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = semrange->to_string( )
      exp = '>=1.2.3' ).

  ENDMETHOD.

ENDCLASS.
