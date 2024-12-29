CLASS ltcl_semver_range DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      range_include FOR TESTING RAISING zcx_abappm_semver_error,
      range_intersect FOR TESTING RAISING zcx_abappm_semver_error,
      range_exclude FOR TESTING RAISING zcx_abappm_semver_error,
      range_parse FOR TESTING RAISING zcx_abappm_semver_error,
      range_formatted FOR TESTING RAISING zcx_abappm_semver_error,
      empty_comparator FOR TESTING RAISING zcx_abappm_semver_error,
      create_from_comparator FOR TESTING RAISING zcx_abappm_semver_error,
      create_from_range FOR TESTING RAISING zcx_abappm_semver_error,
      strict_vs_loose FOR TESTING RAISING zcx_abappm_semver_error,
      to_string FOR TESTING RAISING zcx_abappm_semver_error.

ENDCLASS.

CLASS zcl_abappm_semver_range DEFINITION LOCAL FRIENDS ltcl_semver_range.

CLASS ltcl_semver_range IMPLEMENTATION.

  METHOD range_include.

    LOOP AT zcl_abappm_semver_fixtures=>range_include( ) INTO DATA(range_include).
      DATA(msg) = |{ range_include-range } { range_include-version } { range_include-loose } { range_include-incpre }|.
      DATA(r) = zcl_abappm_semver_range=>create(
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

    LOOP AT zcl_abappm_semver_fixtures=>range_exclude( ) INTO DATA(range_exclude).
      DATA(msg) = |{ range_exclude-range } { range_exclude-version } { range_exclude-loose } { range_exclude-incpre }|.
      DATA(r) = zcl_abappm_semver_range=>create(
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

    LOOP AT zcl_abappm_semver_fixtures=>range_intersection( ) INTO DATA(range_intersection).
      DATA(msg) = |{ range_intersection-r0 } { range_intersection-r1 }|.
      DATA(r0) = zcl_abappm_semver_range=>create( range_intersection-r0 ).
      DATA(r1) = zcl_abappm_semver_range=>create( range_intersection-r1 ).

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

    LOOP AT zcl_abappm_semver_fixtures=>range_parse( ) INTO DATA(range_parse).
      DATA(msg) = |{ range_parse-range } { range_parse-loose } { range_parse-incpre }|.

      IF range_parse-res IS INITIAL.
        TRY.
            DATA(r) = zcl_abappm_semver_range=>create(
              range  = range_parse-range
              loose  = range_parse-loose
              incpre = range_parse-incpre ).
            cl_abap_unit_assert=>fail( msg = msg ).
          CATCH zcx_abappm_semver_error ##NO_HANDLER.
        ENDTRY.
      ELSE.
        r = zcl_abappm_semver_range=>create(
          range  = range_parse-range
          loose  = range_parse-loose
          incpre = range_parse-incpre ).

        DATA(res) = COND #( WHEN r->range( ) IS INITIAL THEN `*` ELSE r->range( ) ).

        cl_abap_unit_assert=>assert_equals(
          act = res
          exp = range_parse-res
          msg = msg ).

        DATA(e) = zcl_abappm_semver_range=>create(
          range  = range_parse-res
          loose  = range_parse-loose
          incpre = range_parse-incpre ).

        cl_abap_unit_assert=>assert_equals(
          act = r->range( )
          exp = e->range( )
          msg = msg ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD range_formatted.
    " formatted value is calculated lazily and cached

    DATA(r) = zcl_abappm_semver_range=>create( '>= 1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = r->formatted
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = r->format( )
      exp = '>=1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = r->formatted
      exp = '>=1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = r->format( )
      exp = '>=1.2.3' ).

  ENDMETHOD.


  METHOD empty_comparator.

    TRY.
        DATA(r) = zcl_abappm_semver_range=>create(
          range  = 'sadf||asdf'
          loose  = abap_true ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD create_from_comparator.

    DATA(c) = zcl_abappm_semver_comparator=>create( '>=1.2.3' ).

    DATA(r) = zcl_abappm_semver_range=>create( c ).

    cl_abap_unit_assert=>assert_equals(
      act = r->raw
      exp = c->value ).

  ENDMETHOD.

  METHOD create_from_range.

    DATA(loose) = zcl_abappm_semver_range=>create(
      range  = '1.2.3'
      loose  = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_range=>create( range = loose loose = abap_true )->range( )
      exp = loose->range( ) ).

    cl_abap_unit_assert=>assert_differs(
      act = zcl_abappm_semver_range=>create( range = loose )->options
      exp = loose->options ).

    DATA(incpre) = zcl_abappm_semver_range=>create(
      range  = '1.2.3'
      incpre = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_range=>create( range = incpre incpre = abap_true )->range( )
      exp = incpre->range( ) ).

    cl_abap_unit_assert=>assert_differs(
      act = zcl_abappm_semver_range=>create( range = incpre )->options
      exp = incpre->options ).

  ENDMETHOD.

  METHOD strict_vs_loose.

    TRY.
        DATA(loose) = zcl_abappm_semver_range=>create( '>=01.02.03' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_range=>create( range = '>=01.02.03' loose = abap_true )->range( )
      exp = '>=1.2.3' ).

    TRY.
        loose = zcl_abappm_semver_range=>create( '~1.02.03beta' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_range=>create( range = '~1.02.03beta' loose = abap_true )->range( )
      exp = '>=1.2.3-beta <1.3.0-0' ).

  ENDMETHOD.

  METHOD to_string.

    DATA(semrange) = zcl_abappm_semver_range=>create( '>= v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = semrange->to_string( )
      exp = '>=1.2.3' ).

  ENDMETHOD.

ENDCLASS.
