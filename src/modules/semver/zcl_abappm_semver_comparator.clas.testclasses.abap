CLASS ltcl_semver_comparator DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      comparator FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      to_string FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      intersect FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      any FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      invalid FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      ignore_equal FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR.

ENDCLASS.

CLASS ltcl_semver_comparator IMPLEMENTATION.

  METHOD comparator.

    DATA(c) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( '>=1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = c->test( '1.2.4' )
      exp = abap_true ).

    DATA(c2) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( c ).

    cl_abap_unit_assert=>assert_equals(
      act = c2->test( '1.2.4' )
      exp = abap_true ).

    DATA(c3) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( comp = c loose = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = c3->test( '1.2.4' )
      exp = abap_true ).

    " test an invalid version, should not throw
    DATA(c4) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( c ).

    cl_abap_unit_assert=>assert_equals(
      act = c4->test( 'not a version string' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD to_string.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( '>= 1.2.3' )->to_string( )
      exp = '>=1.2.3' ).

  ENDMETHOD.

  METHOD intersect.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>COMPARATOR_INTERSECTION( ) INTO DATA(intersection).
      DATA(msg) = |{ intersection-c0 } { intersection-c1 }|.
      DATA(comp0) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( intersection-c0 ).
      DATA(comp1) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( intersection-c1 ).

      cl_abap_unit_assert=>assert_equals(
        act = comp0->intersects( comp = comp1 incpre = intersection-incpre )
        exp = intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = comp1->intersects( comp = comp0 incpre = intersection-incpre )
        exp = intersection-res
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD any.
    " ANY matches anything

    DATA(c) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = c->test( '1.2.3' )
      exp = abap_true
      msg = 'ANY should match anything' ).

    DATA(c1) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( '>=1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = c1->test( ZCL_ABAPPM_SEMVER_COMPARATOR=>ANY_SEMVER->VERSION )
      exp = abap_true
      msg = 'anything should match ANY' ).

  ENDMETHOD.

  METHOD invalid.

    TRY.
        DATA(c) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( 'foo bar baz' ).

        cl_abap_unit_assert=>fail( msg = 'Should throw invalid comparator' ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD ignore_equal.
    " equal sign is ignored

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( '=1.2.3' )->value
      exp = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( '1.2.3' )->value ).

  ENDMETHOD.

ENDCLASS.
