CLASS ltcl_semver DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      comparisons FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      equality FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      to_string FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      invalid_versions FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      options FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      really_big FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      incrementing FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      compare_main_vs_pre FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      compare_build FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR.

ENDCLASS.

CLASS ZCL_ABAPPM_SEMVER DEFINITION LOCAL FRIENDS ltcl_semver.

CLASS ltcl_semver IMPLEMENTATION.

  METHOD comparisons.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>COMPARISONS( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose }|.
      DATA(s0) = ZCL_ABAPPM_SEMVER=>CREATE( version = comparison-v0 loose = comparison-loose ).
      DATA(s1) = ZCL_ABAPPM_SEMVER=>CREATE( version = comparison-v1 loose = comparison-loose ).

      cl_abap_unit_assert=>assert_equals( act = s0->compare( s1->version ) exp = 1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( comparison-v1 ) exp = 1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s0->version ) exp = -1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( comparison-v0 ) exp = -1 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( comparison-v0 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( comparison-v1 ) exp = 0 msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD equality.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>EQUALITY( ) INTO DATA(equality).
      DATA(msg) = |{ equality-v0 } { equality-v1 }|.
      DATA(s0) = ZCL_ABAPPM_SEMVER=>CREATE( version = equality-v0 loose = equality-loose ).
      DATA(s1) = ZCL_ABAPPM_SEMVER=>CREATE( version = equality-v1 loose = equality-loose ).

      cl_abap_unit_assert=>assert_equals( act = s0->compare( s1->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s0->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( equality-v1 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( equality-v0 ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare( s0->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s1->compare( s1->version ) exp = 0 msg = msg ).
      cl_abap_unit_assert=>assert_equals( act = s0->compare_pre( s1->version ) exp = 0 msg = msg ). " just to hit that code path
    ENDLOOP.

  ENDMETHOD.

  METHOD to_string.

    DATA(s) = ZCL_ABAPPM_SEMVER=>CREATE( 'v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = s->to_string( )
      exp = '1.2.3'
      msg = 'to_string does not equal parsed version' ).

  ENDMETHOD.

  METHOD invalid_versions.
    " throws when presented with garbage

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>INVALID_VERSIONS( ) INTO DATA(invalid_version).
      DATA(msg) = |{ invalid_version-value } { invalid_version-reason }|.

      TRY.
          DATA(s) = ZCL_ABAPPM_SEMVER=>CREATE( version = invalid_version-value loose = invalid_version-loose ).
          cl_abap_unit_assert=>fail( msg = msg ).
        CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD options.

    DATA(s) = ZCL_ABAPPM_SEMVER=>CREATE( version = '1.2.3' loose = abap_true incpre = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER=>CREATE( version = s loose = abap_true incpre = abap_true )
      exp = s
      msg = 'Should get same object when options match' ).

    IF ZCL_ABAPPM_SEMVER=>CREATE( s ) = s.
      cl_abap_unit_assert=>fail( msg = 'Should get new object when options do not match' ).
    ENDIF.

  ENDMETHOD.

  METHOD really_big.
    " test really big numeric prerelease value.

    DATA(s) = ZCL_ABAPPM_SEMVER=>CREATE( |1.2.3-beta.{ ZIF_ABAPPM_SEMVER_CONSTANTS=>MAX_SAFE_INTEGER }0| ).

    cl_abap_unit_assert=>assert_equals(
      act = s->prerelease[ 1 ]
      exp = 'beta' ).

    cl_abap_unit_assert=>assert_equals(
      act = s->prerelease[ 2 ]
      exp = |{ ZIF_ABAPPM_SEMVER_CONSTANTS=>MAX_SAFE_INTEGER }0| ).

  ENDMETHOD.

  METHOD incrementing.

    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>INCREMENTS( ) INTO DATA(increments).
      DATA(msg) = |{ increments-version } { increments-release } { increments-identifier }|.

      IF increments-res IS INITIAL.
        TRY.
            DATA(s) = ZCL_ABAPPM_SEMVER=>CREATE( version = increments-version loose = increments-loose ).

            s->inc(
              release         = increments-release
              identifier      = increments-identifier
              identifier_base = increments-identifier_base ).
            cl_abap_unit_assert=>fail( msg = msg ).
          CATCH ZCX_ABAPPM_SEMVER_ERROR ##NO_HANDLER.
            " throws when presented with garbage
        ENDTRY.
      ELSE.
        s = ZCL_ABAPPM_SEMVER=>CREATE( version = increments-version loose = increments-loose ).

        DATA(inc) = s->inc(
          release         = increments-release
          identifier      = increments-identifier
          identifier_base = increments-identifier_base ).

        cl_abap_unit_assert=>assert_equals(
          act = inc->version
          exp = increments-res
          msg = msg ).

        IF inc->build IS NOT INITIAL.
          cl_abap_unit_assert=>assert_equals(
            act = inc->raw
            exp = |{ increments-res }+{ concat_lines_of( table = inc->build sep = '.' ) }|
            msg = msg ).
        ELSE.
          cl_abap_unit_assert=>assert_equals(
            act = inc->raw
            exp = increments-res
            msg = msg ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD compare_main_vs_pre.

    DATA(s) = ZCL_ABAPPM_SEMVER=>CREATE( '1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '2.3.4' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '1.2.4' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '0.1.2' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '1.2.2' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = s->compare_main( '1.2.3-pre' )
      exp = 0 ).

    DATA(p) = ZCL_ABAPPM_SEMVER=>CREATE( '1.2.3-alpha.0.pr.1' ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '9.9.9-alpha.0.pr.1' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '1.2.3' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '1.2.3-alpha.0.pr.2' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = p->compare_pre( '1.2.3-alpha.0.2' )
      exp = +1 ).

  ENDMETHOD.

  METHOD compare_build.

    DATA(nobuild) = ZCL_ABAPPM_SEMVER=>CREATE( '1.0.0' ).
    DATA(build0) = ZCL_ABAPPM_SEMVER=>CREATE( '1.0.0+0' ).
    DATA(build1) = ZCL_ABAPPM_SEMVER=>CREATE( '1.0.0+1' ).
    DATA(build10) = ZCL_ABAPPM_SEMVER=>CREATE( '1.0.0+1.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = nobuild->compare_build( '1.0.0+0' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0+0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0+0.0' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build0->compare_build( '1.0.0+1' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build1->compare_build( '1.0.0+0' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = build10->compare_build( '1.0.0+1' )
      exp = +1 ).

  ENDMETHOD.

ENDCLASS.
