CLASS ltcl_semver_functions DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_test,
        range   TYPE string,
        version TYPE string,
        loose   TYPE abap_bool,
      END OF ty_test,
      ty_tests TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY.

    METHODS:
      clean FOR TESTING RAISING zcx_abappm_semver_error,
      cmp_invalid FOR TESTING RAISING zcx_abappm_semver_error,
      cmp_comparison FOR TESTING RAISING zcx_abappm_semver_error,
      cmp_equality FOR TESTING RAISING zcx_abappm_semver_error,
      coerce_to_null FOR TESTING RAISING zcx_abappm_semver_error,
      coerce_to_valid FOR TESTING RAISING zcx_abappm_semver_error,
      coerce_to_valid_incpre FOR TESTING RAISING zcx_abappm_semver_error,
      coerce_rtl FOR TESTING RAISING zcx_abappm_semver_error,
      coerce_rtl_incpre FOR TESTING RAISING zcx_abappm_semver_error,
      compare FOR TESTING RAISING zcx_abappm_semver_error,
      compare_build FOR TESTING RAISING zcx_abappm_semver_error,
      compare_loose FOR TESTING RAISING zcx_abappm_semver_error,
      diff FOR TESTING RAISING zcx_abappm_semver_error,
      eq FOR TESTING RAISING zcx_abappm_semver_error,
      gt FOR TESTING RAISING zcx_abappm_semver_error,
      gte FOR TESTING RAISING zcx_abappm_semver_error,
      inc FOR TESTING RAISING zcx_abappm_semver_error,
      lt FOR TESTING RAISING zcx_abappm_semver_error,
      lte FOR TESTING RAISING zcx_abappm_semver_error,
      major FOR TESTING RAISING zcx_abappm_semver_error,
      minor FOR TESTING RAISING zcx_abappm_semver_error,
      neq FOR TESTING RAISING zcx_abappm_semver_error,
      parse FOR TESTING RAISING zcx_abappm_semver_error,
      patch FOR TESTING RAISING zcx_abappm_semver_error,
      prerelease FOR TESTING RAISING zcx_abappm_semver_error,
      rcompare FOR TESTING RAISING zcx_abappm_semver_error,
      rsort FOR TESTING RAISING zcx_abappm_semver_error,
      satisfies FOR TESTING RAISING zcx_abappm_semver_error,
      sort FOR TESTING RAISING zcx_abappm_semver_error,
      valid FOR TESTING RAISING zcx_abappm_semver_error.

ENDCLASS.

CLASS ltcl_semver_functions IMPLEMENTATION.

  METHOD clean.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.2.3' version = '1.2.3' )
      ( range = ' 1.2.3 ' version = '1.2.3' )
      ( range = ' 1.2.3-4 ' version = '1.2.3-4' )
      ( range = ' 1.2.3-pre ' version = '1.2.3-pre' )
      ( range = '  =v1.2.3   ' version = '1.2.3' )
      ( range = 'v1.2.3' version = '1.2.3' )
      ( range = ' v1.2.3 ' version = '1.2.3' )
      ( range = |\t1.2.3| version = '1.2.3' )
      ( range = '>1.2.3' version = '' )
      ( range = '~1.2.3' version = '' )
      ( range = '<=1.2.3' version = '' )
      ( range = '1.2.x' version = '' ) ).

    LOOP AT tests INTO DATA(test).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>clean( test-range )
        exp = test-version
        msg = |{ test-range } { test-version }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD cmp_invalid.

    TRY.
        zcl_abappm_semver_functions=>cmp( a = '1.2.3' op = 'a frog' b = '4.5.6' ).
        cl_abap_unit_assert=>fail( msg = 'invalid cmp usage' ).
      CATCH zcx_abappm_semver_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD cmp_comparison.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v0 op = '>' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '>' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v1 op = '<' b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg && '<' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v1 op = '>' b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg && '>' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v0 op = '<' b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg && '<' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v1 op = '==' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '==' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v0 op = '>=' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '>=' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v1 op = '<=' b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg && '<=' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = comparison-v0 op = '!=' b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg && '!=' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD cmp_equality.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      DATA(msg) = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = equality-v0 op = '' b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg && 'nop' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = equality-v0 op = '=' b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg && '=' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = equality-v0 op = '==' b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg && '==' ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = equality-v0 op = '!=' b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg && '!=' ).

      " also test with an object. they are === because obj.version matches
      DATA(semver_v0) = zcl_abappm_semver=>create( version = equality-v0 loose = equality-loose ).
      DATA(semver_v1) = zcl_abappm_semver=>create( version = equality-v1 loose = equality-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = semver_v0 op = '===' b = semver_v1 loose = equality-loose )
        exp = abap_true
        msg = msg && '===' ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>cmp( a = semver_v0 op = '!==' b = semver_v1 loose = equality-loose )
        exp = abap_false
        msg = msg && '!==' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD coerce_to_null.

    DATA(mx) = zif_abappm_semver_constants=>max_safe_component_length.
    DATA(my) = mx + 1. " make it too long

    DATA(tests) = VALUE string_table(
      ( `` )
      ( `.` )
      ( `version one` )
      ( |{ repeat( val = '9' occ = mx ) }| )
      ( |{ repeat( val = '1' occ = my ) }| )
      ( |a{ repeat( val = '9' occ = mx ) }| )
      ( |a{ repeat( val = '1' occ = my ) }| )
      ( |{ repeat( val = '9' occ = mx ) }a| )
      ( |{ repeat( val = '1' occ = my ) }a| )
      ( |{ repeat( val = '9' occ = mx ) }.4.7.4| )
      ( |{ repeat( val = '9' occ = mx ) }.{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }| )    " JS: 9.2.3
      ( |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '9' occ = mx ) }.{ repeat( val = '1' occ = mx ) }| )    " JS: 1.9.3
      ( |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }.{ repeat( val = '9' occ = mx ) }| ) ). " JS: 1.2.9

    LOOP AT tests INTO DATA(test).
      DATA(semver) = zcl_abappm_semver_functions=>coerce( test ).

      cl_abap_unit_assert=>assert_not_bound(
        act = semver
        msg = |{ test }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD coerce_to_valid.

    TYPES:
      BEGIN OF ty_test,
        version TYPE string,
        res     TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    DATA(mx) = zif_abappm_semver_constants=>max_safe_component_length.
    DATA(my) = mx + 1. " make it too long

    tests = VALUE #(
      ( version = '1.2.3.4.5.6' res = '1.2.3' )
      ( version = '.1' res = '1.0.0' )
      ( version = '.1.' res = '1.0.0' )
      ( version = '..1' res = '1.0.0' )
      ( version = '.1.1' res = '1.1.0' )
      ( version = '1.' res = '1.0.0' )
      ( version = '1.0' res = '1.0.0' )
      ( version = '1.0.0' res = '1.0.0' )
      ( version = '0' res = '0.0.0' )
      ( version = '0.0' res = '0.0.0' )
      ( version = '0.0.0' res = '0.0.0' )
      ( version = '0.1' res = '0.1.0' )
      ( version = '0.0.1' res = '0.0.1' )
      ( version = '0.1.1' res = '0.1.1' )
      ( version = '1' res = '1.0.0' )
      ( version = '1.2' res = '1.2.0' )
      ( version = '1.2.3' res = '1.2.3' )
      ( version = '1.2.3.4' res = '1.2.3' )
      ( version = '13' res = '13.0.0' )
      ( version = '35.12' res = '35.12.0' )
      ( version = '35.12.18' res = '35.12.18' )
      ( version = '35.12.18.24' res = '35.12.18' )
      ( version = 'v1' res = '1.0.0' )
      ( version = 'v1.2' res = '1.2.0' )
      ( version = 'v1.2.3' res = '1.2.3' )
      ( version = 'v1.2.3.4' res = '1.2.3' )
      ( version = ' 1' res = '1.0.0' )
      ( version = '1 ' res = '1.0.0' )
      ( version = '1 0' res = '1.0.0' )
      ( version = '1 1' res = '1.0.0' )
      ( version = '1.1 1' res = '1.1.0' )
      ( version = '1.1-1' res = '1.1.0' )
      ( version = '1.1-1' res = '1.1.0' )
      ( version = 'a1' res = '1.0.0' )
      ( version = 'a1a' res = '1.0.0' )
      ( version = '1a' res = '1.0.0' )
      ( version = 'version 1' res = '1.0.0' )
      ( version = 'version1' res = '1.0.0' )
      ( version = 'version1.0' res = '1.0.0' )
      ( version = 'version1.1' res = '1.1.0' )
      ( version = '42.6.7.9.3-alpha' res = '42.6.7' )
      ( version = 'v2' res = '2.0.0' )
      ( version = 'v3.4 replaces v3.3.1' res = '3.4.0' )
      ( version = '4.6.3.9.2-alpha2' res = '4.6.3' )
      ( version = |{ repeat( val = '1' occ = my ) }.2| res = '2.0.0' )
      ( version = |{ repeat( val = '1' occ = my ) }.2.3| res = '2.3.0' )
      ( version = |1.{ repeat( val = '2' occ = my ) }.3| res = '1.0.0' )
      ( version = |1.2.{ repeat( val = '3' occ = my ) }| res = '1.2.0' )
      ( version = |{ repeat( val = '1' occ = my ) }.2.3.4| res = '2.3.4' )
      ( version = |1.{ repeat( val = '2' occ = my ) }.3.4| res = '1.0.0' )
      ( version = |1.2.{ repeat( val = '3' occ = my ) }.4| res = '1.2.0' )
      ( version = |{ repeat( val = '1' occ = my ) }.{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }|
        res = |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }.0| ) " JS: 1.2.3 2.3.0
      ( version = |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '2' occ = my ) }.{ repeat( val = '1' occ = mx ) }|
        res = |{ repeat( val = '1' occ = mx ) }.0.0| ) " JS: 1.2.3 1.0.0
      ( version = |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }.{ repeat( val = '3' occ = my ) }|
        res = |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }.0| )  " JS: 1.2.3 1.2.0
      ( version = |11{ repeat( val = '.1' occ = 126 ) }| res = '11.1.1' )
      ( version = repeat( val = '1' occ = mx ) res = |{ repeat( val = '1' occ = mx ) }.0.0| )
      ( version = |a{ repeat( val = '1' occ = mx ) }| res = |{ repeat( val = '1' occ = mx ) }.0.0| )
      ( version = |{ repeat( val = '1' occ = mx ) }.2.3.4| res = |{ repeat( val = '1' occ = mx ) }.2.3| )
      ( version = |1.{ repeat( val = '1' occ = mx ) }.3.4| res = |1.{ repeat( val = '1' occ = mx ) }.3| ) " JS 1.2.3.4 1.2.3
      ( version = |1.2.{ repeat( val = '1' occ = mx ) }.4| res = |1.2.{ repeat( val = '1' occ = mx ) }| ) " JS 1.2.3.4 1.2.3
      ( version = |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }|
        res = |{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }.{ repeat( val = '1' occ = mx ) }| ) " JS 1.2.3 1.2.3
      ( version = |1.2.3.{ repeat( val = '4' occ = 252 ) }.5| res = '1.2.3' )
      ( version = |1.2.3.{ repeat( val = '4' occ = 1024 ) }| res = '1.2.3' )
      ( version = |{ repeat( val = '1' occ = my ) }.4.7.4| res = '4.7.4' )
      ( version = 10 res = '10.0.0' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(semver) = zcl_abappm_semver_functions=>coerce( test-version ).
      DATA(expected) = zcl_abappm_semver_functions=>parse( test-res ).

      cl_abap_unit_assert=>assert_bound(
        act = semver
        msg = |{ test-version } { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare( semver )
        exp = 0
        msg = |{ test-version } should be equal to { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare_build( semver )
        exp = 0
        msg = |{ test-version } build should be equal to { test-res }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD coerce_to_valid_incpre.

    TYPES:
      BEGIN OF ty_test,
        version TYPE string,
        res     TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( version = '1-rc.5' res = '1.0.0-rc.5' )
      ( version = '1.2-rc.5' res = '1.2.0-rc.5' )
      ( version = '1.2.3-rc.5' res = '1.2.3-rc.5' )
      ( version = '1.2.3-rc.5/a' res = '1.2.3-rc.5' )
      ( version = '1.2.3.4-rc.5' res = '1.2.3' )
      ( version = '1.2.3.4+rev.6' res = '1.2.3' )
      ( version = '1+rev.6' res = '1.0.0+rev.6' )
      ( version = '1.2+rev.6' res = '1.2.0+rev.6' )
      ( version = '1.2.3+rev.6' res = '1.2.3+rev.6' )
      ( version = '1.2.3+rev.6/a' res = '1.2.3+rev.6' )
      ( version = '1.2.3.4-rc.5' res = '1.2.3' )
      ( version = '1.2.3.4+rev.6' res = '1.2.3' )
      ( version = '1-rc.5+rev.6' res = '1.0.0-rc.5+rev.6' )
      ( version = '1.2-rc.5+rev.6' res = '1.2.0-rc.5+rev.6' )
      ( version = '1.2.3-rc.5+rev.6' res = '1.2.3-rc.5+rev.6' )
      ( version = '1.2.3-rc.5+rev.6/a' res = '1.2.3-rc.5+rev.6' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(semver) = zcl_abappm_semver_functions=>coerce( version = test-version incpre = abap_true ).
      DATA(expected) = zcl_abappm_semver_functions=>parse( version = test-res incpre = abap_true ).

      cl_abap_unit_assert=>assert_bound(
        act = semver
        msg = |{ test-version } { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare( semver )
        exp = 0
        msg = |{ test-version } should be equal to { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare_build( semver )
        exp = 0
        msg = |{ test-version } build should be equal to { test-res }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD coerce_rtl.

    TYPES:
      BEGIN OF ty_test,
        version TYPE string,
        res     TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( version = '1.2.3.4.5.6' res = '4.5.6' )
      ( version = '1.2.3/a/b/c/2.3.4' res = '2.3.4' )
      ( version = '1.2.3.4.5/6' res = '6.0.0' )
      ( version = '1.2.3.4./6' res = '6.0.0' )
      ( version = '1.2.3.4/6' res = '6.0.0' )
      ( version = '1.2.3./6' res = '6.0.0' )
      ( version = '1.2.3/6' res = '6.0.0' )
      ( version = '1.2.3.4' res = '2.3.4' )
      ( version = '1.2.3.4xyz' res = '2.3.4' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(semver) = zcl_abappm_semver_functions=>coerce( version = test-version rtl = abap_true ).
      DATA(expected) = zcl_abappm_semver_functions=>parse( version = test-res ).

      cl_abap_unit_assert=>assert_bound(
        act = semver
        msg = |{ test-version } { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare( semver )
        exp = 0
        msg = |{ test-version } should be equal to { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare_build( semver )
        exp = 0
        msg = |{ test-version } build should be equal to { test-res }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD coerce_rtl_incpre.

    TYPES:
      BEGIN OF ty_test,
        version TYPE string,
        res     TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( version = '1.2-rc.5+rev.6' res = '1.2.0-rc.5+rev.6' )
      ( version = '1.2.3-rc.5+rev.6' res = '1.2.3-rc.5+rev.6' )
      ( version = '1.2.3.4-rc.5+rev.6' res = '2.3.4-rc.5+rev.6' )
      ( version = '1.2.3.4-rc.5' res = '2.3.4-rc.5' )
      ( version = '1.2.3.4+rev.6' res = '2.3.4+rev.6' )
      ( version = '1.2.3.4-rc.5+rev.6/7' res = '7.0.0' )
      ( version = '1.2.3.4-rc/7.5+rev.6' res = '7.5.0+rev.6' )
      ( version = '1.2.3.4/7-rc.5+rev.6' res = '7.0.0-rc.5+rev.6' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(semver) = zcl_abappm_semver_functions=>coerce( version = test-version rtl = abap_true incpre = abap_true ).
      DATA(expected) = zcl_abappm_semver_functions=>parse( version = test-res incpre = abap_true ).

      cl_abap_unit_assert=>assert_bound(
        act = semver
        msg = |{ test-version } { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare( semver )
        exp = 0
        msg = |{ test-version } should be equal to { test-res }| ).

      cl_abap_unit_assert=>assert_equals(
        act = expected->compare_build( semver )
        exp = 0
        msg = |{ test-version } build should be equal to { test-res }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD compare.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = +1
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = -1
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = 0
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = equality-v0 b = equality-v0 loose = equality-loose )
        exp = 0
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = equality-v1 b = equality-v1 loose = equality-loose )
        exp = 0
        msg = msg ).

      " also test with an object. they are === because obj.version matches
      DATA(semver_v0) = zcl_abappm_semver=>create( version = equality-v0 loose = equality-loose ).
      DATA(semver_v1) = zcl_abappm_semver=>create( version = equality-v1 loose = equality-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare( a = semver_v0 b = semver_v1 loose = equality-loose )
        exp = 0
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD compare_build.

    DATA(nobuild) = '1.0.0'.
    DATA(build0) = '1.0.0+0'.
    DATA(build1) = '1.0.0+1'.
    DATA(build10) = '1.0.0+1.0'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>compare_build( a = nobuild b = build0 )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>compare_build( a = build0 b = build0 )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>compare_build( a = build0 b = nobuild )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>compare_build( a = build0 b = '1.0.0+0.0' )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>compare_build( a = build0 b = build1 )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>compare_build( a = build1 b = build0 )
      exp = +1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>compare_build( a = build10 b = build1 )
      exp = +1 ).

  ENDMETHOD.

  METHOD compare_loose.
    " strict vs loose version numbers

    TYPES:
      BEGIN OF ty_test,
        loose  TYPE string,
        strict TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( loose = '=1.2.3' strict = '1.2.3' )
      ( loose = '01.02.03' strict = '1.2.3' )
      ( loose = '1.2.3-beta.01' strict = '1.2.3-beta.1' )
      ( loose = '   =1.2.3' strict = '1.2.3' )
      ( loose = '1.2.3foo' strict = '1.2.3-foo' ) ).

    LOOP AT tests INTO DATA(test).

      TRY.
          DATA(semver) = zcl_abappm_semver=>create( test-loose ).
          cl_abap_unit_assert=>fail( ).
        CATCH zcx_abappm_semver_error ##NO_HANDLER.
      ENDTRY.

      semver = zcl_abappm_semver=>create( version = test-loose loose = abap_true ).

      cl_abap_unit_assert=>assert_equals(
        act = semver->version
        exp = test-strict ).

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = test-loose b = test-strict loose = abap_true )
        exp = abap_true ).

      TRY.
          cl_abap_unit_assert=>assert_equals(
            act = zcl_abappm_semver_functions=>eq( a = test-loose b = test-strict )
            exp = abap_true ).

          cl_abap_unit_assert=>fail( ).
        CATCH zcx_abappm_semver_error ##NO_HANDLER.
      ENDTRY.

      TRY.
          semver = zcl_abappm_semver=>create( test-strict ).
          semver->compare( test-loose ).
          cl_abap_unit_assert=>fail( ).
        CATCH zcx_abappm_semver_error ##NO_HANDLER.
      ENDTRY.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>compare_loose( a = test-loose b = test-strict )
        exp = 0 ).

    ENDLOOP.

  ENDMETHOD.

  METHOD diff.

    TYPES:
      BEGIN OF ty_test,
        v1  TYPE string,
        v2  TYPE string,
        res TYPE string,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( v1 = '1.2.3' v2 = '0.2.3' res = 'major' )
      ( v1 = '0.2.3' v2 = '1.2.3' res = 'major' )
      ( v1 = '1.4.5' v2 = '0.2.3' res = 'major' )
      ( v1 = '1.2.3' v2 = '2.0.0-pre' res = 'premajor' )
      ( v1 = '2.0.0-pre' v2 = '1.2.3' res = 'premajor' )
      ( v1 = '1.2.3' v2 = '1.3.3' res = 'minor' )
      ( v1 = '1.0.1' v2 = '1.1.0-pre' res = 'preminor' )
      ( v1 = '1.2.3' v2 = '1.2.4' res = 'patch' )
      ( v1 = '1.2.3' v2 = '1.2.4-pre' res = 'prepatch' )
      ( v1 = '0.0.1' v2 = '0.0.1-pre' res = 'patch' )
      ( v1 = '0.0.1' v2 = '0.0.1-pre-2' res = 'patch' )
      ( v1 = '1.1.0' v2 = '1.1.0-pre' res = 'minor' )
      ( v1 = '1.1.0-pre-1' v2 = '1.1.0-pre-2' res = 'prerelease' )
      ( v1 = '1.0.0' v2 = '1.0.0' res = '' )
      ( v1 = '1.0.0-1' v2 = '1.0.0-1' res = '' )
      ( v1 = '0.0.2-1' v2 = '0.0.2' res = 'patch' )
      ( v1 = '0.0.2-1' v2 = '0.0.3' res = 'patch' )
      ( v1 = '0.0.2-1' v2 = '0.1.0' res = 'minor' )
      ( v1 = '0.0.2-1' v2 = '1.0.0' res = 'major' )
      ( v1 = '0.1.0-1' v2 = '0.1.0' res = 'minor' )
      ( v1 = '1.0.0-1' v2 = '1.0.0' res = 'major' )
      ( v1 = '1.0.0-1' v2 = '1.1.1' res = 'major' )
      ( v1 = '1.0.0-1' v2 = '2.1.1' res = 'major' )
      ( v1 = '1.0.1-1' v2 = '1.0.1' res = 'patch' )
      ( v1 = '0.0.0-1' v2 = '0.0.0' res = 'major' )
      ( v1 = '1.0.0-1' v2 = '2.0.0' res = 'major' )
      ( v1 = '1.0.0-1' v2 = '2.0.0-1' res = 'premajor' )
      ( v1 = '1.0.0-1' v2 = '1.1.0-1' res = 'preminor' )
      ( v1 = '1.0.0-1' v2 = '1.0.1-1' res = 'prepatch' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-v1 } { test-v2 } { test-res } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>diff( version_1 = test-v1 version_2 = test-v2 )
        exp = test-res
        msg = msg ).
    ENDLOOP.

    " throws on bad version
    TRY.
        zcl_abappm_semver_functions=>diff(
          version_1 = 'bad'
          version_2 = '1.2.3' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error INTO DATA(error).
        cl_abap_unit_assert=>assert_equals(
          act = error->get_text( )
          exp = 'Invalid version: bad' ).
    ENDTRY.

  ENDMETHOD.

  METHOD eq.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = equality-v0 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>eq( a = equality-v1 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD gt.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gt( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gt( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gt( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gt( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gt( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gt( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD gte.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gte( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gte( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gte( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gte( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gte( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>gte( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD inc.

    LOOP AT zcl_abappm_semver_fixtures=>increments( ) INTO DATA(increments).
      DATA(msg) = |{ increments-version } { increments-release } { increments-identifier }|
        && | { increments-identifier_base } { increments-res } |.

      DATA(s) = zcl_abappm_semver_functions=>inc(
                  version         = increments-version
                  release         = increments-release
                  identifier      = increments-identifier
                  identifier_base = increments-identifier_base
                  loose           = increments-loose
                  incpre          = increments-incpre ).

      IF s IS BOUND.
        cl_abap_unit_assert=>assert_equals(
          act = s->version
          exp = increments-res
          msg = msg ).
      ELSE.
        cl_abap_unit_assert=>assert_equals(
          act = ''
          exp = increments-res
          msg = msg ).
      ENDIF.

      DATA(parsed) = zcl_abappm_semver_functions=>parse(
                       version = increments-version
                       loose   = increments-loose
                       incpre  = increments-incpre ).

      DATA(parsed_input) = zcl_abappm_semver_functions=>parse(
                             version = increments-version
                             loose   = increments-loose
                             incpre  = increments-incpre ).

      IF increments-res IS NOT INITIAL.

        parsed->inc(
          release         = increments-release
          identifier      = increments-identifier
          identifier_base = increments-identifier_base ).

        cl_abap_unit_assert=>assert_equals(
          act = parsed->version
          exp = increments-res
          msg = msg && 'version' ).

        DATA(pre_inc) = parsed_input->version.

        zcl_abappm_semver_functions=>inc(
          version         = parsed_input
          release         = increments-release
          identifier      = increments-identifier
          identifier_base = increments-identifier_base
          loose           = increments-loose
          incpre          = increments-incpre ).

        DATA(post_inc) = parsed_input->version.

        cl_abap_unit_assert=>assert_equals(
          act = pre_inc
          exp = post_inc
          msg = msg && 'must not modify its input' ).

      ELSEIF parsed IS NOT INITIAL.

        TRY.
            parsed->inc(
              release         = increments-release
              identifier      = increments-identifier
              identifier_base = increments-identifier_base ).

            cl_abap_unit_assert=>fail( ).
          CATCH zcx_abappm_semver_error ##NO_HANDLER.
        ENDTRY.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD lt.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lt( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lt( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lt( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lt( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lt( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lt( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD lte.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lte( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lte( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lte( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lte( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lte( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>lte( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD major.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.2.3' version = 1 )
      ( range = ' 1.2.3 ' version = 1 )
      ( range = ' 2.2.3-4 ' version = 2 )
      ( range = ' 3.2.3-pre ' version = 3 )
      ( range = 'v5.2.3' version = 5 )
      ( range = ' v8.2.3 ' version = 8 )
      ( range = |\t13.2.3| version = 13 )
      ( range = '=21.2.3' version = 21 loose = abap_true )
      ( range = 'v=34.2.3' version = 34 loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-range } { test-loose } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>major( version = test-range loose = test-loose )
        exp = condense( test-version )
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD minor.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.1.3' version = 1 )
      ( range = ' 1.1.3 ' version = 1 )
      ( range = ' 1.2.3-4 ' version = 2 )
      ( range = ' 1.3.3-pre ' version = 3 )
      ( range = 'v1.5.3' version = 5 )
      ( range = ' v1.8.3 ' version = 8 )
      ( range = |\t1.13.3| version = 13 )
      ( range = '=1.21.3' version = 21 loose = abap_true )
      ( range = 'v=1.34.3' version = 34 loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-range } { test-loose } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>minor( version = test-range loose = test-loose )
        exp = condense( test-version )
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD neq.

    LOOP AT zcl_abappm_semver_fixtures=>comparisons( ) INTO DATA(comparison).
      DATA(msg) = |{ comparison-v0 } { comparison-v1 } { comparison-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = comparison-v0 b = comparison-v1 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = comparison-v1 b = comparison-v0 loose = comparison-loose )
        exp = abap_true
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = comparison-v0 b = comparison-v0 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = comparison-v1 b = comparison-v1 loose = comparison-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>equality( ) INTO DATA(equality).
      msg = |{ equality-v0 } { equality-v1 } { equality-loose } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = equality-v0 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = equality-v1 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = equality-v0 b = equality-v0 loose = equality-loose )
        exp = abap_false
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>neq( a = equality-v1 b = equality-v1 loose = equality-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD parse.
    " returns null instead of throwing when presented with garbage

    LOOP AT zcl_abappm_semver_fixtures=>invalid_versions( ) INTO DATA(invalid_version).
      DATA(msg) = |{ invalid_version-value } { invalid_version-loose } { invalid_version-reason } |.

      cl_abap_unit_assert=>assert_initial(
        act = zcl_abappm_semver_functions=>parse( version = invalid_version-value loose = invalid_version-loose )
        msg = msg ).
    ENDLOOP.

    " throw errors if asked to
    TRY.
        zcl_abappm_semver_functions=>parse(
          version      = 'bad'
          throw_errors = abap_true ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error INTO DATA(error).
        cl_abap_unit_assert=>assert_equals(
          act = error->get_text( )
          exp = 'Invalid version: bad' ).
    ENDTRY.

    TRY.
        DATA(wrong_type) = NEW zcl_abappm_semver_cli( ).

        zcl_abappm_semver_functions=>parse(
          version      = wrong_type
          throw_errors = abap_true ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error INTO error.
        cl_abap_unit_assert=>assert_equals(
          act = substring( val = error->get_text( ) len = 50 )
          exp = 'Invalid version. Must be a string or a semver. Got' ).
    ENDTRY.

    " parse a version into a SemVer object
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>parse( '1.2.3' )->version
      exp = '1.2.3' ).

    DATA(s) = zcl_abappm_semver=>create( '4.5.6' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>parse( s )
      exp = s
      msg = 'return it if it is a SemVer obj' ).

    DATA(l) = zcl_abappm_semver=>create( version = '4.2.0' loose = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>parse( version = '4.2.0' loose = abap_true )->version
      exp = l->version
      msg = 'looseness as an option' ).
  ENDMETHOD.

  METHOD patch.
    " Version should be detectable despite extra characters

    DATA(tests) = VALUE ty_tests(
      ( range = '1.2.1' version = 1 )
      ( range = ' 1.2.1 ' version = 1 )
      ( range = ' 1.2.2-4 ' version = 2 )
      ( range = ' 1.2.3-pre ' version = 3 )
      ( range = 'v1.2.5' version = 5 )
      ( range = ' v1.2.8 ' version = 8 )
      ( range = |\t1.2.13| version = 13 )
      ( range = '=1.2.21' version = 21 loose = abap_true )
      ( range = 'v=1.2.34' version = 34 loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-range } { test-loose } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>patch( version = test-range loose = test-loose )
        exp = condense( test-version )
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD prerelease.

    TYPES:
      BEGIN OF ty_test,
        version TYPE string,
        loose   TYPE abap_bool,
        prerel  TYPE string_table,
      END OF ty_test.

    DATA tests TYPE TABLE OF ty_test.

    tests = VALUE #(
      ( version = '1.2.2-alpha.1' prerel = VALUE #( ( `alpha` ) ( `1` ) ) )
      ( version = '0.6.1-1' prerel = VALUE #( ( `1` ) ) )
      ( version = '1.0.0-beta.2' prerel = VALUE #( ( `beta` ) ( `2` ) ) )
      ( version = 'v0.5.4-pre' prerel = VALUE #( ( `pre` ) ) )
      ( version = '1.2.2-alpha.1' prerel = VALUE #( ( `alpha` ) ( `1` ) ) loose = abap_false )
      ( version = '0.6.1beta' prerel = VALUE #( ( `beta` ) ) loose = abap_true )
      ( version = '1.0.0' loose = abap_true )
      ( version = '~2.0.0-alpha.1'  loose = abap_false )
      ( version = 'invalid version' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(msg) = |{ test-version } { test-loose } { lines( test-prerel ) } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>prerelease( version = test-version loose = test-loose )
        exp = test-prerel
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD rcompare.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>rcompare( a = '1.0.0' b = '1.0.1' )
      exp = +1 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>rcompare( a = '1.0.0' b = '1.0.0' )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>rcompare( a = '1.0.0+0' b = '1.0.0' )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>rcompare( a = '1.0.1' b = '1.0.0' )
      exp = -1 ).

  ENDMETHOD.

  METHOD rsort.

    DATA(list) = VALUE string_table(
      ( `1.2.3+1` )
      ( `1.2.3+0` )
      ( `1.2.3` )
      ( `5.9.6` )
      ( `0.1.2` ) ).

    DATA(rsorted) = VALUE string_table(
      ( `5.9.6` )
      ( `1.2.3+1` )
      ( `1.2.3+0` )
      ( `1.2.3` )
      ( `0.1.2` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>rsort( list )
      exp = rsorted ).

  ENDMETHOD.

  METHOD satisfies.

    DATA tests TYPE zcl_abappm_semver_fixtures=>ty_ranges.

    LOOP AT zcl_abappm_semver_fixtures=>range_include( ) INTO DATA(range_include).
      DATA(msg) = |{ range_include-range } { range_include-version } { range_include-loose } { range_include-incpre } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>satisfies(
                 version = range_include-version
                 range   = range_include-range
                 loose   = range_include-loose
                 incpre  = range_include-incpre )
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    LOOP AT zcl_abappm_semver_fixtures=>range_exclude( ) INTO DATA(range_exclude).
      msg = |{ range_exclude-range } { range_exclude-version } { range_exclude-loose } { range_exclude-incpre } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>satisfies(
                 version = range_exclude-version
                 range   = range_exclude-range
                 loose   = range_exclude-loose
                 incpre  = range_exclude-incpre )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    " invalid ranges never satisfied (but do not throw)
    tests = VALUE #(
      ( range = 'blerg' version = '1.2.3' )
      ( range = 'git+https://user:password0123@github.com/foo' version = '123.0.0' loose = abap_true )
      ( range = '^1.2.3' version = '2.0.0-pre' )
      ( range = '0.x' version = '' )
      ( range = '*' version = '' ) ).

    LOOP AT tests INTO DATA(test).
      msg = |{ test-range } { test-version } |.

      cl_abap_unit_assert=>assert_equals(
        act = zcl_abappm_semver_functions=>satisfies( version = test-version range = test-range loose = test-loose )
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD sort.

    DATA(list) = VALUE string_table(
      ( `1.2.3+1` )
      ( `1.2.3+0` )
      ( `1.2.3` )
      ( `5.9.6` )
      ( `0.1.2` ) ).

    DATA(sorted) = VALUE string_table(
      ( `0.1.2` )
      ( `1.2.3` )
      ( `1.2.3+0` )
      ( `1.2.3+1` )
      ( `5.9.6` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>sort( list )
      exp = sorted ).

  ENDMETHOD.

  METHOD valid.
    " returns null instead of throwing when presented with garbage

    LOOP AT zcl_abappm_semver_fixtures=>invalid_versions( ) INTO DATA(invalid_version).
      DATA(msg) = |{ invalid_version-value } { invalid_version-loose } { invalid_version-reason } |.

      cl_abap_unit_assert=>assert_initial(
        act = zcl_abappm_semver_functions=>valid( version = invalid_version-value loose = invalid_version-loose )
        msg = msg ).
    ENDLOOP.

    " validate a version into a SemVer object
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>valid( '1.2.3' )
      exp = '1.2.3' ).

    DATA(s) = zcl_abappm_semver=>create( '4.5.6' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>valid( s )
      exp = '4.5.6'
      msg = 'return the version if a SemVer obj' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>valid( version = '4.2.0foo' loose = abap_true )
      exp = '4.2.0-foo'
      msg = 'looseness as an option' ).

    DATA(mx) = zif_abappm_semver_constants=>max_safe_integer.

    DATA(long_build)    = '-928490632884417731e7af463c92b034d6a78268fc993bcb88a57944'.
    DATA(short_version) = '1.1.1'.
    DATA(long_version)  = |{ mx }.{ mx }.{ mx }|.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>valid( short_version && long_build )
      exp = short_version && long_build ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_functions=>valid( long_version && long_build )
      exp = long_version && long_build ).

  ENDMETHOD.

ENDCLASS.
