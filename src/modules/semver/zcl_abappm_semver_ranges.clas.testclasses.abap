CLASS ltcl_semver_ranges DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      gtr FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      intersects FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      ltr FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      max_satisfying FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      min_satisfying FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      min_version FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      outside FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      simplify FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      subset FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      to_comparators FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      valid_range FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR.

ENDCLASS.

CLASS ltcl_semver_ranges IMPLEMENTATION.

  METHOD gtr.

    " Version should be greater than range
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_GT_RANGE( ) INTO DATA(version_gt_range).
      DATA(msg) = |{ version_gt_range-range } { version_gt_range-version } { version_gt_range-loose }|.
      DATA(act) = ZCL_ABAPPM_SEMVER_RANGES=>GTR(
        range   = version_gt_range-range
        version = version_gt_range-version
        loose   = version_gt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    " Version should not be greater than range
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_NOT_GT_RANGE( ) INTO DATA(version_not_gt_range).
      msg = |{ version_not_gt_range-range } { version_not_gt_range-version } |
         && |{ version_not_gt_range-loose } { version_not_gt_range-incpre }|.
      act = ZCL_ABAPPM_SEMVER_RANGES=>GTR(
        range   = version_not_gt_range-range
        version = version_not_gt_range-version
        loose   = version_not_gt_range-loose
        incpre  = version_not_gt_range-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD intersects.

    " Intersect comparators
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>COMPARATOR_INTERSECTION( ) INTO DATA(comparator_intersection).
      DATA(msg) = |{ comparator_intersection-c0 } { comparator_intersection-c1 } { comparator_intersection-res }|.

      DATA(comp0) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( comparator_intersection-c0 ).
      DATA(comp1) = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( comparator_intersection-c1 ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comp0 r2 = comp1 )
        exp = comparator_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comp1 r2 = comp0 )
        exp = comparator_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comp0 r2 = comp1 loose = abap_true )
        exp = comparator_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comp1 r2 = comp0 loose = abap_true )
        exp = comparator_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comparator_intersection-c0 r2 = comparator_intersection-c1 )
        exp = comparator_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comparator_intersection-c1 r2 = comparator_intersection-c0 )
        exp = comparator_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comparator_intersection-c0 r2 = comparator_intersection-c1 loose = abap_true )
        exp = comparator_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = comparator_intersection-c1 r2 = comparator_intersection-c0 loose = abap_true )
        exp = comparator_intersection-res
        msg = msg ).
    ENDLOOP.

    " Ranges intersect
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>RANGE_INTERSECTION( ) INTO DATA(range_intersection).
      msg = |{ range_intersection-r0 } { range_intersection-r1 } { range_intersection-res }|.

      DATA(range0) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range_intersection-r0 ).
      DATA(range1) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( range_intersection-r1 ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range0 r2 = range1 )
        exp = range_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range1 r2 = range0 )
        exp = range_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range0 r2 = range1 loose = abap_true )
        exp = range_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range1 r2 = range0 loose = abap_true )
        exp = range_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range_intersection-r0 r2 = range_intersection-r1 )
        exp = range_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range_intersection-r1 r2 = range_intersection-r0 )
        exp = range_intersection-res
        msg = msg ).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range_intersection-r0 r2 = range_intersection-r1 loose = abap_true )
        exp = range_intersection-res
        msg = msg ).
      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>INTERSECTS( r1 = range_intersection-r1 r2 = range_intersection-r0 loose = abap_true )
        exp = range_intersection-res
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD ltr.

    " Version should be less than range
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_LT_RANGE( ) INTO DATA(version_lt_range).
      DATA(msg) = |{ version_lt_range-range } { version_lt_range-version } { version_lt_range-loose }|.
      DATA(act) = ZCL_ABAPPM_SEMVER_RANGES=>LTR(
        range   = version_lt_range-range
        version = version_lt_range-version
        loose   = version_lt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    " Version not should be less than range
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_NOT_LT_RANGE( ) INTO DATA(version_not_lt_range).
      msg = |{ version_not_lt_range-range } { version_not_lt_range-version } |
         && |{ version_not_lt_range-loose } { version_not_lt_range-incpre }|.
      act = ZCL_ABAPPM_SEMVER_RANGES=>LTR(
        range   = version_not_lt_range-range
        version = version_not_lt_range-version
        loose   = version_not_lt_range-loose
        incpre  = version_not_lt_range-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_false
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

  METHOD max_satisfying.

    TYPES:
      BEGIN OF ty_test,
        versions TYPE string,
        range    TYPE string,
        res      TYPE string,
        loose    TYPE abap_bool,
      END OF ty_test,
      ty_tests TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY.

    DATA(tests) = VALUE ty_tests(
       ( versions = '1.2.3 1.2.4' range = '1.2' res = '1.2.4' )
       ( versions = '1.2.4 1.2.3' range = '1.2' res = '1.2.4' )
       ( versions = '1.2.3 1.2.4 1.2.5 1.2.6' range = '~1.2.3' res = '1.2.6' )
       ( versions = '1.1.0 1.2.0 1.2.1 1.3.0 2.0.0b1 2.0.0b2 2.0.0b3 2.0.0 2.1.0'
         range = '~2.0.0' res = '2.0.0' loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      SPLIT test-versions AT ` ` INTO TABLE DATA(versions).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>MAX_SATISFYING( versions = versions range = test-range loose = test-loose )
        exp = test-res
        msg = |{ test-versions } { test-range } { test-res }| ).
    ENDLOOP.

    " bad ranges in max satisfying
    CLEAR versions.

    DATA(range) = 'some frogs and sneks-v2.5.6'.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>MAX_SATISFYING( versions = versions range = range )
      exp = ''
      msg = |{ range }| ).

  ENDMETHOD.

  METHOD min_satisfying.

    TYPES:
      BEGIN OF ty_test,
        versions TYPE string,
        range    TYPE string,
        res      TYPE string,
        loose    TYPE abap_bool,
      END OF ty_test,
      ty_tests TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY.

    DATA(tests) = VALUE ty_tests(
       ( versions = '1.2.3 1.2.4' range = '1.2' res = '1.2.3' )
       ( versions = '1.2.4 1.2.3' range = '1.2' res = '1.2.3' )
       ( versions = '1.2.3 1.2.4 1.2.5 1.2.6' range = '~1.2.3' res = '1.2.3' )
       ( versions = '1.1.0 1.2.0 1.2.1 1.3.0 2.0.0b1 2.0.0b2 2.0.0b3 2.0.0 2.1.0'
         range = '~2.0.0' res = '2.0.0' loose = abap_true ) ).

    LOOP AT tests INTO DATA(test).
      SPLIT test-versions AT ` ` INTO TABLE DATA(versions).

      cl_abap_unit_assert=>assert_equals(
        act = ZCL_ABAPPM_SEMVER_RANGES=>MIN_SATISFYING( versions = versions range = test-range loose = test-loose )
        exp = test-res
        msg = |{ test-versions } { test-range } { test-res }| ).
    ENDLOOP.

    " bad ranges in min satisfying
    CLEAR versions.

    DATA(range) = 'some frogs and sneks-v2.5.6'.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>MIN_SATISFYING( versions = versions range = range )
      exp = ''
      msg = |{ range }| ).

  ENDMETHOD.

  METHOD min_version.

    TYPES:
      BEGIN OF ty_test,
        range TYPE string,
        min   TYPE string,
        loose TYPE abap_bool,
      END OF ty_test,
      ty_tests TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY.

    DATA(tests) = VALUE ty_tests(
      " Stars
      ( range = '*' min = '0.0.0' )
      ( range = '* || >=2' min = '0.0.0' )
      ( range = '>=2 || *' min = '0.0.0' )
      ( range = '>2 || *' min = '0.0.0' )

      " Equal
      ( range = '1.0.0' min = '1.0.0' )
      ( range = '1.0' min = '1.0.0' )
      ( range = '1.0.x' min = '1.0.0' )
      ( range = '1.0.*' min = '1.0.0' )
      ( range = '1' min = '1.0.0' )
      ( range = '1.x.x' min = '1.0.0' )
      ( range = '1.x.x' min = '1.0.0' )
      ( range = '1.*.x' min = '1.0.0' )
      ( range = '1.x.*' min = '1.0.0' )
      ( range = '1.x' min = '1.0.0' )
      ( range = '1.*' min = '1.0.0' )
      ( range = '=1.0.0' min = '1.0.0' )

      " Tilde
      ( range = '~1.1.1' min = '1.1.1' )
      ( range = '~1.1.1-beta' min = '1.1.1-beta' )
      ( range = '~1.1.1 || >=2' min = '1.1.1' )

      " Carot
      ( range = '^1.1.1' min = '1.1.1' )
      ( range = '^1.1.1-beta' min = '1.1.1-beta' )
      ( range = '^1.1.1 || >=2' min = '1.1.1' )
      ( range = '^2.16.2 ^2.16' min = '2.16.2' )

      " '-' operator
      ( range = '1.1.1 - 1.8.0' min = '1.1.1' )
      ( range = '1.1 - 1.8.0' min = '1.1.0' )

      " Less / less or equal
      ( range = '<2' min = '0.0.0' )
      ( range = '<0.0.0-beta' min = '0.0.0-0' )
      ( range = '<0.0.1-beta' min = '0.0.0' )
      ( range = '<2 || >4' min = '0.0.0' )
      ( range = '>4 || <2' min = '0.0.0' )
      ( range = '<=2 || >=4' min = '0.0.0' )
      ( range = '>=4 || <=2' min = '0.0.0' )
      ( range = '<0.0.0-beta >0.0.0-alpha' min = '0.0.0-alpha.0' )
      ( range = '>0.0.0-alpha <0.0.0-beta' min = '0.0.0-alpha.0' )

      " Greater than or equal
      ( range = '>=1.1.1 <2 || >=2.2.2 <2' min = '1.1.1' )
      ( range = '>=2.2.2 <2 || >=1.1.1 <2' min = '1.1.1' )

      " Greater than but not equal
      ( range = '>1.0.0' min = '1.0.1' )
      ( range = '>1.0.0-0' min = '1.0.0-0.0' )
      ( range = '>1.0.0-beta' min = '1.0.0-beta.0' )
      ( range = '>2 || >1.0.0' min = '1.0.1' )
      ( range = '>2 || >1.0.0-0' min = '1.0.0-0.0' )
      ( range = '>2 || >1.0.0-beta' min = '1.0.0-beta.0' )

      " Impossible range
      ( range = '>4 <3' min = '' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(act) = ZCL_ABAPPM_SEMVER_RANGES=>MIN_VERSION( range = test-range loose = test-loose ).

      IF test-min IS INITIAL.
        cl_abap_unit_assert=>assert_not_bound(
          act = act
          msg = |{ test-range }| ).
      ELSE.
        cl_abap_unit_assert=>assert_bound(
          act = act
          msg = |{ test-range } { test-min }| ).

        cl_abap_unit_assert=>assert_equals(
          act = act->version
          exp = test-min
          msg = |{ test-range } { test-min }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD outside.

    " Version should be greater than range
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_GT_RANGE( ) INTO DATA(version_gt_range).
      DATA(msg) = |{ version_gt_range-range } { version_gt_range-version } { version_gt_range-loose }|.
      DATA(act) = ZCL_ABAPPM_SEMVER_RANGES=>OUTSIDE(
        version = version_gt_range-version
        range   = version_gt_range-range
        hilo    = '>'
        loose   = version_gt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    " Version should be less than range
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_LT_RANGE( ) INTO DATA(version_lt_range).
      msg = |{ version_lt_range-range } { version_lt_range-version } { version_lt_range-loose }|.
      act = ZCL_ABAPPM_SEMVER_RANGES=>OUTSIDE(
        version = version_lt_range-version
        range   = version_lt_range-range
        hilo    = '<'
        loose   = version_lt_range-loose ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_true
        msg = msg ).
    ENDLOOP.

    " Negative greater than test
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_NOT_GT_RANGE( ) INTO DATA(version_not_gt_range).
      msg = |{ version_not_gt_range-range } { version_not_gt_range-version } |
         && |{ version_not_gt_range-loose } { version_not_gt_range-incpre }|.
      act = ZCL_ABAPPM_SEMVER_RANGES=>OUTSIDE(
        version = version_not_gt_range-version
        range   = version_not_gt_range-range
        hilo    = '>'
        loose   = version_not_gt_range-loose
        incpre  = version_not_gt_range-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    " Negative less than test
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>VERSION_NOT_LT_RANGE( ) INTO DATA(version_not_lt_range).
      msg = |{ version_not_lt_range-range } { version_not_lt_range-version } |
         && |{ version_not_lt_range-loose } { version_not_lt_range-incpre }|.
      act = ZCL_ABAPPM_SEMVER_RANGES=>OUTSIDE(
        version = version_not_lt_range-version
        range   = version_not_lt_range-range
        hilo    = '<'
        loose   = version_not_lt_range-loose
        incpre  = version_not_lt_range-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = abap_false
        msg = msg ).
    ENDLOOP.

    " outside with bad hilo throws
    TRY.
        act = ZCL_ABAPPM_SEMVER_RANGES=>OUTSIDE(
          version = '1.2.3'
          range   = '>1.5.0'
          hilo    = 'x' ).
        cl_abap_unit_assert=>fail( ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR INTO DATA(error).
        cl_abap_unit_assert=>assert_equals(
          act = error->get_text( )
          exp = 'Must provide a hilo val of "<" or ">"' ).
    ENDTRY.

  ENDMETHOD.

  METHOD simplify.

    DATA(versions) = VALUE string_table(
      ( `1.0.0` )
      ( `1.0.1` )
      ( `1.0.2` )
      ( `1.0.3` )
      ( `1.0.4` )
      ( `1.1.0` )
      ( `1.1.1` )
      ( `1.1.2` )
      ( `1.2.0` )
      ( `1.2.1` )
      ( `1.2.2` )
      ( `1.2.3` )
      ( `1.2.4` )
      ( `1.2.5` )
      ( `2.0.0` )
      ( `2.0.1` )
      ( `2.1.0` )
      ( `2.1.1` )
      ( `2.1.2` )
      ( `2.2.0` )
      ( `2.2.1` )
      ( `2.2.2` )
      ( `2.3.0` )
      ( `2.3.1` )
      ( `2.4.0` )
      ( `3.0.0` )
      ( `3.1.0` )
      ( `3.2.0` )
      ( `3.3.0` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>SIMPLIFY(
              versions = versions
              range    = '1.x' )
      exp = '1.x' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>SIMPLIFY(
              versions = versions
              range    = '1.0.0 || 1.0.1 || 1.0.2 || 1.0.3 || 1.0.4' )
      exp = '<=1.0.4' ).

    DATA(range) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( '1.0.0 || 1.0.1 || 1.0.2 || 1.0.3 || 1.0.4' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>SIMPLIFY(
              versions = versions
              range    = range )
      exp = '<=1.0.4' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>SIMPLIFY(
              versions = versions
              range    = '>=3.0.0 <3.1.0' )
      exp = '3.0.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>SIMPLIFY(
              versions = versions
              range    = '3.0.0 || 3.1 || 3.2 || 3.3' )
      exp = '>=3.0.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>SIMPLIFY(
              versions = versions
              range    = '1 || 2 || 3' )
      exp = '*' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>SIMPLIFY(
              versions = versions
              range    = '2.1 || 2.2 || 2.3' )
      exp = '2.1.0 - 2.3.1' ).

  ENDMETHOD.

  METHOD subset.

    " Method is not implemented yet
    " https://github.com/npm/node-semver/blob/main/test/ranges/subset.js

  ENDMETHOD.

  METHOD to_comparators.

    TYPES:
      BEGIN OF ty_test,
        range TYPE string,
        comps TYPE string,
      END OF ty_test,
      ty_tests TYPE STANDARD TABLE OF ty_test WITH DEFAULT KEY.

    DATA test_act TYPE string.

    DATA(tests) = VALUE ty_tests(
      ( range = '1.0.0 - 2.0.0' comps = '>=1.0.0 <=2.0.0' )
      ( range = '1.0.0' comps = '1.0.0' )
      ( range = '>=*' comps = '' )
      ( range = '' comps = '' )
      ( range = '*' comps = '' )
      ( range = '>=1.0.0' comps = '>=1.0.0' )
      ( range = '>1.0.0' comps = '>1.0.0' )
      ( range = '<=2.0.0' comps = '<=2.0.0' )
      ( range = '1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '<=2.0.0' comps = '<=2.0.0' )
      ( range = '<2.0.0' comps = '<2.0.0' )
      ( range = '>= 1.0.0' comps = '>=1.0.0' )
      ( range = '>=  1.0.0' comps = '>=1.0.0' )
      ( range = '> 1.0.0' comps = '>1.0.0' )
      ( range = '>  1.0.0' comps = '>1.0.0' )
      ( range = '<=  2.0.0' comps = '<=2.0.0' )
      ( range = '<= 2.0.0' comps = '<=2.0.0' )
      ( range = '<    2.0.0' comps = '<2.0.0' )
      ( range = |<\t2.0.0| comps = '<2.0.0' )
      ( range = '>=0.1.97' comps = '>=0.1.97' )
      ( range = '>=0.1.97' comps = '>=0.1.97' )
      ( range = '0.1.20 || 1.2.4' comps = '0.1.20,1.2.4' )
      ( range = '>=0.2.3 || <0.0.1' comps = '>=0.2.3,<0.0.1' )
      ( range = '>=0.2.3 || <0.0.1' comps = '>=0.2.3,<0.0.1' )
      ( range = '>=0.2.3 || <0.0.1' comps = '>=0.2.3,<0.0.1' )
      ( range = '||' comps = '' )
      ( range = '2.x.x' comps = '>=2.0.0 <3.0.0-0' )
      ( range = '1.2.x' comps = '>=1.2.0 <1.3.0-0' )
      ( range = '1.2.x || 2.x' comps = '>=1.2.0 <1.3.0-0,>=2.0.0 <3.0.0-0' )
      ( range = '1.2.x || 2.x' comps = '>=1.2.0 <1.3.0-0,>=2.0.0 <3.0.0-0' )
      ( range = 'x' comps = '' )
      ( range = '2.*.*' comps = '>=2.0.0 <3.0.0-0' )
      ( range = '1.2.*' comps = '>=1.2.0 <1.3.0-0' )
      ( range = '1.2.* || 2.*' comps = '>=1.2.0 <1.3.0-0,>=2.0.0 <3.0.0-0' )
      ( range = '1.2.* || 2.*' comps = '>=1.2.0 <1.3.0-0,>=2.0.0 <3.0.0-0' )
      ( range = '*' comps = '' )
      ( range = '2' comps = '>=2.0.0 <3.0.0-0' )
      ( range = '2.3' comps = '>=2.3.0 <2.4.0-0' )
      ( range = '~2.4' comps = '>=2.4.0 <2.5.0-0' )
      ( range = '~2.4' comps = '>=2.4.0 <2.5.0-0' )
      ( range = '~>3.2.1' comps = '>=3.2.1 <3.3.0-0' )
      ( range = '~1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '~>1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '~> 1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '~1.0' comps = '>=1.0.0 <1.1.0-0' )
      ( range = '~ 1.0' comps = '>=1.0.0 <1.1.0-0' )
      ( range = '~ 1.0.3' comps = '>=1.0.3 <1.1.0-0' )
      ( range = '~> 1.0.3' comps = '>=1.0.3 <1.1.0-0' )
      ( range = '<1' comps = '<1.0.0-0' )
      ( range = '< 1' comps = '<1.0.0-0' )
      ( range = '>=1' comps = '>=1.0.0' )
      ( range = '>= 1' comps = '>=1.0.0' )
      ( range = '<1.2' comps = '<1.2.0-0' )
      ( range = '< 1.2' comps = '<1.2.0-0' )
      ( range = '1' comps = '>=1.0.0 <2.0.0-0' )
      ( range = '1 2' comps = '>=1.0.0 <2.0.0-0 >=2.0.0 <3.0.0-0' )
      ( range = '1.2 - 3.4.5' comps = '>=1.2.0 <=3.4.5' )
      ( range = '1.2.3 - 3.4' comps = '>=1.2.3 <3.5.0-0' )
      ( range = '1.2.3 - 3' comps = '>=1.2.3 <4.0.0-0' )
      ( range = '>*' comps = '<0.0.0-0' )
      ( range = '<*' comps = '<0.0.0-0' )
      ( range = '>X' comps = '<0.0.0-0' )
      ( range = '<X' comps = '<0.0.0-0' )
      ( range = '<x <* || >* 2.x' comps = '<0.0.0-0' )
      ( range = '>x 2.x || * || <x' comps = '' ) ).

    LOOP AT tests INTO DATA(test).
      DATA(comparators) = ZCL_ABAPPM_SEMVER_RANGES=>TO_COMPARATORS( test-range ).

      CLEAR test_act.
      LOOP AT comparators ASSIGNING FIELD-SYMBOL(<comp_list>).
        DATA(comp_list) = concat_lines_of( table = <comp_list> sep = ` ` ).
        IF test_act IS INITIAL.
          test_act = comp_list.
        ELSE.
          test_act = test_act && ',' && comp_list.
        ENDIF.
      ENDLOOP.

      cl_abap_unit_assert=>assert_equals(
        act = test_act
        exp = test-comps
        msg = |{ test-range }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD valid_range.

    " translate ranges into their canonical form
    LOOP AT ZCL_ABAPPM_SEMVER_FIXTURES=>RANGE_PARSE( ) INTO DATA(range_parse).
      DATA(msg) = |{ range_parse-range } { range_parse-res } { range_parse-loose } { range_parse-incpre }|.
      DATA(act) = ZCL_ABAPPM_SEMVER_RANGES=>VALID_RANGE(
        range   = range_parse-range
        loose   = range_parse-loose
        incpre  = range_parse-incpre ).

      cl_abap_unit_assert=>assert_equals(
        act = act
        exp = range_parse-res
        msg = msg ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
