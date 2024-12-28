CLASS ltcl_semver_integration DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA:
      ws_medium  TYPE string,
      ws_large   TYPE string,
      zero_large TYPE string.

    METHODS:
      setup,
      range_with_whitespace FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      range_with_0 FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      semver_version FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR,
      comparator FOR TESTING RAISING ZCX_ABAPPM_SEMVER_ERROR.

ENDCLASS.

CLASS ltcl_semver_integration IMPLEMENTATION.

  METHOD setup.
    ws_medium = repeat(
      val = ` `
      occ = 125 ).
    ws_large = repeat(
      val = ` `
      occ = 500000 ).
    zero_large = repeat(
      val = `0`
      occ = 500000 ).
  ENDMETHOD.

  METHOD range_with_whitespace.

    " a range with these extra characters would take a few minutes to process if
    " any redos susceptible regexes were used (in JavaScript). ABAP does not
    " seem to have this problem but we will include the tests anyway.

    DATA(r) = |1.2.3 { ws_large } <1.3.0|.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( r )->range( )
      exp = '1.2.3 <1.3.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>VALID_RANGE( r )
      exp = '1.2.3 <1.3.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>MIN_VERSION( r )->version
      exp = '1.2.3' ).

    DATA(t) = VALUE string_table( ( `1.2.3` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>MIN_SATISFYING( versions = t range = r )
      exp = '1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>MAX_SATISFYING( versions = t range = r )
      exp = '1.2.3' ).

  ENDMETHOD.

  METHOD range_with_0.

    DATA(r) = |1.2.3 { zero_large } <1.3.0|.

    TRY.
        DATA(range) = ZCL_ABAPPM_SEMVER_RANGE=>CREATE( r ).
        cl_abap_unit_assert=>fail( ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##no_handler.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>VALID_RANGE( r )
      exp = '' ).

    TRY.
        DATA(min) = ZCL_ABAPPM_SEMVER_RANGES=>MIN_VERSION( r ).
        cl_abap_unit_assert=>fail( ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##no_handler.
    ENDTRY.

    DATA(t) = VALUE string_table( ( `1.2.3` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>MIN_SATISFYING( versions = t range = r )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_RANGES=>MAX_SATISFYING( versions = t range = r )
      exp = '' ).

  ENDMETHOD.

  METHOD semver_version.

    DATA(v) = |{ ws_medium }1.2.3{ ws_medium }|.
    DATA(too_long) = |{ ws_large }1.2.3{ ws_large }|.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER=>CREATE( v )->version
      exp = '1.2.3' ).

    TRY.
        DATA(t) = ZCL_ABAPPM_SEMVER=>CREATE( too_long ).
        cl_abap_unit_assert=>fail( ).
      CATCH ZCX_ABAPPM_SEMVER_ERROR ##no_handler.
    ENDTRY.

  ENDMETHOD.

  METHOD comparator.

    DATA(c) = |{ ws_large }<{ ws_large }1.2.3{ ws_large }|.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_COMPARATOR=>CREATE( c )->value
      exp = '<1.2.3' ).

  ENDMETHOD.
ENDCLASS.
