CLASS ltcl_semver_integration DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA:
      ws_medium  TYPE string,
      ws_large   TYPE string,
      zero_large TYPE string.

    METHODS:
      setup,
      range_with_whitespace FOR TESTING RAISING zcx_abappm_semver_error,
      range_with_0 FOR TESTING RAISING zcx_abappm_semver_error,
      semver_version FOR TESTING RAISING zcx_abappm_semver_error,
      comparator FOR TESTING RAISING zcx_abappm_semver_error.

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
      act = zcl_abappm_semver_range=>create( r )->range
      exp = '1.2.3 <1.3.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_ranges=>valid_range( r )
      exp = '1.2.3 <1.3.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_ranges=>min_version( r )->version
      exp = '1.2.3' ).

    DATA(t) = VALUE string_table( ( `1.2.3` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_ranges=>min_satisfying( versions = t range = r )
      exp = '1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_ranges=>max_satisfying( versions = t range = r )
      exp = '1.2.3' ).

  ENDMETHOD.

  METHOD range_with_0.

    DATA(r) = |1.2.3 { zero_large } <1.3.0|.

    TRY.
        DATA(range) = zcl_abappm_semver_range=>create( r ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error ##no_handler.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_ranges=>valid_range( r )
      exp = '' ).

    TRY.
        DATA(min) = zcl_abappm_semver_ranges=>min_version( r ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error ##no_handler.
    ENDTRY.

    DATA(t) = VALUE string_table( ( `1.2.3` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_ranges=>min_satisfying( versions = t range = r )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_ranges=>max_satisfying( versions = t range = r )
      exp = '' ).

  ENDMETHOD.

  METHOD semver_version.

    DATA(v) = |{ ws_medium }1.2.3{ ws_medium }|.
    DATA(too_long) = |{ ws_large }1.2.3{ ws_large }|.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver=>create( v )->version
      exp = '1.2.3' ).

    TRY.
        DATA(t) = zcl_abappm_semver=>create( too_long ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abappm_semver_error ##no_handler.
    ENDTRY.

  ENDMETHOD.

  METHOD comparator.

    DATA(c) = |{ ws_large }<{ ws_large }1.2.3{ ws_large }|.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_comparator=>create( c )->value
      exp = '<1.2.3' ).

  ENDMETHOD.
ENDCLASS.
