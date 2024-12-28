CLASS ltcl_semver_utils DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      is_numeric FOR TESTING,
      trim FOR TESTING,
      version_trim FOR TESTING.

ENDCLASS.

CLASS ltcl_semver_utils IMPLEMENTATION.

  METHOD is_numeric.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>IS_NUMERIC( '26111968' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>IS_NUMERIC( '+123' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>IS_NUMERIC( 'abc' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD trim.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( |  test   | )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( |\t\t test| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( |test\n \t| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( |test| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>TRIM( |test test| )
      exp = 'test test' ).

  ENDMETHOD.

  METHOD version_trim.

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>VERSION_TRIM( | v 1.2.3| )
      exp = 'v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_SEMVER_UTILS=>VERSION_TRIM( |=  \t 1.2.3 | )
      exp = '=1.2.3' ).

  ENDMETHOD.

ENDCLASS.
