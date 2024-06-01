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
      act = zcl_abappm_semver_utils=>is_numeric( '26111968' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>is_numeric( '+123' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>is_numeric( 'abc' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD trim.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>trim( |  test   | )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>trim( |\t\t test| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>trim( |test\n \t| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>trim( |test| )
      exp = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>trim( |test test| )
      exp = 'test test' ).

  ENDMETHOD.

  METHOD version_trim.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>version_trim( | v 1.2.3| )
      exp = 'v1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_semver_utils=>version_trim( |=  \t 1.2.3 | )
      exp = '=1.2.3' ).

  ENDMETHOD.

ENDCLASS.
