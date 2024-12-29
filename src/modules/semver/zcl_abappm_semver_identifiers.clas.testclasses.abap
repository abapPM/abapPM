CLASS ltcl_semver_identifiers DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abappm_semver_identifiers.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_semver_identifiers IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test.
    " rcompareIdentifiers and compareIdentifiers

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->compare_identifiers( a = '1' b = '2' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->rcompare_identifiers( a = '1' b = '2' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->compare_identifiers( a = 'alpha' b = 'beta' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->rcompare_identifiers( a = 'alpha' b = 'beta' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->compare_identifiers( a = '0' b = 'beta' )
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->rcompare_identifiers( a = '0' b = 'beta' )
      exp = +1 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->compare_identifiers( a = '0' b = '0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->rcompare_identifiers( a = '0' b = '0' )
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
