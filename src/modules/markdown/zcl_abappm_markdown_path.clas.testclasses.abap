CLASS ltcl_path_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abappm_markdown_path.

    METHODS:
      setup,
      test_01 FOR TESTING,
      test_02 FOR TESTING,
      test_03 FOR TESTING,
      test_04 FOR TESTING,
      test_05 FOR TESTING,
      test_06 FOR TESTING,
      test_07 FOR TESTING,
      test_08 FOR TESTING,
      test_09 FOR TESTING,
      test_10 FOR TESTING,
      test_11 FOR TESTING,
      test_12 FOR TESTING,
      test_13 FOR TESTING,
      test_14 FOR TESTING,
      test_15 FOR TESTING,
      test_16 FOR TESTING,
      test_17 FOR TESTING,
      test_18 FOR TESTING,
      test_19 FOR TESTING.

ENDCLASS.

CLASS ltcl_path_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_01.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( './fixtures///b/../b/c.js' )
      exp = 'fixtures/b/c.js' ).
  ENDMETHOD.

  METHOD test_02.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '/foo/../../../bar' )
      exp = '/bar' ).
  ENDMETHOD.

  METHOD test_03.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'a//b//../b' )
      exp = 'a/b' ).
  ENDMETHOD.

  METHOD test_04.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'a//b//./c' )
      exp = 'a/b/c' ).
  ENDMETHOD.

  METHOD test_05.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'a//b//.' )
      exp = 'a/b' ).
  ENDMETHOD.

  METHOD test_06.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '/a/b/c/../../../x/y/z' )
      exp = '/x/y/z' ).
  ENDMETHOD.

  METHOD test_07.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '///..//./foo/.//bar' )
      exp = '/foo/bar' ).
  ENDMETHOD.

  METHOD test_08.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'bar/foo../../' )
      exp = 'bar/' ).
  ENDMETHOD.

  METHOD test_09.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'bar/foo../..' )
      exp = 'bar' ).
  ENDMETHOD.

  METHOD test_10.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'bar/foo../../baz' )
      exp = 'bar/baz' ).
  ENDMETHOD.

  METHOD test_11.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'bar/foo../' )
      exp = 'bar/foo../' ).
  ENDMETHOD.

  METHOD test_12.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( 'bar/foo..' )
      exp = 'bar/foo..' ).
  ENDMETHOD.

  METHOD test_13.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '../foo../../../bar' )
      exp = '../../bar' ).
  ENDMETHOD.

  METHOD test_14.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '../../.././../../../bar' )
      exp = '../../../../../../bar' ).
  ENDMETHOD.

  METHOD test_15.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '../../../foo/../../../bar' )
      exp = '../../../../../bar' ).
  ENDMETHOD.

  METHOD test_16.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '../../../foo/../../../bar/../../' )
      exp = '../../../../../../' ).
  ENDMETHOD.

  METHOD test_17.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '../foobar/barfoo/foo/../../../bar/../../' )
      exp = '../../' ).
  ENDMETHOD.

  METHOD test_18.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '../../../foobar/../../../bar/../../baz' )
      exp = '../../../../../../baz' ).
  ENDMETHOD.

  METHOD test_19.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->normalize( '/../../../foobar/../../../bar/../../baz' )
      exp = '/baz' ).
  ENDMETHOD.

ENDCLASS.
