CLASS ltcl_tar_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abappm_tar.

    METHODS:
      setup,
      null FOR TESTING,
      filename FOR TESTING RAISING zcx_abappm_error,
      checksum FOR TESTING RAISING zcx_abappm_error,
      octal FOR TESTING,
      pad FOR TESTING,
      unixtime FOR TESTING RAISING zcx_abappm_error,
      xstring FOR TESTING RAISING zcx_abappm_error.

ENDCLASS.

CLASS zcl_abappm_tar DEFINITION LOCAL FRIENDS ltcl_tar_tests.

CLASS ltcl_tar_tests IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_abappm_tar=>new( ).
  ENDMETHOD.

  METHOD null.

    DATA:
      lv_null TYPE c LENGTH 1,
      BEGIN OF ls_data,
        name TYPE c LENGTH 10,
        size TYPE c LENGTH 5,
        mode TYPE c LENGTH 8,
      END OF ls_data.

    lv_null = mo_cut->gv_null(1).

    ls_data-name = 'test.txt'.
    ls_data-size = '12345'.
    ls_data-mode = '01234'.

    mo_cut->_append_nulls( CHANGING cg_data = ls_data ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-name
      exp = 'test.txt' && lv_null && lv_null ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-size
      exp = '12345' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-mode
      exp = '01234' && lv_null && lv_null && lv_null ).

    mo_cut->_remove_nulls( CHANGING cg_data = ls_data ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-name
      exp = 'test.txt' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-size
      exp = '12345' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-mode
      exp = '01234' ).

  ENDMETHOD.

  METHOD filename.

    DATA:
      lv_filename TYPE string,
      lv_prefix   TYPE zcl_abappm_tar=>ty_header-prefix,
      lv_name     TYPE zcl_abappm_tar=>ty_header-name.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_filename( iv_prefix = 'package/modules' iv_name = 'tar.sh' )
      exp = 'package/modules/tar.sh' ).

    " Short filename
    mo_cut->_from_filename(
      EXPORTING
        iv_filename = 'package/modules/tar.sh'
      IMPORTING
        ev_prefix   = lv_prefix
        ev_name     = lv_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_prefix
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'package/modules/tar.sh' ).

    " Long filename
    lv_filename = 'package/node_modules/node-gyp/node_modules/path-array/' &&
      'node_modules/array-index/node_modules/es6-symbol/case-insensitive-compare.js'.

    mo_cut->_from_filename(
      EXPORTING
        iv_filename = lv_filename
      IMPORTING
        ev_prefix   = lv_prefix
        ev_name     = lv_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_prefix
      exp = 'package/node_modules/node-gyp' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_prefix && '/' && lv_name
      exp = lv_filename ).

  ENDMETHOD.

  METHOD checksum.

    CONSTANTS lc_data TYPE string VALUE 'abc 123'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_checksum( lc_data )
      exp = 476 ).

  ENDMETHOD.

  METHOD octal.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 0 )
      exp = '0' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 143 )
      exp = '217' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 4565 )
      exp = '10725' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_octal( 498112 )
      exp = '1714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '10725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_octal( '1714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD pad.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 0 iv_length = 4 )
      exp = '0000' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 143 iv_length = 8 )
      exp = '00000217' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 4565 iv_length = 8 )
      exp = '00010725' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_pad( iv_number = 498112 iv_length = 12 )
      exp = '000001714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '0000' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '00000217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '00010725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_unpad( '000001714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD unixtime.

    DATA:
      lv_date TYPE d,
      lv_time TYPE t.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_unixtime( iv_date = '20221126' iv_time = '123456' )
      exp = 1669466096 ).

    mo_cut->_from_unixtime(
      EXPORTING
        iv_unixtime = 1669466096
      IMPORTING
        ev_date     = lv_date
        ev_time     = lv_time ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_date
      exp = '20221126' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_time
      exp = '123456' ).

  ENDMETHOD.

  METHOD xstring.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_to_xstring( 'abc 123 -' )
      exp = '61626320313233202D' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_from_xstring( '61626320313233202D' )
      exp = 'abc 123 -' ).

  ENDMETHOD.

ENDCLASS.
