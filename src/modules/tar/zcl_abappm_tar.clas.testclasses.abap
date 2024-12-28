CLASS ltcl_tar_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ZCL_ABAPPM_TAR.

    " TODO:
    " So far tests cover only helper methods but it needs tests to validated the tar methods
    " We know it works based on tests using program Z_TAR_TESTER and tar files
    " created with unix tar binary :-)
    METHODS:
      setup,
      null FOR TESTING,
      filename FOR TESTING RAISING ZCX_ABAPPM_ERROR,
      checksum FOR TESTING RAISING ZCX_ABAPPM_ERROR,
      octal FOR TESTING,
      pad FOR TESTING,
      unixtime FOR TESTING RAISING ZCX_ABAPPM_ERROR,
      xstring FOR TESTING RAISING ZCX_ABAPPM_ERROR.

ENDCLASS.

CLASS ZCL_ABAPPM_TAR DEFINITION LOCAL FRIENDS ltcl_tar_tests.

CLASS ltcl_tar_tests IMPLEMENTATION.

  METHOD setup.
    cut = ZCL_ABAPPM_TAR=>NEW( ).
  ENDMETHOD.

  METHOD null.

    DATA:
      null TYPE c LENGTH 1,
      BEGIN OF ls_data,
        name TYPE c LENGTH 10,
        size TYPE c LENGTH 5,
        mode TYPE c LENGTH 8,
      END OF ls_data.

    null = cut->null(1).

    ls_data-name = 'test.txt'.
    ls_data-size = '12345'.
    ls_data-mode = '01234'.

    cut->_append_nulls( CHANGING data = ls_data ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-name
      exp = 'test.txt' && null && null ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-size
      exp = '12345' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-mode
      exp = '01234' && null && null && null ).

    cut->_remove_nulls( CHANGING data = ls_data ).

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
      filename TYPE string,
      prefix   TYPE ZCL_ABAPPM_TAR=>TY_HEADER-PREFIX,
      name     TYPE ZCL_ABAPPM_TAR=>TY_HEADER-NAME.

    cl_abap_unit_assert=>assert_equals(
      act = cut->_to_filename( prefix = 'package/modules' name = 'tar.sh' )
      exp = 'package/modules/tar.sh' ).

    " Short filename
    cut->_from_filename(
      EXPORTING
        filename = 'package/modules/tar.sh'
      IMPORTING
        prefix   = prefix
        name     = name ).

    cl_abap_unit_assert=>assert_equals(
      act = prefix
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = name
      exp = 'package/modules/tar.sh' ).

    " Long filename
    filename = 'package/node_modules/node-gyp/node_modules/path-array/' &&
      'node_modules/array-index/node_modules/es6-symbol/case-insensitive-compare.js'.

    cut->_from_filename(
      EXPORTING
        filename = filename
      IMPORTING
        prefix   = prefix
        name     = name ).

    cl_abap_unit_assert=>assert_equals(
      act = prefix
      exp = 'package/node_modules/node-gyp' ).

    cl_abap_unit_assert=>assert_equals(
      act = prefix && '/' && name
      exp = filename ).

  ENDMETHOD.

  METHOD checksum.

    CONSTANTS lc_data TYPE string VALUE 'abc 123'.

    cl_abap_unit_assert=>assert_equals(
      act = cut->_checksum( lc_data )
      exp = 476 ).

  ENDMETHOD.

  METHOD octal.

    cl_abap_unit_assert=>assert_equals(
      act = cut->_to_octal( 0 )
      exp = '0' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_to_octal( 143 )
      exp = '217' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_to_octal( 4565 )
      exp = '10725' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_to_octal( 498112 )
      exp = '1714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_from_octal( '0' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_from_octal( '217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_from_octal( '10725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_from_octal( '1714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD pad.

    cl_abap_unit_assert=>assert_equals(
      act = cut->_pad( number = 0 length = 4 )
      exp = '0000' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_pad( number = 143 length = 8 )
      exp = '00000217' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_pad( number = 4565 length = 8 )
      exp = '00010725' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_pad( number = 498112 length = 12 )
      exp = '000001714700' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_unpad( '0000' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_unpad( '00000217' )
      exp = 143 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_unpad( '00010725' )
      exp = 4565 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_unpad( '000001714700' )
      exp = 498112 ).

  ENDMETHOD.

  METHOD unixtime.

    DATA:
      date TYPE d,
      time TYPE t.

    cl_abap_unit_assert=>assert_equals(
      act = cut->_to_unixtime( date = '20221126' time = '123456' )
      exp = 1669466096 ).

    cut->_from_unixtime(
      EXPORTING
        unixtime = 1669466096
      IMPORTING
        date     = date
        time     = time ).

    cl_abap_unit_assert=>assert_equals(
      act = date
      exp = '20221126' ).

    cl_abap_unit_assert=>assert_equals(
      act = time
      exp = '123456' ).

  ENDMETHOD.

  METHOD xstring.

    cl_abap_unit_assert=>assert_equals(
      act = cut->_to_xstring( 'abc 123 -' )
      exp = '61626320313233202D' ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->_from_xstring( '61626320313233202D' )
      exp = 'abc 123 -' ).

  ENDMETHOD.

ENDCLASS.
