CLASS ltcl_package_json_valid DEFINITION FINAL FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA tap TYPE REF TO zcl_abappm_tap.

    METHODS:
      setup,
      is_valid_name FOR TESTING,
      is_scoped_name FOR TESTING,
      is_valid_email FOR TESTING,
      is_valid_url FOR TESTING.

ENDCLASS.

CLASS ltcl_package_json_valid IMPLEMENTATION.

  METHOD setup.
    tap = NEW #( ).
  ENDMETHOD.

  METHOD is_valid_name.
    tap->_( zcl_abappm_package_json_valid=>is_valid_name( '' ) )->false( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_name( 'test' ) )->true( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_name( 'test/invalid' ) )->false( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_name( '@test/valid' ) )->true( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_name( '@test/@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_scoped_name.
    tap->_( zcl_abappm_package_json_valid=>is_scoped_name( '' ) )->false( ).
    tap->_( zcl_abappm_package_json_valid=>is_scoped_name( 'test' ) )->false( ).
    tap->_( zcl_abappm_package_json_valid=>is_scoped_name( 'test/invalid' ) )->false( ).
    tap->_( zcl_abappm_package_json_valid=>is_scoped_name( '@test/valid' ) )->true( ).
    tap->_( zcl_abappm_package_json_valid=>is_scoped_name( '@test/@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_valid_email.
    tap->_( zcl_abappm_package_json_valid=>is_valid_email( '' ) )->true( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_email( 'test@example.com' ) )->true( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_email( 'test@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_valid_url.
    tap->_( zcl_abappm_package_json_valid=>is_valid_url( '' ) )->true( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_url( 'https://example.com' ) )->true( ).
    tap->_( zcl_abappm_package_json_valid=>is_valid_url( '\\git://invalid.com' ) )->false( ).
  ENDMETHOD.

ENDCLASS.
