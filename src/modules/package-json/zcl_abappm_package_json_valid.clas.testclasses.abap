CLASS lcl_test DEFINITION FINAL FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA tap TYPE REF TO zcl_abappm_tap.
    DATA cut TYPE REF TO zcl_abappm_package_json_valid.

    METHODS:
      setup,
      is_valid_name FOR TESTING,
      is_scoped_name FOR TESTING,
      is_valid_email FOR TESTING,
      is_valid_url FOR TESTING.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT tap.
    CREATE OBJECT cut.
  ENDMETHOD.

  METHOD is_valid_name.
    tap->_( cut->is_valid_name( '' ) )->false( ).
    tap->_( cut->is_valid_name( 'test' ) )->true( ).
    tap->_( cut->is_valid_name( 'test/invalid' ) )->false( ).
    tap->_( cut->is_valid_name( '@test/valid' ) )->true( ).
    tap->_( cut->is_valid_name( '@test/@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_scoped_name.
    tap->_( cut->is_scoped_name( '' ) )->false( ).
    tap->_( cut->is_scoped_name( 'test' ) )->false( ).
    tap->_( cut->is_scoped_name( 'test/invalid' ) )->false( ).
    tap->_( cut->is_scoped_name( '@test/valid' ) )->true( ).
    tap->_( cut->is_scoped_name( '@test/@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_valid_email.
    tap->_( cut->is_valid_email( '' ) )->true( ).
    tap->_( cut->is_valid_email( 'test@example.com' ) )->true( ).
    tap->_( cut->is_valid_email( 'test@invalid' ) )->false( ).
  ENDMETHOD.

  METHOD is_valid_url.
    tap->_( cut->is_valid_url( '' ) )->true( ).
    tap->_( cut->is_valid_url( 'https://example.com' ) )->true( ).
    tap->_( cut->is_valid_url( 'git://invalid.com' ) )->false( ).
  ENDMETHOD.

ENDCLASS.
