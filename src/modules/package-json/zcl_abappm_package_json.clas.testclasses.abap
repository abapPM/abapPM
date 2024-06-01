CLASS ltcl_package_json DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA mi_package TYPE REF TO zif_abappm_package_json.

    METHODS init_test
      IMPORTING
        iv_args TYPE string
      RAISING
        zcx_abappm_package_json.

    METHODS test_valid
      IMPORTING
        iv_args TYPE string.

    METHODS test_invalid
      IMPORTING
        iv_args TYPE string.

    METHODS test_compare
      IMPORTING
        is_json TYPE zif_abappm_package_json_types=>ty_package_json
        iv_json TYPE string.

    METHODS:
      valid_packages FOR TESTING,
      invalid_packages FOR TESTING,
      get_complete FOR TESTING RAISING zcx_abappm_package_json,
      get_package FOR TESTING RAISING zcx_abappm_package_json,
      set_package FOR TESTING RAISING zcx_abappm_package_json,
      get_persons FOR TESTING RAISING zcx_abappm_package_json,
      set_persons FOR TESTING RAISING zcx_abappm_package_json,
      dependencies_json_to_abap FOR TESTING RAISING zcx_abappm_package_json,
      dependencies_abap_to_json FOR TESTING RAISING zcx_abappm_package_json.

ENDCLASS.

CLASS zcl_abappm_package_json DEFINITION LOCAL FRIENDS ltcl_package_json.

CLASS ltcl_package_json IMPLEMENTATION.

  METHOD init_test.

    DATA:
      lv_package TYPE devclass,
      lv_name    TYPE string,
      lv_version TYPE string,
      lv_private TYPE abap_bool.

    FREE mi_package.

    SPLIT iv_args AT ',' INTO lv_package lv_name lv_version lv_private.

    CREATE OBJECT mi_package TYPE zcl_abappm_package_json
      EXPORTING
        iv_package = lv_package
        iv_name    = lv_name
        iv_version = lv_version
        iv_private = lv_private.

  ENDMETHOD.

  METHOD test_valid.

    DATA lx_error TYPE REF TO zcx_abappm_package_json.

    TRY.
        init_test( iv_args ).
        IF mi_package->is_valid( ) = abap_false.
          cl_abap_unit_assert=>fail( |Invalid but expected valid: { iv_args }| ).
        ENDIF.
      CATCH zcx_abappm_package_json INTO lx_error.
        cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_invalid.
    TRY.
        init_test( iv_args ).
        IF mi_package->is_valid( ) = abap_true.
          cl_abap_unit_assert=>fail( |Valid but expected invalid: { iv_args }| ).
        ENDIF.
      CATCH zcx_abappm_package_json.
    ENDTRY.
  ENDMETHOD.

  METHOD test_compare.

    DATA lv_json TYPE string.

    cl_abap_unit_assert=>assert_equals(
      act = mi_package->get( )
      exp = is_json ).

    " Strip newlines and condense for easier comparison
    lv_json = condense( replace(
      val  = mi_package->get_json( )
      sub  = cl_abap_char_utilities=>newline
      with = ''
      occ  = 0 ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_json
      exp = iv_json ).

  ENDMETHOD.

  METHOD valid_packages.
    " Various SAP packages
    test_valid( '$TEST,test,1.0.0' ).
    test_valid( 'YTEST,test,1.0.0' ).
    test_valid( 'ZTEST,test,1.0.0' ).
    test_valid( '/NAMESPC/TEST,test,1.0.0' ).
    " Various names
    test_valid( '$TEST,test_long_name-with-special-characters,1.0.0' ).
    test_valid( '$TEST,@scope/test,1.0.0' ).
    " Various versions
    test_valid( '$TEST,test,1.0.0' ).
    test_valid( '$TEST,test,1.2.3' ).
    test_valid( '$TEST,test,3.0.99' ).
    test_valid( '$TEST,test,1.0.0-prerelease.10' ).
  ENDMETHOD.

  METHOD invalid_packages.
    " Missing input
    test_invalid( ',test,1.0.0' ).
    test_invalid( '$TEST,,1.0.0' ).
    test_invalid( '$TEST,test,' ).
    " Various SAP packages
    test_invalid( 'TEST,test,1.0.0' ).
    test_invalid( '$TE^ST,test,1.0.0' ).
    test_invalid( '$test,test,1.0.0' ).
    test_invalid( ' $TEST,test,1.0.0' ).
    test_invalid( '$TE ST,test,1.0.0' ).
    test_invalid( '//TEST,test,1.0.0' ).
    test_invalid( '/TEST/,test,1.0.0' ).
    test_invalid( '/NAMESPACE/TEST,test,1.0.0' ).
    test_invalid( '/0SAP/TEST,test,1.0.0' ).
    " Various names
    test_invalid( '$TEST, test,1.0.0' ).
    test_invalid( '$TEST,te st,1.0.0' ).
    test_invalid( '$TEST,/test,1.0.0' ).
    test_invalid( '$TEST,test/,1.0.0' ).
    test_invalid( '$TEST,scope/test,1.0.0' ).
    test_invalid( '$TEST,t,1.0.0' ).
    test_invalid( '$TEST,tt,1.0.0' ).
    test_invalid( |$TEST,{ repeat( val = 't' occ = 215 ) },1.0.0| ).
    " Various versions
    test_invalid( '$TEST,test,1.0' ).
    test_invalid( '$TEST,test,1' ).
  ENDMETHOD.

  METHOD get_complete.

    DATA:
      lv_json TYPE string,
      ls_json TYPE zif_abappm_package_json_types=>ty_package_json,
      ls_dep  TYPE zif_abappm_package_json_types=>ty_dependency.

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.

    ls_dep-name  = 'dep2'.
    ls_dep-range = '2.0.0'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.
    ls_dep-name  = 'dep3'.
    ls_dep-range = '>3'.
    INSERT ls_dep INTO TABLE ls_json-dev_dependencies.
    ls_dep-name  = 'dep4'.
    ls_dep-range = '^4.1.0'.
    INSERT ls_dep INTO TABLE ls_json-optional_dependencies.
    ls_dep-name  = 'abap'.
    ls_dep-range = '>=7.50'.
    INSERT ls_dep INTO TABLE ls_json-engines.
    ls_dep-name  = 'apm'.
    ls_dep-range = '>=1'.
    INSERT ls_dep INTO TABLE ls_json-engines.

    mi_package->set( ls_json ).

    lv_json = |\{\n|
      && |  "name": "test",\n|
      && |  "version": "1.0.0",\n|
      && |  "description": "",\n|
      && |  "keywords": [],\n|
      && |  "homepage": "",\n|
      && |  "bugs": \{\n|
      && |    "url": "",\n|
      && |    "email": ""\n|
      && |  \},\n|
      && |  "license": "",\n|
      && |  "author": \{\n|
      && |    "name": "",\n|
      && |    "url": "",\n|
      && |    "email": "",\n|
      && |    "avatar": ""\n|
      && |  \},\n|
      && |  "contributors": [],\n|
      && |  "maintainers": [],\n|
      && |  "main": "",\n|
      && |  "man": [],\n|
      && |  "type": "",\n|
      && |  "repository": \{\n|
      && |    "type": "",\n|
      && |    "url": "",\n|
      && |    "directory": ""\n|
      && |  \},\n|
      && |  "funding": \{\n|
      && |    "type": "",\n|
      && |    "url": ""\n|
      && |  \},\n|
      && |  "dependencies": \{\n|
      && |    "dep2": "2.0.0"\n|
      && |  \},\n|
      && |  "devDependencies": \{\n|
      && |    "dep3": ">3"\n|
      && |  \},\n|
      && |  "optionalDependencies": \{\n|
      && |    "dep4": "^4.1.0"\n|
      && |  \},\n|
      && |  "bundleDependencies": [],\n|
      && |  "engines": \{\n|
      && |    "abap": ">=7.50",\n|
      && |    "apm": ">=1"\n|
      && |  \},\n|
      && |  "os": [],\n|
      && |  "cpu": [],\n|
      && |  "db": [],\n|
      && |  "private": false,\n|
      && |  "dist": \{\n|
      && |    "fileCount": 0,\n|
      && |    "integrity": "",\n|
      && |    "shasum": "",\n|
      && |    "signatures": [],\n|
      && |    "tarball": "",\n|
      && |    "unpackedSize": 0\n|
      && |  \},\n|
      && |  "readme": ""\n|
      && |\}|.

    cl_abap_unit_assert=>assert_equals(
      act = mi_package->get_json( iv_complete = abap_true )
      exp = lv_json ).

  ENDMETHOD.

  METHOD get_package.

    DATA ls_json TYPE zif_abappm_package_json_types=>ty_package_json.

    init_test( '$TEST,test,1.0.0' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0"}' ).

    init_test( '$TEST,test,1.0.0,X' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-private = abap_true.

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    init_test( '/TEST/TEST,@test/test,1.2.3' ).

    CLEAR ls_json.
    ls_json-name    = '@test/test'.
    ls_json-version = '1.2.3'.

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "@test/test", "version": "1.2.3"}' ).

  ENDMETHOD.

  METHOD set_package.

    DATA ls_json TYPE zif_abappm_package_json_types=>ty_package_json.

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-private = abap_true.

    mi_package->set( ls_json ).

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "private": true}' ).

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.
    ls_json-description = 'My package test'.

    mi_package->set( ls_json ).

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test", "version": "1.0.0", "description": "My package test"}' ).

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test-2'.
    ls_json-version = '1.2.3'.

    mi_package->set_json( |\{\n "name": "test-2",\n "version": "1.2.3"\n\}\n| ).

    test_compare(
      is_json = ls_json
      iv_json = '{ "name": "test-2", "version": "1.2.3"}' ).

  ENDMETHOD.

  METHOD get_persons.

  ENDMETHOD.

  METHOD set_persons.

  ENDMETHOD.

  METHOD dependencies_json_to_abap.

    DATA:
      lv_json TYPE string,
      ls_json TYPE zif_abappm_package_json_types=>ty_package_json,
      ls_dep  TYPE zif_abappm_package_json_types=>ty_dependency.

    init_test( '$TEST' ).

    lv_json = |\{\n|
      && |  "name": "test",\n|
      && |  "version": "1.0.0",\n|
      && |  "dependencies": \{\n|
      && |    "dep1": "2.0.0",\n|
      && |    "dep2": ">3",\n|
      && |    "dep3": "^4.1.0"\n|
      && |  \}\n|
      && |\}|.

    mi_package->set_json( lv_json ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.

    ls_dep-name  = 'dep1'.
    ls_dep-range = '2.0.0'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.
    ls_dep-name  = 'dep2'.
    ls_dep-range = '>3'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.
    ls_dep-name  = 'dep3'.
    ls_dep-range = '^4.1.0'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.

    cl_abap_unit_assert=>assert_equals(
      act = mi_package->get( )
      exp = ls_json ).

  ENDMETHOD.

  METHOD dependencies_abap_to_json.

    DATA:
      lv_json TYPE string,
      ls_json TYPE zif_abappm_package_json_types=>ty_package_json,
      ls_dep  TYPE zif_abappm_package_json_types=>ty_dependency.

    init_test( '$TEST' ).

    CLEAR ls_json.
    ls_json-name    = 'test'.
    ls_json-version = '1.0.0'.

    ls_dep-name  = 'dep1'.
    ls_dep-range = '2.0.0'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.
    ls_dep-name  = 'dep2'.
    ls_dep-range = '>3'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.
    ls_dep-name  = 'dep3'.
    ls_dep-range = '^4.1.0'.
    INSERT ls_dep INTO TABLE ls_json-dependencies.

    mi_package->set( ls_json ).

    lv_json = |\{\n|
      && |  "name": "test",\n|
      && |  "version": "1.0.0",\n|
      && |  "dependencies": \{\n|
      && |    "dep1": "2.0.0",\n|
      && |    "dep2": ">3",\n|
      && |    "dep3": "^4.1.0"\n|
      && |  \}\n|
      && |\}|.

    cl_abap_unit_assert=>assert_equals(
      act = mi_package->get_json( )
      exp = lv_json ).

  ENDMETHOD.
ENDCLASS.
