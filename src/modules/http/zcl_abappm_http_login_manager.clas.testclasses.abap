CLASS ltcl_login_manager DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    CONSTANTS:
      c_username TYPE string VALUE 'Aladdin',
      c_password TYPE string VALUE 'OpenSesame'.

    METHODS:
      setup,
      teardown,
      encoding FOR TESTING,
      save FOR TESTING,
      same_server FOR TESTING.

ENDCLASS.

CLASS ltcl_login_manager IMPLEMENTATION.

  METHOD setup.
    zcl_abappm_http_login_manager=>clear( ).
  ENDMETHOD.

  METHOD teardown.
    zcl_abappm_http_login_manager=>clear( ).
  ENDMETHOD.

  METHOD save.

    CONSTANTS c_host TYPE string VALUE 'https://abapgit.org/foo/bar'.
    CONSTANTS c_auth TYPE string VALUE 'foobar'.

    zcl_abappm_http_login_manager=>save(
      host = c_host
      auth = c_auth ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_http_login_manager=>get( c_host )
      exp = c_auth ).

  ENDMETHOD.

  METHOD encoding.

    DATA(auth) = zcl_abappm_http_login_manager=>set(
      host     = 'https://github.com/abapGit/abapGit.git'
      username = c_username
      password = c_password ).

    cl_abap_unit_assert=>assert_equals(
      act = auth
      exp = 'Basic QWxhZGRpbjpPcGVuU2VzYW1l' ).

  ENDMETHOD.

  METHOD same_server.

    CONSTANTS: c_github1 TYPE string VALUE 'https://github.com/abapGit/abapGit.git',
               c_github2 TYPE string VALUE 'https://github.com/larshp/Foobar.git'.

    zcl_abappm_http_login_manager=>set(
      host     = c_github1
      username = c_username
      password = c_password ).

    DATA(auth1) = zcl_abappm_http_login_manager=>get( c_github1 ).
    DATA(auth2) = zcl_abappm_http_login_manager=>get( c_github2 ).

    cl_abap_unit_assert=>assert_equals(
      act = auth1
      exp = auth2 ).

  ENDMETHOD.

ENDCLASS.
