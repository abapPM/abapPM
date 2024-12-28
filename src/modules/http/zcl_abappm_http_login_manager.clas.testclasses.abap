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
    ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>CLEAR( ).
  ENDMETHOD.

  METHOD teardown.
    ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>CLEAR( ).
  ENDMETHOD.

  METHOD save.

    CONSTANTS c_host TYPE string VALUE 'https://abapgit.org/foo/bar'.
    CONSTANTS c_auth TYPE string VALUE 'foobar'.

    ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>SAVE(
      host = c_host
      auth = c_auth ).

    cl_abap_unit_assert=>assert_equals(
      act = ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>GET( c_host )
      exp = c_auth ).

  ENDMETHOD.

  METHOD encoding.

    DATA auth TYPE string.

    auth = ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>SET(
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

    DATA: auth1 TYPE string,
          auth2 TYPE string.

    ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>SET(
      host     = c_github1
      username = c_username
      password = c_password ).

    auth1 = ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>GET( c_github1 ).
    auth2 = ZCL_ABAPPM_HTTP_LOGIN_MANAGER=>GET( c_github2 ).

    cl_abap_unit_assert=>assert_equals(
      act = auth1
      exp = auth2 ).

  ENDMETHOD.

ENDCLASS.
