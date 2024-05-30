CLASS ltcl_semver_cli DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

* https://github.com/npm/node-semver/blob/main/test/bin/semver.js
* https://github.com/npm/node-semver/blob/main/tap-snapshots/test/bin/semver.js.test.cjs
  PRIVATE SECTION.

    METHODS:
      test
        IMPORTING
          args    TYPE string
          out     TYPE string OPTIONAL
          out_tab TYPE string_table OPTIONAL
          err     TYPE string OPTIONAL,
      help FOR TESTING RAISING zcx_abappm_semver_error,
      inc FOR TESTING RAISING zcx_abappm_semver_error,
      sorting_and_filtering FOR TESTING RAISING zcx_abappm_semver_error,
      coercing FOR TESTING RAISING zcx_abappm_semver_error,
      args_with_equals FOR TESTING RAISING zcx_abappm_semver_error.

ENDCLASS.

CLASS zcl_abappm_semver_cli DEFINITION LOCAL FRIENDS ltcl_semver_cli.

CLASS ltcl_semver_cli IMPLEMENTATION.

  METHOD test.

    IF out_tab IS INITIAL.
      SPLIT out AT |\n| INTO TABLE DATA(exp_out).
    ELSE.
      exp_out = out_tab.
    ENDIF.

    DATA(exp_err) = replace( val = err sub = |\n| with = '' ).

    TRY.
        DATA(act_out) = zcl_abappm_semver_cli=>main( args ).
      CATCH zcx_abappm_semver_error INTO DATA(error).
        DATA(act_err) = error->get_text( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = act_out
      exp = exp_out
      msg = |{ args } \| Different output| ).

    cl_abap_unit_assert=>assert_equals(
      act = act_err
      exp = exp_err
      msg = |{ args } \| Different error| ).

  ENDMETHOD.

  METHOD help.

    test(
      args    = '-h'
      out_tab = zcl_abappm_semver_cli=>help( ) ).

    test(
      args    = '-?'
      out_tab = zcl_abappm_semver_cli=>help( ) ).

    test(
      args    = '--help'
      out_tab = zcl_abappm_semver_cli=>help( ) ).

    test(
      args    = ''
      out_tab = zcl_abappm_semver_cli=>help( ) ).

  ENDMETHOD.

  METHOD inc.

    test(
      args = '-i major 1.0.0'
      out  = |2.0.0\n| ).

    test(
      args = '-i major 1.0.0 1.0.1'
      err  = |--inc can only be used on a single version with no range\n| ).

    test(
      args = '-i premajor 1.0.0 --preid=beta'
      out  = |2.0.0-beta.0\n| ).

    test(
      args = '-i premajor 1.0.0 --preid=beta -n 1'
      out  = |2.0.0-beta.1\n| ).

    test(
      args = '-i premajor 1.0.0 --preid=beta -n false'
      out  = |2.0.0-beta\n| ).

    test(
      args = '-i 1.2.3'
      out  = |1.2.4\n| ).

  ENDMETHOD.

  METHOD sorting_and_filtering.

    test(
      args = '1.2.3 3.2.1 2.3.4'
      out  = |1.2.3\n2.3.4\n3.2.1\n| ).

    test(
      args = '1.2.3 3.2.1 2.3.4 2.3.4-beta'
      out  = |1.2.3\n2.3.4-beta\n2.3.4\n3.2.1\n| ).

    test(
      args = '1.2.3 -v 3.2.1 --version 2.3.4'
      out  = |1.2.3\n2.3.4\n3.2.1\n| ).

    test(
      args = '1.2.3 -v 3.2.1 --version 2.3.4 -rv'
      out  = |3.2.1\n2.3.4\n1.2.3\n| ).

    test(
      args = '1.2.3foo 1.2.3-bar'
      out  = |1.2.3-bar\n| ).

    test(
      args = '1.2.3foo 1.2.3-bar -l'
      out  = |1.2.3-bar\n| ).

    test(
      args = '1.2.3 3.2.1 -r 2.x 2.3.4'
      out  = |2.3.4\n| ).

    test(
      args = '1.2.3 3.2.1 2.3.4 2.3.4-beta 2.0.0asdf -r 2.x'
      out  = |2.3.4\n| ).

    test(
      args = '1.2.3 3.2.1 2.3.4 2.3.4-beta 2.0.0asdf -r 2.x -p'
      out  = |2.3.4-beta\n2.3.4\n| ).

    test(
      args = '3.2.1 2.3.4 2.3.4-beta 2.0.0asdf -r 2.x -p -l'
      out  = |2.3.4-beta\n2.3.4\n| ).

    test(
      args = '1.2.3 3.2.1 -r 2.x'
      out  = ||
      err  = 'No valid versions found' ).

  ENDMETHOD.

  METHOD coercing.

    test(
      args = '1.2.3.4.5.6 -c'
      out  = |1.2.3\n| ).

    test(
      args = '1.2.3.4.5.6 -c --rtl'
      out  = |4.5.6\n| ).

    test(
      args = '1.2.3.4.5.6 -c --rtl --ltr'
      out  = |1.2.3\n| ).

    test(
      args = 'not a version 1.2.3 -c'
      out  = |1.2.3\n| ).

    test(
      args = 'not a version -c'
      out  = ||
      err  = 'No valid versions found' ).

  ENDMETHOD.

  METHOD args_with_equals.

    test(
      args = '1.2.3 --version'
      out  = |1.2.3\n| ).

    test(
      args = '1.2.3 2.3.4 --range 1'
      out  = |1.2.3\n| ).

    test(
      args = '1.0.0 --increment major'
      out  = |2.0.0\n| ).

    test(
      args = '1.0.0 --increment premajor --preid beta'
      out  = |2.0.0-beta.0\n| ).

  ENDMETHOD.

ENDCLASS.
