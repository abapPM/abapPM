CLASS lcl_good_renderable DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_gui_renderable.
ENDCLASS.
CLASS lcl_good_renderable IMPLEMENTATION.
  METHOD /apmg/if_apm_gui_renderable~render.
    ri_html = /apmg/cl_apm_html=>create( 'Hello' ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_bad_renderable DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_gui_renderable.
ENDCLASS.
CLASS lcl_bad_renderable IMPLEMENTATION.
  METHOD /apmg/if_apm_gui_renderable~render.
    /apmg/cx_apm_error=>raise( 'Fail!' ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_html DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_html TYPE REF TO /apmg/if_apm_html.

    METHODS:
      wrap    FOR TESTING RAISING /apmg/cx_apm_error,
      add_renderable FOR TESTING RAISING /apmg/cx_apm_error,
      td      FOR TESTING RAISING /apmg/cx_apm_error,
      th      FOR TESTING RAISING /apmg/cx_apm_error,
      wrap_ii FOR TESTING RAISING /apmg/cx_apm_error,
      indent1 FOR TESTING RAISING /apmg/cx_apm_error,
      indent2 FOR TESTING RAISING /apmg/cx_apm_error,
      indent3 FOR TESTING RAISING /apmg/cx_apm_error,
      indent4 FOR TESTING RAISING /apmg/cx_apm_error,
      indent5 FOR TESTING RAISING /apmg/cx_apm_error,
      indent6 FOR TESTING RAISING /apmg/cx_apm_error,
      indent7 FOR TESTING RAISING /apmg/cx_apm_error,
      style1  FOR TESTING RAISING /apmg/cx_apm_error.

    METHODS:
      setup.

ENDCLASS.


CLASS ltcl_html IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_html TYPE /apmg/cl_apm_html.
  ENDMETHOD.

  METHOD indent1.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( 'hello world' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  hello world' && cl_abap_char_utilities=>newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent2.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<input name="comment" type="text">' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  <input name="comment" type="text">' && cl_abap_char_utilities=>newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent3.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<textarea name="body" rows="10" cols="72"></textarea>' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  <textarea name="body" rows="10" cols="72"></textarea>' && cl_abap_char_utilities=>newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent4.

    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( 'foo<br>bar' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '  foo<br>bar' && cl_abap_char_utilities=>newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent5.
* don't dump if something messes up or the nesting gets too wide
    DO 300 TIMES.
      mo_html->add( '<td>' ).
    ENDDO.

    mo_html->render( ).

  ENDMETHOD.

  METHOD indent6.

    " Content of textarea must not be indented
    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<textarea name="body" rows="10" cols="72">' ).
    mo_html->add( 'Some default' ).
    mo_html->add( 'content' ).
    mo_html->add( '</textarea>' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '<textarea name="body" rows="10" cols="72">' && cl_abap_char_utilities=>newline &&
             'Some default' && cl_abap_char_utilities=>newline &&
             'content' && cl_abap_char_utilities=>newline &&
             '</textarea>' && cl_abap_char_utilities=>newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD indent7.

    " Content of pre tag must not be indented
    DATA lv_exp TYPE string.

    mo_html->add( '<td>' ).
    mo_html->add( '<pre>' ).
    mo_html->add( 'Do not change' ).
    mo_html->add( '  the indent' ).
    mo_html->add( '    here' ).
    mo_html->add( '</pre>' ).
    mo_html->add( '</td>' ).

    lv_exp = '<td>' && cl_abap_char_utilities=>newline &&
             '<pre>' && cl_abap_char_utilities=>newline &&
             'Do not change' && cl_abap_char_utilities=>newline &&
             '  the indent' && cl_abap_char_utilities=>newline &&
             '    here' && cl_abap_char_utilities=>newline &&
             '</pre>' && cl_abap_char_utilities=>newline &&
             '</td>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.


  METHOD style1.

    DATA lv_exp TYPE string.

    mo_html->add( '<style type="text/css">' ).
    mo_html->add( '.class1 { color: red }' ).
    mo_html->add( '.class2 {' ).
    mo_html->add( 'color: red' ).
    mo_html->add( '}' ).
    mo_html->add( '</style>' ).

    lv_exp = '<style type="text/css">' && cl_abap_char_utilities=>newline &&
             '  .class1 { color: red }' && cl_abap_char_utilities=>newline &&
             '  .class2 {' && cl_abap_char_utilities=>newline &&
             '    color: red' && cl_abap_char_utilities=>newline &&
             '  }' && cl_abap_char_utilities=>newline &&
             '</style>'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD td.

    mo_html->td( 'Hello' ).
    mo_html->td(
      iv_format_single_line = abap_false
      iv_content = 'Hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        '<td>Hello</td>' && cl_abap_char_utilities=>newline &&
        '<td>' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</td>' ).

  ENDMETHOD.

  METHOD th.

    mo_html->th( 'Hello' ).
    mo_html->th(
      iv_format_single_line = abap_false
      iv_content = 'Hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        '<th>Hello</th>' && cl_abap_char_utilities=>newline &&
        '<th>' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</th>' ).

  ENDMETHOD.

  METHOD wrap_ii.

    mo_html->wrap(
      iv_tag     = 'td'
      ii_content = /apmg/cl_apm_html=>create( )->add( 'Hello' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        '<td>' && cl_abap_char_utilities=>newline &&
        '  Hello' && cl_abap_char_utilities=>newline &&
        '</td>' ).

  ENDMETHOD.

  METHOD wrap.

    mo_html->wrap( 'td' ).
    mo_html->wrap(
      iv_tag     = 'td'
      iv_content = 'Hello' ).
    mo_html->wrap(
      iv_tag     = 'td'
      iv_class   = 'class'
      iv_hint    = 'hint'
      iv_id      = 'id'
      iv_content = 'Hello' ).
    mo_html->wrap(
      iv_tag     = 'td'
      iv_content = 'Hello'
      iv_format_single_line = abap_true ).
    mo_html->wrap(
      iv_tag     = 'td'
      iv_content = 'Hello'
      is_data_attr = /apmg/cl_apm_html=>parse_data_attr( 'id=123' )
      iv_format_single_line = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_html->render( )
      exp =
        |<td></td>\n| &&
        |<td>\n| &&
        |  Hello\n| &&
        |</td>\n| &&
        |<td id="id" class="class" title="hint">\n| &&
        |  Hello\n| &&
        |</td>\n| &&
        |<td>Hello</td>\n| &&
        |<td data-id="123">Hello</td>| ).

  ENDMETHOD.

  METHOD add_renderable.

    DATA lo_good TYPE REF TO lcl_good_renderable.
    DATA lo_bad TYPE REF TO lcl_bad_renderable.

    CREATE OBJECT lo_good.
    CREATE OBJECT lo_bad.

    cl_abap_unit_assert=>assert_equals(
      act = /apmg/cl_apm_html=>create( lo_good )->render( )
      exp = 'Hello' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = /apmg/cl_apm_html=>create( lo_bad )->render( )
      exp = '<span*Fail!*' ).

  ENDMETHOD.

ENDCLASS.
