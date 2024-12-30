CLASS ltcl_abapgit_syntax_abap DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_abappm_highlighter_abap.

    METHODS:
      setup,
      report_header FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_abapgit_syntax_abap IMPLEMENTATION.

  METHOD setup.

    cut = NEW #( ).

  ENDMETHOD.

  METHOD report_header.

    DATA(act) = cut->process_line( |REPORT zfoo.| ).

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="keyword">REPORT</span> zfoo.|
      act = act ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_syntax_basic_logic DEFINITION DEFERRED.

CLASS zcl_abappm_highlighter_abap DEFINITION LOCAL FRIENDS ltcl_syntax_basic_logic.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_basic_logic DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_syntax_basic_logic DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    DATA syntax_highlighter TYPE REF TO zcl_abappm_highlighter_abap.

    METHODS:
      setup,
      process_line  FOR TESTING,
      format_line   FOR TESTING,
      apply_style   FOR TESTING.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_basic_logic IMPLEMENTATION.

  METHOD setup.

    syntax_highlighter = NEW #( ).

  ENDMETHOD.

  METHOD format_line.

    DATA(line) = `call function 'FM_NAME'. " Commented`.

    DATA(line_exp) =
      '<span class="keyword">call</span>' &&
      ' <span class="keyword">function</span>' &&
      | <span class="text">'FM_NAME'</span>.| &&
      ' <span class="comment">" Commented</span>'.

    DATA(line_act) = syntax_highlighter->process_line( line ).

    cl_abap_unit_assert=>assert_equals( exp = line_exp
                                        act = line_act
                                        msg = |Error during formating: { line }| ).

  ENDMETHOD.

  METHOD apply_style.

    " Call the method and compare results
    DATA(line_act) = syntax_highlighter->apply_style(
      line  = 'CALL FUNCTION'
      class = zcl_abappm_highlighter_abap=>c_css-keyword ).

    cl_abap_unit_assert=>assert_equals(
      act = line_act
      exp = '<span class="keyword">CALL FUNCTION</span>'
      msg = 'Failure during applying of style.' ).

  ENDMETHOD.

  METHOD process_line.

    " Call the method with empty parameter and compare results
    DATA(line_act) = syntax_highlighter->process_line( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = line_act
      exp = ''
      msg = 'Failure in method process_line.' ).

    " Call the method with non-empty line and compare results
    line_act = syntax_highlighter->process_line( '* CALL FUNCTION' ).

    cl_abap_unit_assert=>assert_equals(
      act = line_act
      exp = '<span class="comment">* CALL FUNCTION</span>'
      msg = 'Failure in method process_line.' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_syntax_cases DEFINITION DEFERRED.

CLASS zcl_abappm_highlighter_abap DEFINITION LOCAL FRIENDS ltcl_syntax_cases.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases definition
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    DATA:
      after_parse  TYPE zcl_abappm_highlighter_abap=>ty_match_tt,
      after_order  TYPE zcl_abappm_highlighter_abap=>ty_match_tt,
      after_extend TYPE zcl_abappm_highlighter_abap=>ty_match_tt.

    METHODS:
      do_test IMPORTING line TYPE string,
      generate_parse IMPORTING token  TYPE c
                               offset TYPE i
                               length TYPE i,
      generate_order IMPORTING token    TYPE c
                               offset   TYPE i
                               length   TYPE i
                               text_tag TYPE string,
      generate_extend IMPORTING token    TYPE c
                                offset   TYPE i
                                length   TYPE i
                                text_tag TYPE string,
      test_abap_01 FOR TESTING,
      test_abap_02 FOR TESTING,
      test_abap_03 FOR TESTING,
      test_abap_04 FOR TESTING,
      test_abap_05 FOR TESTING,
      test_abap_06 FOR TESTING,
      test_abap_07 FOR TESTING,
      test_abap_08 FOR TESTING.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases IMPLEMENTATION.

  METHOD do_test.

    DATA(syntax_highlighter) = NEW zcl_abappm_highlighter_abap( ).

    DATA(matches_act) = syntax_highlighter->parse_line( line ).

    SORT matches_act BY offset.

    cl_abap_unit_assert=>assert_equals( exp = after_parse
                                        act = matches_act
                                        msg = |Error during parsing: { line }| ).

    syntax_highlighter->order_matches( EXPORTING line    = line
                              CHANGING  matches = matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = after_order
                                        act = matches_act
                                        msg = |Error during ordering: { line }| ).

    syntax_highlighter->extend_matches(
      EXPORTING
        line    = line
      CHANGING
        matches = matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = after_extend
                                        act = matches_act
                                        msg = |Error during extending: { line }| ).

    " Check consistency
    DATA(offs) = 0.
    LOOP AT matches_act INTO DATA(match).
      IF match-offset <> offs.
        cl_abap_unit_assert=>assert_equals( exp = offs
                                            act = match-offset
                                            msg = | Error during consistency check: { sy-tabix }| ).
      ENDIF.
      offs = offs + match-length.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_parse.
    DATA(match) = VALUE zcl_abappm_highlighter_abap=>ty_match(
      token  = token
      offset = offset
      length = length ).
    APPEND match TO after_parse.
  ENDMETHOD.

  METHOD generate_order.
    DATA(match) = VALUE zcl_abappm_highlighter_abap=>ty_match(
      token    = token
      offset   = offset
      length   = length
      text_tag = text_tag ).
    APPEND match TO after_order.
  ENDMETHOD.

  METHOD generate_extend.
    DATA(match) = VALUE zcl_abappm_highlighter_abap=>ty_match(
      token    = token
      offset   = offset
      length   = length
      text_tag = text_tag ).
    APPEND match TO after_extend.
  ENDMETHOD.

******************************************************
* Test parsing and ordering of comments              *
******************************************************
  METHOD test_abap_01.

    DATA(line) = `* commented out line with key word data`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'C'
                    offset = 0
                    length = 1 ).
    generate_parse( token  = 'K'
                    offset = 12
                    length = 3 ).
    generate_parse( token  = 'K'
                    offset = 16
                    length = 4 ).
    generate_parse( token  = 'K'
                    offset = 21
                    length = 4 ).
    generate_parse( token  = 'K'
                    offset = 26
                    length = 3 ).
    generate_parse( token  = 'K'
                    offset = 30
                    length = 4 ).
    generate_parse( token  = 'K'
                    offset = 35
                    length = 4 ).


    " Generate table with expected values after ordering
    generate_order( token    = 'C'
                    offset   = 0
                    length   = 39
                    text_tag = '' ).

    " Generate table with expected values after ordering
    generate_extend( token    = 'C'
                     offset   = 0
                     length   = 39
                     text_tag = '' ).

    do_test( line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of remainder of string   *
******************************************************
  METHOD test_abap_02.

    DATA(line) = `data: lv_var_name type string.`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'K'
                    offset = 0
                    length = 4 ).
    generate_parse( token  = 'K'
                    offset = 18
                    length = 4 ).
    generate_parse( token  = 'K'
                    offset = 23
                    length = 6 ).

    " Generate table with expected values after ordering
    generate_order( token    = 'K'
                    offset   = 0
                    length   = 4
                    text_tag = '' ).
    generate_order( token    = 'K'
                    offset   = 18
                    length   = 4
                    text_tag = '' ).
    generate_order( token    = 'K'
                    offset   = 23
                    length   = 6
                    text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( token    = 'K'
                     offset   = 0
                     length   = 4
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 4
                     length   = 14
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 18
                     length   = 4
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 22
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 23
                     length   = 6
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 29
                     length   = 1
                     text_tag = '' ).

    do_test( line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of key words & texts     *
******************************************************
  METHOD test_abap_03.

    DATA(line) = `call function 'FM_NAME'. " Commented`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'K'
                    offset = 0
                    length = 4 ).
    generate_parse( token  = 'K'
                    offset = 5
                    length = 8 ).
    generate_parse( token  = 'T'
                    offset = 14
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 22
                    length = 1 ).
    generate_parse( token  = 'C'
                    offset = 25
                    length = 1 ).

    " Generate table with expected values after ordering
    generate_order( token    = 'K'
                    offset   = 0
                    length   = 4
                    text_tag = '' ).
    generate_order( token    = 'K'
                    offset   = 5
                    length   = 8
                    text_tag = '' ).
    generate_order( token    = 'T'
                    offset   = 14
                    length   = 9
                    text_tag = '''' ).
    generate_order( token    = 'C'
                    offset   = 25
                    length   = 11
                    text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( token    = 'K'
                     offset   = 0
                     length   = 4
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 4
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 5
                     length   = 8
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 13
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 14
                     length   = 9
                     text_tag = '''' ).
    generate_extend( token    = '.'
                     offset   = 23
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'C'
                     offset   = 25
                     length   = 11
                     text_tag = '' ).

    do_test( line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of key words in texts    *
******************************************************
  METHOD test_abap_04.

    DATA(line) = `constants: lc_var type string value 'simpletext data simpletext'.`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'K'
                    offset = 0
                    length = 9 ).
    generate_parse( token  = 'K'
                    offset = 18
                    length = 4 ).
    generate_parse( token  = 'K'
                    offset = 23
                    length = 6 ).
    generate_parse( token  = 'K'
                    offset = 30
                    length = 5 ).
    generate_parse( token  = 'T'
                    offset = 36
                    length = 1 ).
    generate_parse( token  = 'K'
                    offset = 48
                    length = 4 ).
    generate_parse( token  = 'T'
                    offset = 63
                    length = 1 ).

    " Generate table with expected values after ordering
    generate_order( token    = 'K'
                    offset   = 0
                    length   = 9
                    text_tag = '' ).
    generate_order( token    = 'K'
                    offset   = 18
                    length   = 4
                    text_tag = '' ).
    generate_order( token    = 'K'
                    offset   = 23
                    length   = 6
                    text_tag = '' ).
    generate_order( token    = 'K'
                    offset   = 30
                    length   = 5
                    text_tag = '' ).
    generate_order( token    = 'T'
                    offset   = 36
                    length   = 28
                    text_tag = '''' ).

    " Generate table with expected values after ordering
    generate_extend( token    = 'K'
                     offset   = 0
                     length   = 9
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 9
                     length   = 9
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 18
                     length   = 4
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 22
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 23
                     length   = 6
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 29
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 30
                     length   = 5
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 35
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 36
                     length   = 28
                     text_tag = '''' ).
    generate_extend( token    = '.'
                     offset   = 64
                     length   = 1
                     text_tag = '' ).

    do_test( line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering texts in curly brackets  *
******************************************************
  METHOD test_abap_05.

    DATA(line) = `a = |{ b }={ c }|.`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'T'
                    offset = 4
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 5
                    length = 1 ).
    generate_parse( token  = 'K'
                    offset = 7
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 9
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 11
                    length = 1 ).
    generate_parse( token  = 'K'
                    offset = 13
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 15
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 16
                    length = 1 ).

    " Generate table with expected values after ordering
    generate_order( token    = 'T'
                    offset   = 4
                    length   = 1
                    text_tag = '|' ).
    generate_order( token    = 'K'
                    offset   = 7
                    length   = 1
                    text_tag = '' ).
    generate_order( token    = 'T'
                    offset   = 10
                    length   = 1
                    text_tag = '}' ).
    generate_order( token    = 'K'
                    offset   = 13
                    length   = 1
                    text_tag = '' ).
    generate_order( token    = 'T'
                    offset   = 16
                    length   = 1
                    text_tag = '}' ).

    " Generate table with expected values after extending
    generate_extend( token    = '.'
                     offset   = 0
                     length   = 4
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 4
                     length   = 1
                     text_tag = '|' ).
    generate_extend( token    = '.'
                     offset   = 5
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 7
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 8
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 10
                     length   = 1
                     text_tag = '}' ).
    generate_extend( token    = '.'
                     offset   = 11
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 13
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 14
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 16
                     length   = 1
                     text_tag = '}' ).
    generate_extend( token    = '.'
                     offset   = 17
                     length   = 1
                     text_tag = '' ).

    do_test( line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of texts                 *
******************************************************
  METHOD test_abap_06.

    DATA(line) = `lv_line = lc_constant && |XYZ { 'ab' && |ac{ 'UU' }| }|`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'K'
                    offset = 22
                    length = 2 ).
    generate_parse( token  = 'T'
                    offset = 25
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 30
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 32
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 35
                    length = 1 ).
    generate_parse( token  = 'K'
                    offset = 37
                    length = 2 ).
    generate_parse( token  = 'T'
                    offset = 40
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 43
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 45
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 48
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 50
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 51
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 53
                    length = 1 ).
    generate_parse( token  = 'T'
                    offset = 54
                    length = 1 ).

    " Generate table with expected values after ordering
    generate_order( token    = 'K'
                    offset   = 22
                    length   = 2
                    text_tag = '' ).
    generate_order( token    = 'T'
                    offset   = 25
                    length   = 5
                    text_tag = '|' ).
    generate_order( token    = 'T'
                    offset   = 32
                    length   = 4
                    text_tag = '''' ).
    generate_order( token    = 'K'
                    offset   = 37
                    length   = 2
                    text_tag = '' ).
    generate_order( token    = 'T'
                    offset   = 40
                    length   = 3
                    text_tag = '|' ).
    generate_order( token    = 'T'
                    offset   = 45
                    length   = 4
                    text_tag = '''' ).
    generate_order( token    = 'T'
                    offset   = 51
                    length   = 1
                    text_tag = '}' ).
    generate_order( token    = 'T'
                    offset   = 54
                    length   = 1
                    text_tag = '}' ).

    " Generate table with expected values after extending
    generate_extend( token    = '.'
                     offset   = 0
                     length   = 22
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 22
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 24
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 25
                     length   = 5
                     text_tag = '|' ).
    generate_extend( token    = '.'
                     offset   = 30
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 32
                     length   = 4
                     text_tag = '''' ).
    generate_extend( token    = '.'
                     offset   = 36
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 37
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 39
                     length   = 1
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 40
                     length   = 3
                     text_tag = '|' ).
    generate_extend( token    = '.'
                     offset   = 43
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 45
                     length   = 4
                     text_tag = '''' ).
    generate_extend( token    = '.'
                     offset   = 49
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 51
                     length   = 1
                     text_tag = '}' ).
    generate_extend( token    = '.'
                     offset   = 52
                     length   = 2
                     text_tag = '' ).
    generate_extend( token    = 'T'
                     offset   = 54
                     length   = 1
                     text_tag = '}' ).

    do_test( line ).

  ENDMETHOD.

********************************************************
* Check that '*' in select statement is not a match    *
********************************************************
  METHOD test_abap_07.

    DATA(line) = `SELECT * FROM foo`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'K'
                    offset = 0
                    length = 6 ).

    generate_parse( token  = 'K'
                    offset = 9
                    length = 4 ).

    " Generate table with expected values after ordering
    generate_order( token    = 'K'
                    offset   = 0
                    length   = 6
                    text_tag = '' ).
    generate_order( token    = 'K'
                    offset   = 9
                    length   = 4
                    text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( token    = 'K'
                     offset   = 0
                     length   = 6
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 6
                     length   = 3
                     text_tag = '' ).
    generate_extend( token    = 'K'
                     offset   = 9
                     length   = 4
                     text_tag = '' ).
    generate_extend( token    = '.'
                     offset   = 13
                     length   = 4
                     text_tag = '' ).

    do_test( line ).

  ENDMETHOD.

********************************************************
* Test parsing and ordering of key words in structures *
********************************************************
  METHOD test_abap_08.

    DATA(line) = `lv_length = <match>-length.`.

    " Generate table with expected values after parsing
    generate_parse( token  = 'K'
                    offset = 13
                    length = 5 ).
    generate_parse( token  = 'K'
                    offset = 20
                    length = 6 ).

    " Generate table with expected values after extending
    generate_extend( token    = '.'
                     offset   = 0
                     length   = 27
                     text_tag = '' ).

    do_test( line ).

  ENDMETHOD.

ENDCLASS.
