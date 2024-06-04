********************************************************************************
* ABAP Emoji
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
********************************************************************************
CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abappm_markdown_emoji.

    METHODS setup.
    METHODS emoji_find FOR TESTING.
    METHODS emoji_format FOR TESTING.
    METHODS emoji_format_other_base FOR TESTING.
    METHODS emoji_css FOR TESTING.
    METHODS emoji_list FOR TESTING.
    METHODS twemoji_find FOR TESTING.
    METHODS twemoji_format FOR TESTING.
    METHODS twemoji_css FOR TESTING.
    METHODS twemoji_list FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_abappm_markdown_emoji=>create( ).
  ENDMETHOD.


  METHOD emoji_find.
    DATA lt_emoji TYPE TABLE OF string.

    lt_emoji = mo_cut->find_emoji( '^heart$' ).

    cl_aunit_assert=>assert_equals(
      act = lines( lt_emoji )
      exp = 1 ).
  ENDMETHOD.

  METHOD emoji_format.
    DATA lv_html TYPE string.
    DATA lv_exp TYPE string.

    lv_html = mo_cut->format_emoji( 'Here is a :heart:' ).
    lv_exp = 'Here is a <img src="https://github.githubassets.com/images/icons/emoji/unicode/2764.png" class="emoji">'.

    cl_aunit_assert=>assert_equals(
      act = lv_html
      exp = lv_exp ).
  ENDMETHOD.

  METHOD emoji_format_other_base.
    DATA lv_html TYPE string.
    DATA lv_exp TYPE string.

    lv_html = mo_cut->format_emoji(
      iv_line     = 'Here is a :heart:'
      iv_base_url = 'https://mydomain.com/emoji/' ).

    lv_exp = 'Here is a <img src="https://mydomain.com/emoji/unicode/2764.png" class="emoji">'.

    cl_aunit_assert=>assert_equals(
      act = lv_html
      exp = lv_exp ).
  ENDMETHOD.

  METHOD emoji_css.
    DATA lt_emoji TYPE TABLE OF string.

    lt_emoji = mo_cut->get_emoji_css( ).

    cl_aunit_assert=>assert_not_initial( lt_emoji ).
  ENDMETHOD.

  METHOD emoji_list.
    DATA lt_emoji TYPE TABLE OF string.

    lt_emoji = mo_cut->get_emoji_list( ).

    cl_aunit_assert=>assert_not_initial( lt_emoji ).
  ENDMETHOD.

  METHOD twemoji_find.
    DATA lt_emoji TYPE TABLE OF string.

    lt_emoji = mo_cut->find_twemoji( '^sparkles$' ).

    cl_aunit_assert=>assert_equals(
      act = lines( lt_emoji )
      exp = 1 ).
  ENDMETHOD.

  METHOD twemoji_format.
    DATA lv_html TYPE string.

    lv_html = mo_cut->format_twemoji( 'Here are some :sparkles:' ).

    cl_aunit_assert=>assert_equals(
      act = lv_html
      exp = 'Here are some <i class="twa twa-sparkles"></i>' ).
  ENDMETHOD.

  METHOD twemoji_css.
    DATA lt_emoji TYPE TABLE OF string.

    lt_emoji = mo_cut->get_twemoji_css( ).

    cl_aunit_assert=>assert_not_initial( lt_emoji ).
  ENDMETHOD.

  METHOD twemoji_list.
    DATA lt_emoji TYPE TABLE OF string.

    lt_emoji = mo_cut->get_twemoji_list( ).

    cl_aunit_assert=>assert_not_initial( lt_emoji ).
  ENDMETHOD.

ENDCLASS.
