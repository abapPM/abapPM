CLASS ZCL_ABAPPM_MARKDOWN_EMOJI DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

********************************************************************************
* ABAP Emoji
*
* Support for Unicode Emoji and Twemoji (Emoji v14.0)
*
* https://github.com/Marc-Bernard-Tools/ABAP-Emoji
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
********************************************************************************
* Source locations used for Emoji data
*
* Local Defs: Unicode Emoji provided by GitHub
* Local Impl: Twemoji prodived by Twitter
* Macros: CSS for Twemoji
********************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_code    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      ty_results TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY.

    CONSTANTS c_version TYPE string VALUE '1.1.0' ##NEEDED.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_result) TYPE REF TO ZCL_ABAPPM_MARKDOWN_EMOJI.

    METHODS get_emoji_css
      RETURNING
        VALUE(rt_result) TYPE ty_code.

    METHODS get_emoji_list
      RETURNING
        VALUE(rt_result) TYPE ty_code.

    METHODS get_twemoji_css
      RETURNING
        VALUE(rt_result) TYPE ty_code.

    METHODS get_twemoji_list
      RETURNING
        VALUE(rt_result) TYPE ty_code.

    METHODS find_emoji
      IMPORTING
        !iv_regex        TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_results.

    METHODS format_emoji
      IMPORTING
        !iv_line         TYPE string
        !iv_base_url     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS find_twemoji
      IMPORTING
        !iv_regex        TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_results.

    METHODS format_twemoji
      IMPORTING
        !iv_line         TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_base_url TYPE string VALUE 'https://github.githubassets.com/images/icons/emoji/'.

    TYPES:
      BEGIN OF ty_emoji,
        name TYPE string,
        img  TYPE string,
      END OF ty_emoji.

    CLASS-DATA go_emoji TYPE REF TO ZCL_ABAPPM_MARKDOWN_EMOJI.

    DATA mt_emoji TYPE HASHED TABLE OF ty_emoji WITH UNIQUE KEY name.

    DATA mt_twemoji TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    METHODS init_emoji_list.

    METHODS init_twemoji_list.

    METHODS get_program_for_emoji_list
      RETURNING
        VALUE(rv_program) TYPE program.

    METHODS get_program_for_twemoji_list
      RETURNING
        VALUE(rv_program) TYPE program.

    METHODS get_program_for_twemoji_css
      RETURNING
        VALUE(rv_program) TYPE program.

ENDCLASS.



CLASS ZCL_ABAPPM_MARKDOWN_EMOJI IMPLEMENTATION.


  METHOD create.
    IF go_emoji IS INITIAL.
      CREATE OBJECT go_emoji.
      go_emoji->init_emoji_list( ).
      go_emoji->init_twemoji_list( ).
    ENDIF.
    ro_result = go_emoji.
  ENDMETHOD.


  METHOD find_emoji.

    FIELD-SYMBOLS <ls_emoji> LIKE LINE OF mt_emoji.

    LOOP AT mt_emoji ASSIGNING <ls_emoji>.
      IF find(
           val   = <ls_emoji>-name
           regex = iv_regex
           case  = abap_false ) >= 0.
        INSERT <ls_emoji>-name INTO TABLE rt_result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_twemoji.

    FIELD-SYMBOLS <lv_emoji> LIKE LINE OF mt_twemoji.

    LOOP AT mt_twemoji ASSIGNING <lv_emoji>.
      IF find(
           val   = <lv_emoji>
           regex = iv_regex
           case  = abap_false ) >= 0.
        INSERT <lv_emoji> INTO TABLE rt_result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_emoji.

    DATA:
      lv_base  TYPE string,
      lv_emoji TYPE string,
      lv_html  TYPE string.

    FIELD-SYMBOLS <ls_emoji> LIKE LINE OF mt_emoji.

    rv_result = iv_line.

    IF iv_base_url IS INITIAL.
      lv_base = c_base_url.
    ELSE.
      lv_base = iv_base_url.
    ENDIF.

    LOOP AT mt_emoji ASSIGNING <ls_emoji>.
      lv_emoji = |:{ <ls_emoji>-name }:|.
      lv_html = |<img src="{ lv_base }{ <ls_emoji>-img }" class="emoji">|.
      REPLACE ALL OCCURRENCES OF lv_emoji IN rv_result WITH lv_html.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_twemoji.

    DATA:
      lv_emoji TYPE string,
      lv_html  TYPE string.

    FIELD-SYMBOLS <lv_emoji> LIKE LINE OF mt_twemoji.

    rv_result = iv_line.

    LOOP AT mt_twemoji ASSIGNING <lv_emoji>.
      lv_emoji = |:{ <lv_emoji> }:|.
      lv_html = |<i class="twa twa-{ <lv_emoji> }"></i>|.
      REPLACE ALL OCCURRENCES OF lv_emoji IN rv_result WITH lv_html.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_emoji_css.
    INSERT `.emoji {` INTO TABLE rt_result.
    INSERT `  display: inline-block;` INTO TABLE rt_result.
    INSERT `  min-width: 1ch;` INTO TABLE rt_result.
    INSERT `  font-family: "Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";` INTO TABLE rt_result.
    INSERT `  font-size: 1em;` INTO TABLE rt_result.
    INSERT `  font-style: normal !important;` INTO TABLE rt_result.
    INSERT `  font-weight: 400;` INTO TABLE rt_result.
    INSERT `  height: 20px;` INTO TABLE rt_result.
    INSERT `  width: 20px;` INTO TABLE rt_result.
    INSERT `  line-height: 1;` INTO TABLE rt_result.
    INSERT `  vertical-align: -0.075em;` INTO TABLE rt_result.
    INSERT `}` INTO TABLE rt_result.
  ENDMETHOD.


  METHOD get_emoji_list.
    FIELD-SYMBOLS <ls_emoji> LIKE LINE OF mt_emoji.

    LOOP AT mt_emoji ASSIGNING <ls_emoji>.
      INSERT <ls_emoji>-name INTO TABLE rt_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_program_for_emoji_list.

    DATA:
      lv_descr TYPE string,
      lv_class TYPE seoclsname.

    lv_descr = cl_abap_classdescr=>get_class_name( me ).
    lv_class = lv_descr+7(*).

    rv_program = cl_oo_classname_service=>get_ccdef_name( lv_class ).

  ENDMETHOD.


  METHOD get_program_for_twemoji_css.

    DATA:
      lv_descr TYPE string,
      lv_class TYPE seoclsname.

    lv_descr = cl_abap_classdescr=>get_class_name( me ).
    lv_class = lv_descr+7(*).

    rv_program = cl_oo_classname_service=>get_ccmac_name( lv_class ).

  ENDMETHOD.


  METHOD get_program_for_twemoji_list.

    DATA:
      lv_descr TYPE string,
      lv_class TYPE seoclsname.

    lv_descr = cl_abap_classdescr=>get_class_name( me ).
    lv_class = lv_descr+7(*).

    rv_program = cl_oo_classname_service=>get_ccimp_name( lv_class ).

  ENDMETHOD.


  METHOD get_twemoji_css.

    DATA:
      lv_program TYPE program,
      lt_code    TYPE ty_code.

    FIELD-SYMBOLS <lv_line> TYPE LINE OF ty_code.

    lv_program = get_program_for_twemoji_css( ).

    READ REPORT lv_program INTO lt_code STATE 'A'.
    ASSERT sy-subrc = 0.

    LOOP AT lt_code ASSIGNING <lv_line>.
      IF strlen( <lv_line> ) > 2.
        INSERT <lv_line>+2(*) INTO TABLE rt_result.
      ELSE.
        INSERT `` INTO TABLE rt_result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_twemoji_list.
    rt_result = mt_twemoji.
  ENDMETHOD.


  METHOD init_emoji_list.

    DATA:
      lv_program TYPE program,
      ls_emoji   TYPE ty_emoji,
      lt_code    TYPE ty_code.

    FIELD-SYMBOLS <lv_line> TYPE LINE OF ty_code.

    CLEAR mt_emoji.

    lv_program = get_program_for_emoji_list( ).

    READ REPORT lv_program INTO lt_code STATE 'A'.
    ASSERT sy-subrc = 0.

    LOOP AT lt_code ASSIGNING <lv_line> WHERE table_line CP '" *'.
      FIND REGEX '"(.*)": "(.*)"' IN <lv_line>+3 SUBMATCHES ls_emoji-name ls_emoji-img.
      IF sy-subrc = 0.
        INSERT ls_emoji INTO TABLE mt_emoji.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD init_twemoji_list.

    DATA:
      lv_program TYPE program,
      lt_code    TYPE ty_code.

    FIELD-SYMBOLS <lv_line> TYPE LINE OF ty_code.

    CLEAR mt_twemoji.

    lv_program = get_program_for_twemoji_list( ).

    READ REPORT lv_program INTO lt_code STATE 'A'.
    ASSERT sy-subrc = 0.

    LOOP AT lt_code ASSIGNING <lv_line> WHERE table_line CP '" *'.
      INSERT <lv_line>+2(*) INTO TABLE mt_twemoji.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
