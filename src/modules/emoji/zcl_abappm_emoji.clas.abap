CLASS zcl_abappm_emoji DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* ABAP Emoji
*
* Support for Unicode Emoji (v16.0) and Twemoji (Emoji v14.0)
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Source locations used for Emoji data
*
* Local Defs: Unicode Emoji provided by GitHub
* Local Impl: Twemoji prodived by Twitter
* Macros: CSS for Twemoji
************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_code    TYPE STANDARD TABLE OF string WITH KEY table_line,
      ty_results TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.

    CONSTANTS c_version TYPE string VALUE '1.4.0' ##NEEDED.

    CONSTANTS:
      "! Emojinarium brands and naming
      "! 1: fast-forward-button
      "! 2: black-right-pointing-double-triangle
      BEGIN OF c_emojinarium_1,
        apple     TYPE c LENGTH 5 VALUE 'apple',
        google    TYPE c LENGTH 6 VALUE 'google',
        samsumg   TYPE c LENGTH 7 VALUE 'samsumg',
        whatsapp  TYPE c LENGTH 8 VALUE 'whatsapp',
        twitter   TYPE c LENGTH 7 VALUE 'twitter',
        facebook  TYPE c LENGTH 8 VALUE 'facebook',
        joypixels TYPE c LENGTH 9 VALUE 'joypixels',
        openmoji  TYPE c LENGTH 8 VALUE 'openmoji',
      END OF c_emojinarium_1,
      BEGIN OF c_emojinarium_2,
        microsoft TYPE c LENGTH 9 VALUE 'microsoft',
        emojidex  TYPE c LENGTH 8 VALUE 'emojidex',
        messenger TYPE c LENGTH 9 VALUE 'messenger',
        lg        TYPE c LENGTH 2 VALUE 'lg',
        mozilla   TYPE c LENGTH 7 VALUE 'mozilla',
        docomo    TYPE c LENGTH 6 VALUE 'docomo',
        au_kidi   TYPE c LENGTH 7 VALUE 'au-kidi',
      END OF c_emojinarium_2.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_emoji.

    METHODS get_emoji_css
      IMPORTING
        size_in_px    TYPE i DEFAULT 20
      RETURNING
        VALUE(result) TYPE ty_code.

    METHODS get_emoji_list
      RETURNING
        VALUE(result) TYPE ty_code.

    METHODS get_twemoji_css
      RETURNING
        VALUE(result) TYPE ty_code.

    METHODS get_twemoji_list
      RETURNING
        VALUE(result) TYPE ty_code.

    METHODS find_emoji
      IMPORTING
        !regex        TYPE string
      RETURNING
        VALUE(result) TYPE ty_results.

    METHODS format_emoji
      IMPORTING
        !line         TYPE string
        !base_url     TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS format_emojinarium
      IMPORTING
        !line         TYPE string
        !brand        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS find_twemoji
      IMPORTING
        !regex        TYPE string
      RETURNING
        VALUE(result) TYPE ty_results.

    METHODS format_twemoji
      IMPORTING
        !line         TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_base_url        TYPE string VALUE 'https://github.githubassets.com/images/icons/emoji',
      c_emojinarium_url TYPE string VALUE 'https://emojinarium.com/img'.

    TYPES:
      BEGIN OF ty_emoji,
        name TYPE string,
        img  TYPE string,
        code TYPE string,
      END OF ty_emoji.

    CLASS-DATA emoji TYPE REF TO zcl_abappm_emoji.

    DATA emojis TYPE HASHED TABLE OF ty_emoji
      WITH UNIQUE KEY name
      WITH NON-UNIQUE SORTED KEY code COMPONENTS code.

    DATA twemojis TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    METHODS init_emoji_list.

    METHODS init_twemoji_list.

    METHODS get_program_for_emoji_list
      RETURNING
        VALUE(result) TYPE program.

    METHODS get_program_for_twemoji_list
      RETURNING
        VALUE(result) TYPE program.

    METHODS get_program_for_twemoji_css
      RETURNING
        VALUE(result) TYPE program.

    METHODS unicode_to_string
      IMPORTING
        codepoint     TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_abappm_emoji IMPLEMENTATION.


  METHOD create.

    IF emoji IS INITIAL.
      emoji = NEW #( ).
      emoji->init_emoji_list( ).
      emoji->init_twemoji_list( ).
    ENDIF.

    result = emoji.

  ENDMETHOD.


  METHOD find_emoji.

    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      IF find(
           val   = <emoji>-name
           regex = regex
           case  = abap_false ) >= 0.
        INSERT <emoji>-name INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_twemoji.

    LOOP AT twemojis ASSIGNING FIELD-SYMBOL(<emoji>).
      IF find(
           val   = <emoji>
           regex = regex
           case  = abap_false ) >= 0.
        INSERT <emoji> INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_emoji.

    result = line.

    IF base_url IS INITIAL.
      DATA(base) = c_base_url.
    ELSE.
      base = base_url.
    ENDIF.
    DATA(len) = strlen( base ) - 1.
    IF base+len(1) <> '/'.
      base = base && '/'.
    ENDIF.

    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      DATA(emoji) = |:{ <emoji>-name }:|.
      DATA(html)  = |<img src="{ base }{ <emoji>-img }" class="emoji">|.
      REPLACE ALL OCCURRENCES OF emoji IN result WITH html.
      IF <emoji>-code IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF <emoji>-code IN result WITH html.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_emojinarium.

    result = line.

    IF brand IS INITIAL OR ( c_emojinarium_1 NS brand AND c_emojinarium_2 NS brand ).
      RETURN.
    ENDIF.

    " TODO: Only works with emoji names that have a single word i.e. "bikini"
    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      DATA(emoji) = |:{ <emoji>-name }:|.
      DATA(html)  = |<img src="{ c_emojinarium_url }/{ brand }/{ <emoji>-name }_{ <emoji>-code }" class="emoji">|.
      REPLACE ALL OCCURRENCES OF emoji IN result WITH html.
      IF <emoji>-code IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF <emoji>-code IN result WITH html.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD format_twemoji.

    result = line.

    LOOP AT twemojis ASSIGNING FIELD-SYMBOL(<emoji>).
      DATA(emoji) = |:{ <emoji> }:|.
      DATA(html)  = |<i class="twa twa-{ <emoji> }"></i>|.
      REPLACE ALL OCCURRENCES OF emoji IN result WITH html.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_emoji_css.
    INSERT `.emoji {` INTO TABLE result.
    INSERT `  display: inline-block;` INTO TABLE result.
    INSERT `  min-width: 1ch;` INTO TABLE result.
    INSERT `  font-family: "Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";` INTO TABLE result.
    INSERT `  font-size: 1em;` INTO TABLE result.
    INSERT `  font-style: normal !important;` INTO TABLE result.
    INSERT `  font-weight: 400;` INTO TABLE result.
    INSERT |  height: { size_in_px }px;| INTO TABLE result.
    INSERT |  width: { size_in_px }px;| INTO TABLE result.
    INSERT `  line-height: 1;` INTO TABLE result.
    INSERT `  vertical-align: -0.1em;` INTO TABLE result.
    INSERT `}` INTO TABLE result.
  ENDMETHOD.


  METHOD get_emoji_list.

    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      INSERT <emoji>-name INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_program_for_emoji_list.

    DATA(descr) = cl_abap_classdescr=>get_class_name( me ).
    DATA(class) = CONV seoclsname( descr+7(*) ).

    result = cl_oo_classname_service=>get_ccdef_name( class ).

  ENDMETHOD.


  METHOD get_program_for_twemoji_css.

    DATA(descr) = cl_abap_classdescr=>get_class_name( me ).
    DATA(class) = CONV seoclsname( descr+7(*) ).

    result = cl_oo_classname_service=>get_ccmac_name( class ).

  ENDMETHOD.


  METHOD get_program_for_twemoji_list.

    DATA(descr) = cl_abap_classdescr=>get_class_name( me ).
    DATA(class) = CONV seoclsname( descr+7(*) ).

    result = cl_oo_classname_service=>get_ccimp_name( class ).

  ENDMETHOD.


  METHOD get_twemoji_css.

    DATA code TYPE ty_code.

    DATA(program) = get_program_for_twemoji_css( ).

    READ REPORT program INTO code STATE 'A'.
    ASSERT sy-subrc = 0.

    LOOP AT code ASSIGNING FIELD-SYMBOL(<line>).
      IF strlen( <line> ) > 2.
        INSERT <line>+2(*) INTO TABLE result.
      ELSE.
        INSERT `` INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_twemoji_list.

    result = twemojis.

  ENDMETHOD.


  METHOD init_emoji_list.

    DATA:
      emoji TYPE ty_emoji,
      code  TYPE ty_code.

    CLEAR emojis.

    DATA(program) = get_program_for_emoji_list( ).

    READ REPORT program INTO code STATE 'A'.
    ASSERT sy-subrc = 0.

    LOOP AT code ASSIGNING FIELD-SYMBOL(<line>) WHERE table_line CS '": "'.
      CLEAR emoji.
      FIND REGEX '"(.*)": "(.*)"' IN <line>+3 SUBMATCHES emoji-name emoji-img.
      IF sy-subrc = 0.
        FIND REGEX '/(.*)\.png' IN emoji-img SUBMATCHES DATA(charcode).
        IF sy-subrc = 0.
          SPLIT charcode AT '-' INTO TABLE DATA(codes).
          LOOP AT codes INTO charcode.
            emoji-code = emoji-code && unicode_to_string( to_upper( charcode ) ).
          ENDLOOP.
        ENDIF.
        INSERT emoji INTO TABLE emojis.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD init_twemoji_list.

    DATA code TYPE ty_code.

    CLEAR twemojis.

    DATA(program) = get_program_for_twemoji_list( ).

    READ REPORT program INTO code STATE 'A'.
    ASSERT sy-subrc = 0.

    LOOP AT code ASSIGNING FIELD-SYMBOL(<line>) WHERE table_line CP '" *'.
      INSERT <line>+2(*) INTO TABLE twemojis.
    ENDLOOP.

  ENDMETHOD.


  METHOD unicode_to_string.

    CONSTANTS:
      x10000 TYPE x LENGTH 4 VALUE '00010000',
      xffff  TYPE x LENGTH 2 VALUE 'FFFF',
      xd800  TYPE x LENGTH 2 VALUE 'D800',
      xdc00  TYPE x LENGTH 2 VALUE 'DC00',
      x400   TYPE x LENGTH 2 VALUE '0400'.

    DATA:
      code_string    TYPE string,
      code_x         TYPE x LENGTH 4,
      code_i         TYPE i,
      high_surrogate TYPE xstring,
      low_surrogate  TYPE xstring,
      utf16          TYPE xstring.

    CHECK strlen( codepoint ) BETWEEN 1 AND 8.

    " Convert the Unicode code point string to an integer
    code_string = codepoint.
    DO 8 - strlen( codepoint ) TIMES.
      code_string = '0' && code_string.
    ENDDO.

    code_x = code_string.
    code_i = code_x.

    IF code_i > xffff.
      " Calculate high and low surrogate
      high_surrogate = xd800 + ( code_i - x10000 ) DIV x400.
      low_surrogate  = xdc00 + ( code_i - x10000 ) MOD x400.
      utf16          = high_surrogate && low_surrogate.
    ELSE.
      " Directly convert for non-surrogate values
      utf16 = code_x+2(2).
    ENDIF.

    result = cl_binary_convert=>xstring_utf16be_to_string( utf16 ).

  ENDMETHOD.
ENDCLASS.
