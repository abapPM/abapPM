CLASS zcl_abappm_emoji DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

********************************************************************************
* ABAP Emoji
*
* Support for Unicode Emoji and Twemoji (Emoji v14.0)
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Source locations used for Emoji data
*
* Local Defs: Unicode Emoji provided by GitHub
* Local Impl: Twemoji prodived by Twitter
* Macros: CSS for Twemoji
********************************************************************************
  PUBLIC SECTION.

    TYPES:
      ty_code    TYPE STANDARD TABLE OF string WITH KEY table_line,
      ty_results TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.

    CONSTANTS c_version TYPE string VALUE '1.4.0' ##NEEDED.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_emoji.

    METHODS get_emoji_css
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

    CONSTANTS c_base_url TYPE string VALUE 'https://github.githubassets.com/images/icons/emoji/'.

    TYPES:
      BEGIN OF ty_emoji,
        name TYPE string,
        img  TYPE string,
        code TYPE c LENGTH 2,
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

    LOOP AT emojis ASSIGNING FIELD-SYMBOL(<emoji>).
      DATA(emoji) = |:{ <emoji>-name }:|.
      DATA(html)  = |<img src="{ base }{ <emoji>-img }" class="emoji">|.
      REPLACE ALL OCCURRENCES OF emoji IN result WITH html.
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
    INSERT `  height: 20px;` INTO TABLE result.
    INSERT `  width: 20px;` INTO TABLE result.
    INSERT `  line-height: 1;` INTO TABLE result.
    INSERT `  vertical-align: -0.075em;` INTO TABLE result.
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

    LOOP AT code ASSIGNING FIELD-SYMBOL(<line>) WHERE table_line CP '" *'.
      FIND REGEX '"(.*)": "(.*)"' IN <line>+3 SUBMATCHES emoji-name emoji-img.
      IF sy-subrc = 0.
        FIND REGEX '/(.*)\.png' IN emoji-img SUBMATCHES DATA(unicode).
        IF sy-subrc = 0.
          emoji-code = ''.
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
ENDCLASS.
