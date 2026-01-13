CLASS /apmg/cl_apm_markdown DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* Markdown Renderer
*
* Original from https://github.com/koemaeda/abap-markdown
*
* Copyright (c) 2015 Guilherme Maeda
* SPDX-License-Identifier: MIT
************************************************************************
* Added by apm:
* - Option to render href and img src links with different root
* - Option to use sapevent for launching links in external browser
* - Option to set root path for internal links
* - Normalizing of link paths
* - Support for sapevent as protocol
* - Syntax highlighting (based on abapGit + diff + markdown)
* - Support for internal links (# Heading {#custom-id})
* - Support for strikethrough, subscript, superscript, highlight
* - Support for task list ([ ] or [x] task)
* - Fix a few regular expressions
* - Support for GitHub alerts
* - Fix for escaped | in tables
* - CSS
* - Remove variable prefixes, strict abaplint rules
************************************************************************
* TODO: Add "copy-to-clipboard" for code blocks
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    CLASS-METHODS styles
      RETURNING
        VALUE(result) TYPE string.

    METHODS text
      IMPORTING
        VALUE(text)   TYPE clike
      RETURNING
        VALUE(markup) TYPE string.

    METHODS set_breaks_enabled
      IMPORTING
        VALUE(breaks_enabled) TYPE clike
      RETURNING
        VALUE(result)         TYPE REF TO /apmg/cl_apm_markdown.

    METHODS set_markup_escaped
      IMPORTING
        VALUE(markup_escaped) TYPE clike
      RETURNING
        VALUE(result)         TYPE REF TO /apmg/cl_apm_markdown.

    METHODS set_urls_linked
      IMPORTING
        VALUE(urls_linked) TYPE clike
      RETURNING
        VALUE(result)      TYPE REF TO /apmg/cl_apm_markdown.

    METHODS set_safe_mode
      IMPORTING
        !iv_safe_mode TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_markdown.

    METHODS constructor
      IMPORTING
        !root_href TYPE string OPTIONAL
        !root_img  TYPE string OPTIONAL
        !path      TYPE string OPTIONAL
        !sapevent  TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_element_attribute,
        name  TYPE string,
        value TYPE string,
      END OF ty_element_attribute,
      ty_t_element_attribute TYPE STANDARD TABLE OF ty_element_attribute WITH KEY name.

    TYPES:
      BEGIN OF ty_element0,
        name       TYPE string,
        handler    TYPE string,
        attributes TYPE ty_t_element_attribute,
        text       TYPE string,
        lines      TYPE string_table,
      END OF ty_element0,
      ty_t_element0 TYPE STANDARD TABLE OF ty_element0 WITH KEY name.

    TYPES:
      BEGIN OF ty_element1,
        name       TYPE string,
        handler    TYPE string,
        attributes TYPE ty_t_element_attribute,
        text       TYPE string,
        texts      TYPE ty_t_element0,
        lines      TYPE string_table,
      END OF ty_element1,
      ty_t_element1 TYPE STANDARD TABLE OF ty_element1 WITH KEY name.

    TYPES:
      BEGIN OF ty_element2,
        name       TYPE string,
        handler    TYPE string,
        attributes TYPE ty_t_element_attribute,
        text       TYPE string,
        texts      TYPE ty_t_element1,
        lines      TYPE string_table,
      END OF ty_element2,
      ty_t_element2 TYPE STANDARD TABLE OF ty_element2 WITH KEY name.

    TYPES:
      BEGIN OF ty_element3,
        name       TYPE string,
        handler    TYPE string,
        attributes TYPE ty_t_element_attribute,
        text       TYPE string,
        texts      TYPE ty_t_element2,
        lines      TYPE string_table,
      END OF ty_element3,
      ty_t_element3 TYPE STANDARD TABLE OF ty_element3 WITH KEY name.

    TYPES:
      BEGIN OF ty_element4,
        name       TYPE string,
        handler    TYPE string,
        attributes TYPE ty_t_element_attribute,
        text       TYPE string,
        texts      TYPE ty_t_element3,
        lines      TYPE string_table,
      END OF ty_element4,
      ty_t_element4 TYPE STANDARD TABLE OF ty_element4 WITH KEY name.

    TYPES:
      BEGIN OF ty_element5,
        name       TYPE string,
        handler    TYPE string,
        attributes TYPE ty_t_element_attribute,
        text       TYPE ty_element4,
        texts      TYPE ty_t_element4,
        lines      TYPE string_table,
      END OF ty_element5.

    TYPES ty_element TYPE ty_element5.

    TYPES:
      BEGIN OF ty_block,
        "// general block fields
        continuable TYPE abap_bool,
        identified  TYPE abap_bool,
        interrupted TYPE abap_bool,
        hidden      TYPE abap_bool,
        closed      TYPE abap_bool,
        type        TYPE string,
        markup      TYPE string,
        element     TYPE ty_element,
        "// specific block fields
        char        TYPE c LENGTH 1,
        complete    TYPE abap_bool,
        indent      TYPE i,
        pattern     TYPE string,
        li          TYPE ty_element4,
        loose       TYPE abap_bool,
        name        TYPE string,
        depth       TYPE i,
        void        TYPE abap_bool,
        alignments  TYPE string_table,
      END OF ty_block.

    TYPES:
      BEGIN OF ty_line,
        body   TYPE string,
        indent TYPE i,
        text   TYPE string,
      END OF ty_line.

    TYPES:
      BEGIN OF ty_excerpt,
        text    TYPE string,
        context TYPE string,
      END OF ty_excerpt.

    TYPES:
      BEGIN OF ty_inline,
        position TYPE i,
        markup   TYPE string,
        extent   TYPE string,
        element  TYPE ty_element,
      END OF ty_inline.

    ">>> apm
    DATA:
      BEGIN OF config,
        root_href TYPE string,
        root_img  TYPE string,
        sapevent  TYPE abap_bool,
        path      TYPE string,
        path_util TYPE REF TO /apmg/cl_apm_markdown_path,
      END OF config.
    "<<< apm

    DATA breaks_enabled TYPE abap_bool.
    DATA markup_escaped TYPE abap_bool.
    DATA urls_linked TYPE abap_bool VALUE abap_true.
    DATA safe_mode TYPE abap_bool.
    DATA block_types TYPE REF TO lcl_hashmap.
    DATA unmarked_block_types TYPE REF TO lcl_string_array.
    DATA inline_types TYPE REF TO lcl_hashmap.
    "DATA inline_marker_list TYPE string VALUE '!"*_&[:<>`~\\' ##NO_TEXT
    DATA inline_marker_list TYPE string VALUE '!"*_&[:<>`~\\=^' ##NO_TEXT. " apm
    DATA definition_data TYPE REF TO lcl_hashmap.
    DATA special_characters TYPE REF TO lcl_string_array.
    DATA strong_regex TYPE REF TO lcl_hashmap.
    DATA em_regex TYPE REF TO lcl_hashmap.
    DATA regex_html_attribute TYPE string
      VALUE '[a-zA-Z_:][\w:.-]*(?:\s*=\s*(?:[^"''=<>`\s]+|"[^"]*"|''[^'']*''))?' ##NO_TEXT.
    DATA void_elements TYPE REF TO lcl_string_array.
    DATA text_level_elements TYPE REF TO lcl_string_array.
    DATA safe_links_whitelist TYPE REF TO lcl_string_array.
    DATA methods TYPE string_table.

    CLASS-METHODS htmlspecialchars
      IMPORTING
        !input        TYPE string
        !ent_html401  TYPE abap_bool DEFAULT abap_true
        !ent_noquotes TYPE abap_bool OPTIONAL
        !ent_quotes   TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS trim
      IMPORTING
        !str          TYPE string
        VALUE(mask)   TYPE string DEFAULT ' \t\n\r'
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS chop
      IMPORTING
        !str          TYPE string
        VALUE(mask)   TYPE string DEFAULT ' \t\n\r'
      RETURNING
        VALUE(result) TYPE string ##CALLED.

    CLASS-METHODS magic_move
      IMPORTING
        !from TYPE any
        !name TYPE clike OPTIONAL
      CHANGING
        !to   TYPE any.

    CLASS-METHODS match_marked_string
      IMPORTING
        !marker          TYPE string
        !subject         TYPE string
      EXPORTING
        VALUE(m0)        TYPE string
        VALUE(m1)        TYPE string
        VALUE(not_found) TYPE abap_bool.

    CLASS-METHODS _escape
      IMPORTING
        !text         TYPE string
        !allow_quotes TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS string_at_start
      IMPORTING
        !haystack     TYPE string
        !needle       TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS _lines
      IMPORTING
        !lines        TYPE STANDARD TABLE
      RETURNING
        VALUE(result) TYPE string.

    " Dynamically called methods

    METHODS block_code
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_code_continue
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_code_complete
      IMPORTING
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_comment
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_comment_continue
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_fencedcode
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_fencedcode_continue
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_fencedcode_complete
      IMPORTING
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_header
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_list
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_list_continue
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_list_complete
      IMPORTING
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_quote
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_quote_complete
      IMPORTING
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_quote_continue
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_rule
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_setextheader
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_markup
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_markup_continue
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS block_reference
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_table
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED ##NEEDED.

    METHODS block_table_continue
      IMPORTING
        !line         TYPE ty_line
        !block        TYPE ty_block
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS paragraph
      IMPORTING
        !line         TYPE ty_line
      RETURNING
        VALUE(result) TYPE ty_block ##CALLED.

    METHODS line
      IMPORTING
        !element      TYPE ty_element4
      RETURNING
        VALUE(result) TYPE string ##CALLED.

    METHODS inline_code
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_emailtag
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_emphasis
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_escapesequence
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_image
      IMPORTING
        VALUE(excerpt) TYPE ty_excerpt
      RETURNING
        VALUE(result)  TYPE ty_inline ##CALLED.

    METHODS inline_link
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_markup
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_specialcharacter
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_strikethrough
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_url
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    METHODS inline_urltag
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.

    ">>> apm
    METHODS inline_highlight
      IMPORTING
        !excerpt      TYPE ty_excerpt
      RETURNING
        VALUE(result) TYPE ty_inline ##CALLED.
    "<<< apm

    METHODS unmarked_text
      IMPORTING
        !text         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS element
      IMPORTING
        !element      TYPE any
      RETURNING
        VALUE(result) TYPE string.

    METHODS elements
      IMPORTING
        !elements     TYPE STANDARD TABLE
      RETURNING
        VALUE(result) TYPE string ##CALLED.

    METHODS li
      IMPORTING
        !lines        TYPE STANDARD TABLE
      RETURNING
        VALUE(result) TYPE string ##CALLED.

    METHODS filter_unsafe_url_in_attribute
      IMPORTING
        !element      TYPE ty_element
        !attribute    TYPE string
      RETURNING
        VALUE(result) TYPE ty_element.

    METHODS sanitise_element
      IMPORTING
        !element      TYPE ty_element
      RETURNING
        VALUE(result) TYPE ty_element.

    ">>> apm
    METHODS _adjust_link
      IMPORTING
        !root         TYPE string
        !source       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS _adjust_a_href
      IMPORTING
        !source       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS _adjust_img_src
      IMPORTING
        !source       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS _adjust_markup
      IMPORTING
        !source       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS syntax_highlighter
      IMPORTING
        !element      TYPE any
      RETURNING
        VALUE(result) TYPE string ##CALLED.
    "<<< apm
ENDCLASS.



CLASS /apmg/cl_apm_markdown IMPLEMENTATION.


  METHOD block_code.
    IF block IS NOT INITIAL AND
       block-type IS INITIAL AND
       block-interrupted IS INITIAL.
      RETURN.
    ENDIF.

    IF line-indent >= 4.
      result-element-name = 'pre'.
      result-element-handler = 'element'.
      result-element-text-name = 'code'.
      result-element-text-text = line-body+4.
    ENDIF.
  ENDMETHOD.                    "block_code


  METHOD block_code_complete.
    result = block.
    result-element-text-text = result-element-text-text.
  ENDMETHOD.                    "block_code_complete


  METHOD block_code_continue.
    DATA text TYPE string.

    IF line-indent >= 4.
      result = block.

      IF block-interrupted IS NOT INITIAL.
        CONCATENATE result-element-text-text %_newline
          INTO result-element-text-text RESPECTING BLANKS.
        CLEAR result-interrupted.
      ENDIF.

      text = line-body+4.
      CONCATENATE result-element-text-text %_newline text
        INTO result-element-text-text RESPECTING BLANKS.
    ENDIF.
  ENDMETHOD.                    "block_code_continue


  METHOD block_comment.
    CHECK markup_escaped IS INITIAL.

    IF strlen( line-text ) >= 3 AND
       line-text+3(1) = '-' AND
       line-text+2(1) = '-' AND
       line-text+1(1) = '!'.
      result-markup = line-body.

      FIND REGEX '-->$' IN line-text ##REGEX_POSIX.
      IF sy-subrc = 0.
        result-closed = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "block_Comment


  METHOD block_comment_continue.
    CHECK block-closed IS INITIAL.
    result = block.

    CONCATENATE result-markup %_newline line-body INTO result-markup.

    FIND REGEX '-->\s*$' IN line-text ##REGEX_POSIX. " apm
    IF sy-subrc = 0.
      result-closed = abap_true.
    ENDIF.
  ENDMETHOD.                    "block_Comment_Continue


  METHOD block_fencedcode.
    DATA:
      regex TYPE string,
      m1    TYPE string.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-text-attributes.

    regex = '^[' && line-text(1) && ']{3,}[ ]*([^`]+)?[ ]*$'.
    FIND REGEX regex IN line-text SUBMATCHES m1 ##REGEX_POSIX.
    IF sy-subrc = 0.
      IF m1 IS NOT INITIAL.
        APPEND INITIAL LINE TO result-element-text-attributes ASSIGNING <attribute>.
        <attribute>-name = 'class'.
        CONCATENATE 'language-' m1 INTO <attribute>-value.
      ENDIF.

      result-char = line-text(1).
      result-element-name = 'pre'.
      result-element-handler = 'element'.
      result-element-text-name = 'code'.
      result-element-text-handler = 'syntax_highlighter'. " apm
    ENDIF.
  ENDMETHOD.                    "block_Fenced_Code


  METHOD block_fencedcode_complete.
    result = block.
    result-element-text-text = result-element-text-text.
  ENDMETHOD.                    "block_Fenced_Code_Complete


  METHOD block_fencedcode_continue.
    DATA regex TYPE string.

    CHECK block-complete IS INITIAL.
    result = block.

    IF result-interrupted IS NOT INITIAL.
      CONCATENATE result-element-text-text %_newline INTO result-element-text-text.
      CLEAR result-interrupted.
    ENDIF.

    CONCATENATE '^' block-char '{3,}[ ]*$' INTO regex.
    FIND REGEX regex IN line-text ##REGEX_POSIX.
    IF sy-subrc = 0.
      result-element-text-text = result-element-text-text+1.
      result-complete = abap_true.
      RETURN.
    ENDIF.

    CONCATENATE
      result-element-text-text %_newline line-body
    INTO result-element-text-text.
  ENDMETHOD.                    "block_Fenced_Code_Continue


  METHOD block_header.
    DATA:
      level   TYPE i VALUE 1,
      h_level TYPE n,
      pos     TYPE i,
      id      TYPE string.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-attributes.

    CHECK strlen( line-text ) > 1 AND line-text+1(1) IS NOT INITIAL.

    WHILE strlen( line-text ) > level AND line-text+level(1) = '#'.
      level = level + 1.
    ENDWHILE.

    CHECK level <= 6.

    h_level = level.
    CONCATENATE 'h' h_level INTO result-element-name.
    result-element-text-text = line-text.
    result-element-text-text = trim(
      str  = result-element-text-text
      mask = ' #' ).
    CONDENSE result-element-text-text.
    result-element-handler = 'line'.

    ">>> apm
    FIND REGEX ' \{(#[\w-]*)\}' IN result-element-text-text IGNORING CASE
      MATCH OFFSET pos SUBMATCHES id ##REGEX_POSIX.
    IF sy-subrc = 0.
      " # Heading {#custom-id}
      APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
      <attribute>-name = 'id'.
      <attribute>-value = id.
      result-element-text-text = result-element-text-text(pos).
    ELSE.
      id = replace(
        val   = to_lower( result-element-text-text )
        regex = `[^\w\s\-]`
        with  = ``
        occ   = 0 ) ##REGEX_POSIX.
      id = replace(
        val   = id
        regex = `\s`
        with  = `-`
        occ   = 0 ) ##REGEX_POSIX.
      " If there are HTML tags, no id
      IF id CA '<>'.
        id = '#'.
      ENDIF.
      APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
      <attribute>-name = 'id'.
      <attribute>-value = id.
    ENDIF.
    "<<< apm
  ENDMETHOD.                    "block_Header


  METHOD block_list.
    DATA:
      name       TYPE string,
      pattern    TYPE string,
      regex      TYPE string,
      m1         TYPE string,
      m2         TYPE string,
      list_start TYPE string.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-attributes.

    IF line-text(1) <= '-'.
      name = 'ul'.
      pattern = '[*+-]'.
    ELSE.
      name = 'ol'.
      pattern = '[0-9]+[.]'.
    ENDIF.

    regex = '^(' && pattern && '[ ]+)(.*)'.
    FIND REGEX regex IN line-text SUBMATCHES m1 m2 ##REGEX_POSIX.
    IF sy-subrc = 0.
      result-indent = line-indent.

      ">>> apm
      " We could distinguish between *,+,- lists
      "IF name = 'ul'
      "  result-pattern = |[{ m1(1) }]|
      "ELSE
      result-pattern = pattern.
      "ENDIF
      "<<< apm

      result-element-name = name.
      result-element-handler = 'elements'.

      IF result-element-name = 'ol'.
        list_start = substring_before(
          val  = line-text
          sub  = '.'
          case = abap_false ).
        IF list_start <> '1'.
          APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
          <attribute>-name = 'start'.
          <attribute>-value = list_start.
        ENDIF.
      ENDIF.

      result-li-name = 'li'.
      result-li-handler = 'li'.
      APPEND m2 TO result-li-lines.
    ENDIF.
  ENDMETHOD.                    "block_List


  METHOD block_list_complete.
    FIELD-SYMBOLS:
      <li>        LIKE LINE OF result-element-texts,
      <last_line> LIKE LINE OF <li>-lines.

    result = block.

    APPEND result-li TO result-element-texts.

    IF result-loose IS NOT INITIAL.
      LOOP AT result-element-texts ASSIGNING <li>.
        READ TABLE <li>-lines INDEX lines( result-li-lines ) ASSIGNING <last_line>.
        IF sy-subrc = 0 AND <last_line> IS NOT INITIAL.
          APPEND INITIAL LINE TO <li>-lines.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "block_List_complete


  METHOD block_list_continue.
    DATA:
      regex     TYPE string,
      m1        TYPE string,
      text      TYPE string,
      ref_block TYPE ty_block.

    result = block.

    regex = '^' && block-pattern && '(?:[ ]+(.*)|$)'.
    IF block-indent = line-indent.
      FIND REGEX regex IN line-text SUBMATCHES m1 ##REGEX_POSIX.
      IF sy-subrc = 0.
        IF result-interrupted IS NOT INITIAL.
          APPEND INITIAL LINE TO result-li-lines.
          result-loose = abap_true.
          CLEAR result-interrupted.
        ENDIF.
        APPEND result-li TO result-element-texts.

        CLEAR result-li.
        result-li-name = 'li'.
        result-li-handler = 'li'.
        APPEND m1 TO result-li-lines.
        RETURN.
      ENDIF.
    ENDIF.

    IF line-text(1) = '['.
      ref_block = block_reference( line ).
      IF ref_block IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF result-interrupted IS INITIAL.
      text = line-body.
      REPLACE ALL OCCURRENCES OF REGEX '^[ ]{0,4}' IN text WITH '' ##REGEX_POSIX.
      APPEND text TO result-li-lines.
      RETURN.
    ENDIF.

    IF line-indent > 0.
      APPEND INITIAL LINE TO result-li-lines.
      text = line-body.
      REPLACE ALL OCCURRENCES OF REGEX '^[ ]{0,4}' IN text WITH '' ##REGEX_POSIX.
      APPEND text TO result-li-lines.
      CLEAR result-interrupted.
      RETURN.
    ENDIF.

    CLEAR result.
  ENDMETHOD.                    "block_List_Continue


  METHOD block_markup.
    DATA:
      regex             TYPE string,
      m1                TYPE string,
      m2                TYPE string,
      length            TYPE i,
      index             TYPE i,
      remainder         TYPE string,
      remainder_trimmed TYPE string.

    CHECK: markup_escaped IS INITIAL,
           safe_mode IS INITIAL.

    regex = '^<(\w*)(?:[ ]*' && regex_html_attribute && ')*[ ]*(/)?>'.
    FIND FIRST OCCURRENCE OF REGEX regex IN line-text SUBMATCHES m1 m2
      MATCH LENGTH length ##REGEX_POSIX.
    IF sy-subrc = 0.

      index = text_level_elements->find_val( m1 ).
      CHECK index = 0.

      result-name = m1.
      result-depth = 0.
      result-markup = _adjust_markup( line-text ). " apm

      remainder = line-text+length.
      remainder_trimmed = trim( remainder ).

      index = void_elements->find_val( m1 ).

      IF remainder_trimmed IS INITIAL.
        IF m2 IS NOT INITIAL OR index <> 0.
          result-closed = abap_true.
          result-void = abap_true.
        ENDIF.
      ELSE.
        IF m2 IS NOT INITIAL OR index <> 0.
          CLEAR result.
          RETURN.
        ENDIF.

        CONCATENATE '</' m1 '>[ ]*$' INTO regex.
        FIND FIRST OCCURRENCE OF REGEX regex IN remainder IGNORING CASE ##REGEX_POSIX.
        IF sy-subrc = 0.
          result-closed = abap_true.
        ENDIF.
      ENDIF.

    ENDIF. "regex sy-subrc = 0
  ENDMETHOD.                    "block_Markup


  METHOD block_markup_continue.
    DATA:
      regex TYPE string,
      body  TYPE string.

    CHECK block-closed IS INITIAL.

    result = block.

    CONCATENATE '^<' result-name '(?:[ ]*' regex_html_attribute ')*[ ]*>' INTO regex.
    FIND REGEX regex IN line-text IGNORING CASE ##REGEX_POSIX. "open
    IF sy-subrc = 0.
      result-depth = result-depth + 1.
    ENDIF.

    CONCATENATE '</' result-name '>[ ]*$' INTO regex.
    FIND REGEX regex IN line-text IGNORING CASE ##REGEX_POSIX. "close
    IF sy-subrc = 0.
      IF result-depth > 0.
        result-depth = result-depth - 1.
      ELSE.
        result-closed = abap_true.
      ENDIF.
    ENDIF.

    IF result-interrupted IS NOT INITIAL.
      CONCATENATE result-markup %_newline INTO result-markup.
      CLEAR result-interrupted.
    ENDIF.

    ">>> apm
    body = _adjust_markup( line-body ).
    CONCATENATE result-markup %_newline body INTO result-markup.
    "<<< apm
  ENDMETHOD.                    "block_Markup_Continue


  METHOD block_quote.
    DATA m1 TYPE string.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-attributes.

    FIND REGEX '^>[ ]?(.*)' IN line-text SUBMATCHES m1 ##REGEX_POSIX.
    IF sy-subrc = 0.
      SHIFT m1 LEFT DELETING LEADING space.
      result-element-name = 'blockquote'.
      result-element-handler = '_lines'.
      " >>> apm
      IF lcl_alerts=>get( m1 ) IS NOT INITIAL.
        APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
        <attribute>-name  = 'class'.
        <attribute>-value = lcl_alerts=>get( m1 )-class.
        m1 = lcl_alerts=>get( m1 )-tag.
      ENDIF.
      " <<< apm
      APPEND m1 TO result-element-lines.
    ENDIF.
  ENDMETHOD.                    "block_Quote


  METHOD block_quote_complete.
    result = block.
  ENDMETHOD.


  METHOD block_quote_continue.
    DATA m1 TYPE string.

    IF line-text(1) = '>'.
      result = block.
      FIND REGEX '^>[ ]?(.*)' IN line-text SUBMATCHES m1 ##REGEX_POSIX.
      IF sy-subrc = 0.
        " >>> apm
        IF lcl_alerts=>get( m1 ) IS NOT INITIAL.
          CLEAR result.
          RETURN.
        ENDIF.
        " <<< apm
        SHIFT m1 LEFT DELETING LEADING space.
        IF result-interrupted IS NOT INITIAL.
          APPEND INITIAL LINE TO result-element-lines.
          CLEAR result-interrupted.
        ENDIF.

        APPEND m1 TO result-element-lines.
        RETURN.
      ENDIF.
    ENDIF.

    IF block-interrupted IS INITIAL.
      result = block.
      APPEND line-text TO result-element-lines.
    ENDIF.
  ENDMETHOD.                    "block_Quote_Continue


  METHOD block_reference.
    DATA:
      m1       TYPE string,
      m2       TYPE string,
      m3       TYPE string,
      m4       TYPE string,
      id       TYPE string,
      ref_map  TYPE REF TO lcl_hashmap,
      ref_item TYPE REF TO lcl_hashmap,
      ref_val  TYPE REF TO lcl_string.

    FIND REGEX '^\[(.+)\]:[ ]*<?(\S+)>?([ ]+["''(](.+)["'')])?[ ]*$'
      IN line-text SUBMATCHES m1 m2 m3 m4 ##REGEX_POSIX.
    IF sy-subrc = 0.
      id = m1.
      TRANSLATE id TO LOWER CASE.

      ref_map ?= definition_data->get( 'Reference' ).
      ref_item ?= ref_map->get( id ).

      ref_val ?= ref_item->get( 'url' ).
      ref_val->set_data( m2 ).
      IF m3 IS NOT INITIAL.
        ref_val ?= ref_item->get( 'title' ).
        ref_val->set_data( m4 ).
      ENDIF.

      result-hidden = abap_true.
    ENDIF.
  ENDMETHOD.                    "block_Reference


  METHOD block_rule.
    DATA regex TYPE string.

    CONCATENATE '^([' line-text(1) '])([ ]*\1){2,}[ ]*$' INTO regex.
    FIND REGEX regex IN line-text ##REGEX_POSIX.
    IF sy-subrc = 0.
      result-element-name = 'hr'.
    ENDIF.
  ENDMETHOD.                    "block_Rule


  METHOD block_setextheader.
    CHECK block IS NOT INITIAL AND block-type IS INITIAL AND
          block-interrupted IS INITIAL.

    IF line-text CO line-text(1).
      result = block.
      IF line-text(1) = '='.
        result-element-name = 'h1'.
      ELSE.
        result-element-name = 'h2'.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "block_SetextHeader


  METHOD block_table.
    DATA:
      divider       TYPE string,
      divider_cells TYPE string_table,
      len           TYPE i,
      header        TYPE string,
      header_cells  TYPE string_table,
      index         TYPE i,
      headeresults  TYPE ty_t_element2.

    FIELD-SYMBOLS:
      <header_cell>   LIKE LINE OF header_cells,
      <headeresult>   LIKE LINE OF headeresults,
      <attribute>     LIKE LINE OF <headeresult>-attributes,
      <divider_cell>  LIKE LINE OF divider_cells,
      <alignment>     LIKE LINE OF result-alignments,
      <element_text1> LIKE LINE OF result-element-texts,
      <element_text2> LIKE LINE OF <element_text1>-texts.

    CHECK NOT ( block IS INITIAL OR
                block-type IS NOT INITIAL OR
                block-interrupted IS NOT INITIAL ).

    FIND '|' IN block-element-text-text.
    IF sy-subrc = 0 AND line-text CO ' -:|'.
      result = block.

      divider = trim( line-text ).
      divider = trim(
        str  = divider
        mask = '|' ).

      " >>> apm
      " Replace escaped \|
      divider = replace(
        val  = divider
        sub  = '\|'
        with = '%bar%'
        occ  = 0 ).
      " <<< apm
      SPLIT divider AT '|' INTO TABLE divider_cells.
      LOOP AT divider_cells ASSIGNING <divider_cell>.
        <divider_cell> = trim( <divider_cell> ).
        <divider_cell> = replace(
          val  = <divider_cell>
          sub  = '%bar%'
          with = '|'
          occ  = 0 ). " apm
        CHECK <divider_cell> IS NOT INITIAL.
        APPEND INITIAL LINE TO result-alignments ASSIGNING <alignment>.

        IF <divider_cell>(1) = ':'.
          <alignment> = 'left'.
        ENDIF.

        len = strlen( <divider_cell> ) - 1.
        IF <divider_cell>+len(1) = ':'.
          IF <alignment> = 'left'.
            <alignment> = 'center'.
          ELSE.
            <alignment> = 'right'.
          ENDIF.
        ENDIF.
      ENDLOOP. "lt_divider_cells

      " ~

      header = trim( result-element-text-text ).
      header = trim(
        str  = header
        mask = '|' ).

      " >>> apm
      " Replace escaped \|
      header = replace(
        val  = header
        sub  = '\|'
        with = '%bar%'
        occ  = 0 ).
      " <<< apm
      SPLIT header AT '|' INTO TABLE header_cells.
      LOOP AT header_cells ASSIGNING <header_cell>.
        index = sy-tabix.
        <header_cell> = trim( <header_cell> ).
        <header_cell> = replace(
          val  = <header_cell>
          sub  = '%bar%'
          with = '|'
          occ  = 0 ). " apm

        APPEND INITIAL LINE TO headeresults ASSIGNING <headeresult>.
        <headeresult>-name = 'th'.
        <headeresult>-text = <header_cell>.
        <headeresult>-handler = 'line'.

        READ TABLE result-alignments ASSIGNING <alignment> INDEX index.
        IF sy-subrc = 0 AND <alignment> IS NOT INITIAL.
          APPEND INITIAL LINE TO <headeresult>-attributes ASSIGNING <attribute>.
          <attribute>-name = 'style'.
          CONCATENATE 'text-align: ' <alignment> ';'
            INTO <attribute>-value RESPECTING BLANKS.
        ENDIF.

      ENDLOOP.

      " ~

      result-identified = abap_true.
      result-element-name = 'table'.
      result-element-handler = 'elements'.

      APPEND INITIAL LINE TO result-element-texts ASSIGNING <element_text1>.
      <element_text1>-name = 'thead'.
      <element_text1>-handler = 'elements'.
      APPEND INITIAL LINE TO <element_text1>-texts ASSIGNING <element_text2>.
      <element_text2>-name = 'tr'.
      <element_text2>-handler = 'elements'.
      <element_text2>-texts = headeresults.

      APPEND INITIAL LINE TO result-element-texts ASSIGNING <element_text1>.
      <element_text1>-name = 'tbody'.
      <element_text1>-handler = 'elements'.
    ENDIF. "sy-subrc = 0 and line-text na ' -:|'.
  ENDMETHOD.                    "block_Table


  METHOD block_table_continue.
    DATA:
      row     TYPE string,
      matches TYPE match_result_tab,
      index   TYPE i,
      cell    TYPE string.

    FIELD-SYMBOLS:
      <match>     LIKE LINE OF matches,
      <text1>     LIKE LINE OF result-element-texts,
      <text2>     LIKE LINE OF <text1>-texts,
      <text3>     LIKE LINE OF <text2>-texts,
      <alignment> LIKE LINE OF result-alignments,
      <attribute> LIKE LINE OF <text3>-attributes.

    CHECK block-interrupted IS INITIAL.

    IF line-text CS '|'.
      result = block.

      row = trim( line-text ).
      row = trim(
        str  = row
        mask = '|' ).

      READ TABLE result-element-texts ASSIGNING <text1> INDEX 2.
      CHECK sy-subrc = 0.

      APPEND INITIAL LINE TO <text1>-texts ASSIGNING <text2>.
      <text2>-name = 'tr'.
      <text2>-handler = 'elements'.

      " >>> apm
      " Replace escaped \|
      row = replace(
        val  = row
        sub  = '\|'
        with = '%bar%'
        occ  = 0 ).
      " REGEX '(?:(\\[|])|[^|`]|`[^`]+`|`)+' is too greedy
      FIND ALL OCCURRENCES OF REGEX '(?:(\\[|])|[^|])+'
        IN row RESULTS matches ##SUBRC_OK ##REGEX_POSIX.
      " <<< apm
      LOOP AT matches ASSIGNING <match>.
        index = sy-tabix.
        cell = row+<match>-offset(<match>-length).
        cell = trim( cell ).

        cell = replace(
          val  = cell
          sub  = '%bar%'
          with = '|'
          occ  = 0 ). " apm

        APPEND INITIAL LINE TO <text2>-texts ASSIGNING <text3>.
        <text3>-name = 'td'.
        <text3>-handler = 'line'.
        <text3>-text = cell.

        READ TABLE result-alignments ASSIGNING <alignment> INDEX index.
        IF sy-subrc = 0 AND <alignment> IS NOT INITIAL.
          APPEND INITIAL LINE TO <text3>-attributes ASSIGNING <attribute>.
          <attribute>-name = 'style'.
          CONCATENATE 'text-align: ' <alignment> ';'
            INTO <attribute>-value RESPECTING BLANKS.
        ENDIF.
      ENDLOOP. "lt_matches
    ENDIF. "line-text cs '|'
  ENDMETHOD.                    "block_Table_Continue


  METHOD chop.
    DATA regex TYPE string.

    result = str.
    REPLACE ALL OCCURRENCES OF REGEX '([\.\?\*\+\|])' IN mask WITH '\\$1' ##REGEX_POSIX.
    CONCATENATE '[' mask ']*\Z' INTO regex.
    REPLACE ALL OCCURRENCES OF REGEX regex IN result WITH '' ##REGEX_POSIX.
  ENDMETHOD.                    "trim


  METHOD constructor.
    " Constuctor method
    " Initializes the instance constants
    DATA:
      ref_sa   TYPE REF TO lcl_string_array,
      string   TYPE REF TO lcl_string,
      objdescr TYPE REF TO cl_abap_objectdescr.

    FIELD-SYMBOLS <method> LIKE LINE OF objdescr->methods.

    ">>> apm
    config-root_href = root_href.
    config-root_img  = root_img.
    config-path      = path.
    config-sapevent  = sapevent.

    config-path_util = NEW #( ).
    "<<< apm

    "
    " Lines
    "
    block_types  = NEW #( value_type = 'lcl_string_array' ).

    ref_sa ?= block_types->new( '#' ).
    ref_sa->append( 'Header' ).
    ref_sa ?= block_types->new( '*' ).
    ref_sa->append( 'Rule' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '+' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '-' ).
    ref_sa->append( 'SetextHeader' ).
    ref_sa->append( 'Table' ).
    ref_sa->append( 'Rule' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '0' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '1' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '2' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '3' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '4' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '5' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '6' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '7' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '8' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( '9' ).
    ref_sa->append( 'List' ).
    ref_sa ?= block_types->new( ':' ).
    ref_sa->append( 'Table' ).
    ref_sa ?= block_types->new( '<' ).
    ref_sa->append( 'Comment' ).
    ref_sa->append( 'Markup' ).
    ref_sa ?= block_types->new( '=' ).
    ref_sa->append( 'SetextHeader' ).
    ref_sa ?= block_types->new( '>' ).
    ref_sa->append( 'Quote' ).
    ref_sa ?= block_types->new( '[' ).
    ref_sa->append( 'Reference' ).
    ref_sa ?= block_types->new( '_' ).
    ref_sa->append( 'Rule' ).
    ref_sa ?= block_types->new( '`' ).
    ref_sa->append( 'FencedCode' ).
    ref_sa ?= block_types->new( '|' ).
    ref_sa->append( 'Table' ).
    ref_sa ?= block_types->new( '~' ).
    ref_sa->append( 'FencedCode' ).

    unmarked_block_types = NEW #( ).
    unmarked_block_types->append( 'Code' ).

    "
    " Inline Elements
    "
    inline_types = NEW #( value_type = 'lcl_string_array' ).

    ref_sa ?= inline_types->new( '"' ).
    ref_sa->append( 'SpecialCharacter' ).
    ref_sa ?= inline_types->new( '!' ).
    ref_sa->append( 'Image' ).
    ref_sa ?= inline_types->new( '&' ).
    ref_sa->append( 'SpecialCharacter' ).
    ref_sa ?= inline_types->new( '*' ).
    ref_sa->append( 'Emphasis' ).
    ref_sa ?= inline_types->new( ':' ).
    ref_sa->append( 'Url' ).
    ref_sa ?= inline_types->new( '<' ).
    ref_sa->append( 'UrlTag' ).
    ref_sa->append( 'EmailTag' ).
    ref_sa->append( 'Markup' ).
    ref_sa->append( 'SpecialCharacter' ).
    ref_sa ?= inline_types->new( '>' ).
    ref_sa->append( 'SpecialCharacter' ).
    ref_sa ?= inline_types->new( '[' ).
    ref_sa->append( 'Link' ).
    ref_sa ?= inline_types->new( '_' ).
    ref_sa->append( 'Emphasis' ).
    ref_sa ?= inline_types->new( '`' ).
    ref_sa->append( 'Code' ).
    ">>> apm
    ref_sa ?= inline_types->new( '~' ).
    ref_sa->append( 'Strikethrough' ).
    ref_sa->append( 'Emphasis' ). "subscript
    ref_sa ?= inline_types->new( '^' ).
    ref_sa->append( 'Emphasis' ). "superscript
    ref_sa ?= inline_types->new( '=' ).
    ref_sa->append( 'Highlight' ).
    "<<< apm
    ref_sa ?= inline_types->new( '\' ).
    ref_sa->append( 'EscapeSequence' ).
    "
    " Read-Only
    "
    special_characters = NEW #( ).

    ref_sa = special_characters.
    ref_sa->append( '\' ).
    ref_sa->append( '`' ).
    ref_sa->append( '*' ).
    ref_sa->append( '_' ).
    ref_sa->append( '{' ).
    ref_sa->append( '}' ).
    ref_sa->append( '[' ).
    ref_sa->append( ']' ).
    ref_sa->append( '(' ).
    ref_sa->append( ')' ).
    ref_sa->append( '>' ).
    ref_sa->append( '#' ).
    ref_sa->append( '+' ).
    ref_sa->append( '-' ).
    ref_sa->append( '.' ).
    ref_sa->append( '!' ).
    ref_sa->append( '|' ).

    strong_regex = NEW #( ).

    string ?= strong_regex->new( '*' ).
    string->set_data( '(^[*][*]((?:\\[*]|[^*]|[*][^*]*[*])+)[*][*](?![*]))' ).
    string ?= strong_regex->new( '_' ).
    string->set_data( '(^__((?:\\_|[^_]|_[^_]*_)+)__(?!_))' ).

    em_regex = NEW #( ).

    string ?= em_regex->new( '*' ).
    string->set_data( '(^[*]((?:\\[*]|[^*]|[*][*][^*]+[*][*])+)[*](?![*]))' ).
    string ?= em_regex->new( '_' ).
    string->set_data( '(^_((?:\\_|[^_]|__[^_]*__)+)_(?!_)\b)' ).
    string ?= em_regex->new( '^' ).
    string->set_data( '(^[\^]((?:\\[\^]|[^\^]|[\^][\^][^\^]+[\^][\^])+)[\^](?![\^]))' ).
    string ?= em_regex->new( '~' ).
    string->set_data( '(^~((?:\\~|[^~]|~~[^~]*~~)+)~(?!~)\b)' ).
    regex_html_attribute = '[a-zA-Z_:][\w:.-]*(?:\s*=\s*(?:[^"''=<>`\s]+|"[^"]*"|''[^'']*''))?'.

    void_elements = NEW #( ).

    ref_sa = void_elements.
    ref_sa->append( 'area' ).
    ref_sa->append( 'base' ).
    ref_sa->append( 'br' ).
    ref_sa->append( 'col' ).
    ref_sa->append( 'command' ).
    ref_sa->append( 'embed' ).
    ref_sa->append( 'hr' ).
    ref_sa->append( 'img' ).
    ref_sa->append( 'input' ).
    ref_sa->append( 'link' ).
    ref_sa->append( 'meta' ).
    ref_sa->append( 'param' ).
    ref_sa->append( 'source' ).

    text_level_elements = NEW #( ).

    ref_sa = text_level_elements.
    ref_sa->append( 'a' ).
    ref_sa->append( 'b' ).
    ref_sa->append( 'i' ).
    ref_sa->append( 'q' ).
    ref_sa->append( 's' ).
    ref_sa->append( 'u' ).
    ref_sa->append( 'br' ).
    ref_sa->append( 'em' ).
    ref_sa->append( 'rp' ).
    ref_sa->append( 'rt' ).
    ref_sa->append( 'tt' ).
    ref_sa->append( 'xm' ).
    ref_sa->append( 'bdo' ).
    ref_sa->append( 'big' ).
    ref_sa->append( 'del' ).
    ref_sa->append( 'ins' ).
    ref_sa->append( 'sub' ).
    ref_sa->append( 'sup' ).
    ref_sa->append( 'var' ).
    ref_sa->append( 'wbr' ).
    ref_sa->append( 'abbr' ).
    ref_sa->append( 'cite' ).
    ref_sa->append( 'code' ).
    ref_sa->append( 'font' ).
    ref_sa->append( 'mark' ).
    ref_sa->append( 'nobr' ).
    ref_sa->append( 'ruby' ).
    ref_sa->append( 'span' ).
    ref_sa->append( 'time' ).
    ref_sa->append( 'blink' ).
    ref_sa->append( 'small' ).
    ref_sa->append( 'nextid' ).
    ref_sa->append( 'spacer' ).
    ref_sa->append( 'strike' ).
    ref_sa->append( 'strong' ).
    ref_sa->append( 'acronym' ).
    ref_sa->append( 'listing' ).
    ref_sa->append( 'marquee' ).
    ref_sa->append( 'basefont' ).

    safe_links_whitelist = NEW #( ).

    ref_sa = safe_links_whitelist.
    ref_sa->append( 'http://' ).
    ref_sa->append( 'https://' ).
    ref_sa->append( 'sapevent:' ). " apm
    ref_sa->append( 'ftp://' ).
    ref_sa->append( 'ftps://' ).
    ref_sa->append( 'mailto:' ).
    ref_sa->append( 'data:image/png;base64,' ).
    ref_sa->append( 'data:image/gif;base64,' ).
    ref_sa->append( 'data:image/jpeg;base64,' ).
    ref_sa->append( 'irc:' ).
    ref_sa->append( 'ircs:' ).
    ref_sa->append( 'git:' ).
    ref_sa->append( 'ssh:' ).
    ref_sa->append( 'news:' ).
    ref_sa->append( 'steam:' ).

    "// Method names
    objdescr ?= cl_abap_objectdescr=>describe_by_object_ref( me ).
    LOOP AT objdescr->methods ASSIGNING <method>.
      APPEND <method>-name TO methods.
    ENDLOOP.
  ENDMETHOD.                    "constructor


  METHOD element.
    DATA:
      current_element TYPE ty_element,
      method_name     TYPE string,
      content         TYPE string.

    FIELD-SYMBOLS:
      <text>       LIKE current_element-text,
      <attribute>  LIKE LINE OF current_element-attributes,
      <content>    LIKE <text>-text,
      <attributes> LIKE <text>-attributes.

    magic_move(
      EXPORTING
        from = element
      CHANGING
        to   = current_element ).

    ">>> apm
    " Always sanitise
    " IF safe_mode IS NOT INITIAL
    current_element = sanitise_element( current_element ).
    " ENDIF
    "<<< apm

    ASSIGN COMPONENT 'TEXT' OF STRUCTURE current_element TO <text>.
    ASSERT sy-subrc = 0.

    result = |<{ current_element-name }|.

    IF current_element-attributes IS NOT INITIAL.
      LOOP AT current_element-attributes ASSIGNING <attribute>.
        result = |{ result } { <attribute>-name }="{ _escape( <attribute>-value ) }"|.
      ENDLOOP.
    ENDIF.

    IF <text> IS NOT INITIAL OR current_element-texts IS NOT INITIAL OR current_element-lines IS NOT INITIAL.
      result = |{ result }>|.

      ">>> apm
      IF current_element-handler = 'syntax_highlighter'.
        ASSIGN COMPONENT 'ATTRIBUTES' OF STRUCTURE <text> TO <attributes>.
        ASSERT sy-subrc = 0.
        <attributes> = current_element-attributes.
      ENDIF.
      "<<< apm

      IF current_element-handler IS NOT INITIAL.
        method_name = current_element-handler.
        TRANSLATE method_name TO UPPER CASE.

        IF current_element-texts IS NOT INITIAL. "// for array of elements
          CALL METHOD (method_name)
            EXPORTING
              elements = current_element-texts
            RECEIVING
              result   = content.
        ELSEIF current_element-lines IS NOT INITIAL. "// for array of lines
          CALL METHOD (method_name)
            EXPORTING
              lines  = current_element-lines
            RECEIVING
              result = content.
        ELSE. "// for simple text
          CALL METHOD (method_name)
            EXPORTING
              element = <text>
            RECEIVING
              result  = content.
        ENDIF.
      ELSE.
        IF current_element-lines IS NOT INITIAL.
          CONCATENATE LINES OF current_element-lines INTO content SEPARATED BY %_newline.
        ELSE.
          ASSIGN COMPONENT 'TEXT' OF STRUCTURE <text> TO <content>.
          ASSERT sy-subrc = 0.
          content = <content>.
          content = _escape(
            text         = content
            allow_quotes = abap_true ).
        ENDIF.

      ENDIF.
      result = |{ result }{ content }</{ current_element-name }>|.
    ELSE.
      result = |{ result } />|.
    ENDIF.
  ENDMETHOD.                    "element


  METHOD elements.
    DATA markup TYPE string_table.

    FIELD-SYMBOLS:
      <element> TYPE any,
      <markup>  TYPE string.

    LOOP AT elements ASSIGNING <element>.
      APPEND INITIAL LINE TO markup ASSIGNING <markup>.
      <markup> = element( <element> ).
    ENDLOOP.

    CONCATENATE LINES OF markup INTO result SEPARATED BY %_newline.
    CONCATENATE %_newline result %_newline INTO result.
  ENDMETHOD.                    "elements


  METHOD filter_unsafe_url_in_attribute.
    FIELD-SYMBOLS:
      <attribute> LIKE LINE OF result-attributes,
      <scheme>    TYPE string. "safe_links_whitelist->data.

    result = element.

    READ TABLE result-attributes WITH KEY name = attribute ASSIGNING <attribute>.
    CHECK sy-subrc = 0.

    ">>> apm
    CASE attribute.
      WHEN 'href'.
        <attribute>-value = _adjust_a_href( <attribute>-value ).
      WHEN 'src'.
        <attribute>-value = _adjust_img_src( <attribute>-value ).
    ENDCASE.
    "<<< apm

    " Check for allowed protocols
    LOOP AT safe_links_whitelist->get_data( ) ASSIGNING <scheme>.
      IF string_at_start(
        haystack = <attribute>-value
        needle   = <scheme> ) = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Effectively disable URL
    REPLACE FIRST OCCURRENCE OF ':' IN <attribute>-value WITH '%3A'.

  ENDMETHOD.


  METHOD htmlspecialchars.
    result = input.
    REPLACE ALL OCCURRENCES OF '&' IN result WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN result WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN result WITH '&gt;'.

    IF ent_noquotes IS INITIAL.
      REPLACE ALL OCCURRENCES OF '"' IN result WITH '&quot;'.
      IF ent_quotes IS NOT INITIAL.
        IF ent_html401 IS NOT INITIAL.
          REPLACE ALL OCCURRENCES OF '''' IN result WITH '&#039;'.
        ELSE.
          REPLACE ALL OCCURRENCES OF '''' IN result WITH '&apos;'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "htmlspecialchars


  METHOD inline_code.
    DATA:
      marker      TYPE c LENGTH 1,
      marker_comb TYPE string,
      m0          TYPE string,
      m1          TYPE string,
      text        TYPE string,
      not_found   TYPE abap_bool.

    marker = excerpt-text(1).

    "// Deal with the different repetitions (from 5 markers to 1)
    marker_comb = '&&&&&'.
    REPLACE ALL OCCURRENCES OF '&' IN marker_comb WITH marker.
    WHILE marker_comb IS NOT INITIAL.
      match_marked_string(
        EXPORTING
          marker    = marker_comb
          subject   = excerpt-text
        IMPORTING
          m0        = m0
          m1        = m1
          not_found = not_found ).
      IF not_found IS INITIAL.
        text = m1.
        CONDENSE text.
        text = text.
        REPLACE ALL OCCURRENCES OF REGEX '[ ]*\n' IN text WITH ' ' ##REGEX_POSIX.

        result-extent = strlen( m0 ).
        result-element-name = 'code'.
        result-element-text-text = text.
        EXIT.
      ENDIF.
      SHIFT marker_comb LEFT.
    ENDWHILE.
  ENDMETHOD.                    "inline_code


  METHOD inline_emailtag.
    DATA:
      hostname_label    TYPE string,
      common_mark_email TYPE string,
      regex             TYPE string,
      m0                TYPE string,
      m1                TYPE string,
      m2                TYPE string,
      url               TYPE string.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-attributes.

    CHECK excerpt-text CS '>'.

    hostname_label = '[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?'.
    common_mark_email = '[a-zA-Z0-9.!#$%&\''*+\/=?^_`{|}~-]+@'
      && hostname_label && '(?:\.' && hostname_label && ')*'.
    regex = '(^<((mailto:)?' && common_mark_email && ')>)'.

    FIND REGEX regex IN excerpt-text IGNORING CASE
      SUBMATCHES m0 m1 m2 ##REGEX_POSIX.
    IF sy-subrc = 0.
      url = m1.
      IF m2 IS INITIAL.
        CONCATENATE 'mailto:' url INTO url.
      ENDIF.

      result-extent = strlen( m0 ).
      result-element-name = 'a'.
      result-element-text-text = m1.

      APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
      <attribute>-name = 'href'.
      <attribute>-value = url.
    ENDIF.
  ENDMETHOD.                    "inline_EmailTag


  METHOD inline_emphasis.
    DATA:
      marker      TYPE c,
      emphasis    TYPE string,
      m0          TYPE string,
      m1          TYPE string,
      regex       TYPE REF TO lcl_string,
      regex_delim TYPE string,
      offset      TYPE i.

    CHECK excerpt-text IS NOT INITIAL.

    marker = excerpt-text(1).

    regex ?= strong_regex->get( marker ).
    IF strlen( excerpt-text ) > 1 AND excerpt-text+1(1) = marker AND
       regex->get_data( ) IS NOT INITIAL.
      FIND REGEX regex->get_data( ) IN excerpt-text SUBMATCHES m0 m1 ##REGEX_POSIX.
      IF sy-subrc = 0.
        emphasis = 'strong'.

        "// get the (ungreedy) end marker
        regex_delim = '[^&][&]{2}(?![&])'.
        REPLACE ALL OCCURRENCES OF '&' IN regex_delim WITH marker ##REGEX_POSIX.
        FIND REGEX regex_delim IN m1 MATCH OFFSET offset ##REGEX_POSIX.
        IF sy-subrc = 0.
          offset = offset + 1.
          m1 = m1(offset).
          offset = strlen( m1 ) + 4.
          m0 = m0(offset).
        ENDIF.
      ENDIF.
    ENDIF.

    regex ?= em_regex->get( marker ).
    IF emphasis IS INITIAL AND regex->get_data( ) IS NOT INITIAL.
      FIND REGEX regex->get_data( ) IN excerpt-text SUBMATCHES m0 m1 ##REGEX_POSIX.
      IF sy-subrc = 0.
        ">>> apm
        CASE marker.
          WHEN '~'.
            IF m0+1(1) = ` `.
              CLEAR emphasis.
            ELSE.
              emphasis = 'sub'.
            ENDIF.
          WHEN '^'.
            IF m0+1(1) = ` `.
              CLEAR emphasis.
            ELSE.
              emphasis = 'sup'.
            ENDIF.
          WHEN OTHERS.
            emphasis = 'em'.
        ENDCASE.
        "<<< apm
      ENDIF.
    ENDIF.

    CHECK emphasis IS NOT INITIAL.

    result-extent = strlen( m0 ).
    result-element-name = emphasis.
    result-element-handler = 'line'.
    result-element-text-text = m1.
  ENDMETHOD.                    "inline_Emphasis


  METHOD inline_escapesequence.
    DATA ch TYPE c.

    CHECK strlen( excerpt-text ) > 1.
    ch = excerpt-text+1(1).
    IF special_characters->find_val( ch ) > 0.
      result-markup = excerpt-text+1(1).
      result-extent = 2.
    ENDIF.
  ENDMETHOD.                    "inline_EscapeSequence


  METHOD inline_highlight.
    DATA:
      m0 TYPE string,
      m1 TYPE string.

    CHECK strlen( excerpt-text ) > 1 AND
          excerpt-text+1(1) = '='.

    FIND REGEX '(^==(?=\S)([^(?:==)]+)(?=\S)==)' IN excerpt-text
      SUBMATCHES m0 m1 ##REGEX_POSIX.
    IF sy-subrc = 0.
      result-extent = strlen( m0 ).
      result-element-name = 'mark'.
      result-element-text-text = m1.
      result-element-handler = 'line'.
    ENDIF.
  ENDMETHOD.


  METHOD inline_image.
    DATA link LIKE result.

    FIELD-SYMBOLS:
      <attribute>      LIKE LINE OF result-element-attributes,
      <attribute_from> LIKE LINE OF link-element-attributes.

    CHECK strlen( excerpt-text ) > 1 AND
          excerpt-text+1(1) = '['.
    excerpt-text = excerpt-text+1.

    link = inline_link( excerpt ).
    CHECK link IS NOT INITIAL.

    result-extent = link-extent + 1.
    result-element-name = 'img'.

    APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
    <attribute>-name = 'src'.
    READ TABLE link-element-attributes ASSIGNING <attribute_from>
      WITH KEY name = 'href'.
    IF sy-subrc = 0.
      <attribute>-value = _adjust_img_src( <attribute_from>-value ). " apm
      DELETE link-element-attributes WHERE name = 'href'.
    ENDIF.

    APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
    <attribute>-name = 'alt'.
    <attribute>-value = link-element-text-text.

    APPEND LINES OF link-element-attributes TO result-element-attributes.
  ENDMETHOD.                    "inline_Image


  METHOD inline_link.
    CONSTANTS c_regex_template TYPE string VALUE '\[((?:[^\]\[]|(?R))*)\]'.

    DATA:
      len        TYPE i,
      regex      TYPE string,
      remainder  TYPE string,
      m0         TYPE string,
      m1         TYPE string,
      m2         TYPE string,
      definition TYPE string,
      ref_map    TYPE REF TO lcl_hashmap,
      def_map    TYPE REF TO lcl_hashmap,
      def_val    TYPE REF TO lcl_string,
      exists     TYPE abap_bool.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-attributes.

    result-element-name = 'a'.
    result-element-handler = 'line'.

    remainder = excerpt-text.

    regex = |({ c_regex_template })|.
    DO 5 TIMES. "// regex recursion
      REPLACE '(?R)' IN regex WITH c_regex_template.
    ENDDO.
    REPLACE '(?R)' IN regex WITH '$'.

    FIND REGEX regex IN remainder SUBMATCHES m0 m1 ##REGEX_POSIX.
    IF sy-subrc = 0.
      result-element-text-text = m1.
      result-extent = strlen( m0 ).
      remainder = remainder+result-extent.
    ELSE.
      CLEAR result.
      RETURN.
    ENDIF.

*^[(]\s*((?:[^ ()]+|[(][^ )]+[)])+)(?:[ ]+("[^"]*"|''[^'']*''))?\s*[)]
*^[(]\s*((?:[^ ()]|[(][^ )]+[)])+)(?:[ ]+("[^"]*"|''[^'']*''))?\s*[)]
*^[(]((?:[^ ()]|[(][^ )]+[)])+)(?:[ ]+("[^"]*"|''[^'']*''))?[)]

    "FIND REGEX '(^[(]\s*((?:[^ ()]|[(][^ )]+[)])+)(?:[ ]+("[^"]*"|''[^\'']*''))?\s*[)])'
    FIND REGEX '(^[(]\s*((?:[^ ()]|[(][^ )]+[)])+)(?:[ ]+("[^"]*"|''[^\'']*''|\([^\)]*\)))?\s*[)])'
      IN remainder SUBMATCHES m0 m1 m2 ##REGEX_POSIX. " apm
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
      <attribute>-name = 'href'.
      <attribute>-value = m1.
      IF m2 IS NOT INITIAL.
        APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
        <attribute>-name = 'title'.
        len = strlen( m2 ) - 2.
        <attribute>-value = m2+1(len).
      ENDIF.

      len = strlen( m0 ).
      result-extent = result-extent + len.

    ELSE.
      FIND REGEX '(^\s*\[([^\]]*)\])' IN remainder SUBMATCHES m0 m1 ##REGEX_POSIX.
      IF sy-subrc = 0.
        IF m1 IS NOT INITIAL.
          definition = m1.
        ELSE.
          definition = result-element-text-text.
        ENDIF.
        len = strlen( m0 ).
        result-extent = result-extent + len.
      ELSE.
        definition = result-element-text-text.
      ENDIF.

      TRANSLATE definition TO LOWER CASE.
      ref_map ?= definition_data->get( 'Reference' ).
      exists = ref_map->exists( definition ).
      IF exists IS INITIAL.
        CLEAR result.
        RETURN.
      ENDIF.

      def_map ?= ref_map->get( definition ).

      def_val ?= def_map->get( 'url' ).
      APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
      <attribute>-name = 'href'.
      <attribute>-value = _adjust_a_href( def_val->get_data( ) ). " apm

      exists = def_map->exists( 'title' ).
      IF exists IS NOT INITIAL.
        def_val ?= def_map->get( 'title' ).
        APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
        <attribute>-name = 'title'.
        <attribute>-value = def_val->get_data( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "inline_Link


  METHOD inline_markup.
    DATA:
      m0    TYPE string,
      regex TYPE string.

    CHECK markup_escaped IS INITIAL AND
          safe_mode IS INITIAL AND
          excerpt-text CS '>' AND
          strlen( excerpt-text ) > 1.

    FIND REGEX '(^<\/\w*[ ]*>)' IN excerpt-text SUBMATCHES m0 ##REGEX_POSIX.
    IF sy-subrc <> 0.
      FIND REGEX '(^<!---?[^>-](?:-?[^-])*-->)' IN excerpt-text SUBMATCHES m0 ##REGEX_POSIX.
      IF sy-subrc <> 0.
        regex = '(^<\w*(?:[ ]*' && regex_html_attribute && ')*[ ]*\/?>)'.
        FIND REGEX regex IN excerpt-text SUBMATCHES m0 ##SUBRC_OK ##REGEX_POSIX.
      ENDIF.
    ENDIF.

    IF m0 IS NOT INITIAL.
      result-extent = strlen( m0 ).
      result-markup = _adjust_markup( m0 ). " apm
    ENDIF.
  ENDMETHOD.                    "inline_Markup


  METHOD inline_specialcharacter.
    DATA special TYPE string.

    CHECK excerpt-text IS NOT INITIAL.

    IF excerpt-text(1) = '&'.
      FIND REGEX '^&#?\w+;' IN excerpt-text ##REGEX_POSIX.
      IF sy-subrc <> 0.
        result-markup = '&amp;'.
        result-extent = 1.
        RETURN.
      ENDIF.
    ENDIF.

    CASE excerpt-text(1).
      WHEN '>'.
        special = 'gt'.
      WHEN '<'.
        special = 'lt'.
      WHEN '"'.
        special = 'quot'.
    ENDCASE.
    IF special IS NOT INITIAL.
      CONCATENATE '&' special ';' INTO result-markup.
      result-extent = 1.
    ENDIF.
  ENDMETHOD.                    "inline_SpecialCharacter


  METHOD inline_strikethrough.
    DATA:
      m0 TYPE string,
      m1 TYPE string.

    CHECK strlen( excerpt-text ) > 1 AND
          excerpt-text+1(1) = '~'.

    FIND REGEX '(^~~(?=\S)([^(?:~~)]+)(?=\S)~~)' IN excerpt-text
      SUBMATCHES m0 m1 ##REGEX_POSIX.
    IF sy-subrc = 0.
      result-extent = strlen( m0 ).
      result-element-name = 'del'.
      result-element-text-text = m1.
      result-element-handler = 'line'.
    ENDIF.
  ENDMETHOD.                    "inline_Strikethrough


  METHOD inline_url.
    DATA:
      m0     TYPE string,
      offset TYPE i.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-attributes.

    CHECK urls_linked IS NOT INITIAL AND
          strlen( excerpt-text ) > 2 AND
          excerpt-text+2(1) = '/'.

    FIND REGEX '(\bhttps?:[\/]{2}[^\s<]+\b\/*)' IN excerpt-context
      IGNORING CASE SUBMATCHES m0 MATCH OFFSET offset ##REGEX_POSIX.
    IF sy-subrc = 0.
      result-extent = strlen( m0 ).
      result-position = offset + 1. "// set to +1 so 0 is not initial
      result-element-name = 'a'.
      result-element-text-text = m0.

      APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
      <attribute>-name = 'href'.
      <attribute>-value = m0.
    ENDIF.
  ENDMETHOD.                    "inline_Url


  METHOD inline_urltag.
    DATA:
      m0  TYPE string,
      m1  TYPE string,
      url TYPE string.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-element-attributes.

    CHECK excerpt-text CS '>'.

    FIND REGEX '(^<(\w+:\/{2}[^ >]+)>)' IN excerpt-text SUBMATCHES m0 m1 ##REGEX_POSIX.
    IF sy-subrc = 0.
      url = m1.
      result-extent = strlen( m0 ).
      result-element-name = 'a'.
      result-element-text-text = url.

      APPEND INITIAL LINE TO result-element-attributes ASSIGNING <attribute>.
      <attribute>-name = 'href'.
      <attribute>-value = url.
    ENDIF.
  ENDMETHOD.                    "inline_UrlTag


  METHOD li.
    DATA:
      trimmed_markup TYPE string,
      fdpos          TYPE i,
      pos_to         TYPE i.

    result = _lines( lines ).
    trimmed_markup = trim( result ).

    IF NOT line_exists( lines[ table_line = '' ] ) AND strlen( trimmed_markup ) >= 3 AND trimmed_markup(3) = '<p>'.
      result = trimmed_markup+3.
      FIND '</p>' IN result MATCH OFFSET fdpos ##SUBRC_OK.
      pos_to = fdpos + 4.
      CONCATENATE result(fdpos) result+pos_to INTO result.
    ENDIF.

    ">>> apm
    " Task lists
    IF result CP '[ ]*'.
      result = |<input type="checkbox" disabled="disabled">{ result+3 }|.
    ELSEIF result CP '[X]*'.
      result = |<input type="checkbox" disabled="disabled" checked="checked">{ result+3 }|.
    ENDIF.
    "<<< apm
  ENDMETHOD.                    "li


  METHOD line.
    DATA:
      text            TYPE string,
      unmarked_text   TYPE string,
      marker_position TYPE i,
      pos             TYPE i,
      excerpt         TYPE ty_excerpt,
      inline          TYPE ty_inline,
      marker          TYPE c,
      inline_types_sa TYPE REF TO lcl_string_array,
      method_name     TYPE string,
      markup_part     TYPE string,
      continue_loop   TYPE abap_bool.

    FIELD-SYMBOLS <inline_type> TYPE string. "inline_types_sa->data.

    " text contains the unexamined text
    " excerpt-text is based on the first occurrence of a marker
    text = element-text.

    WHILE text IS NOT INITIAL.
      IF text NA inline_marker_list.
        EXIT.
      ENDIF.
      excerpt-text = text+sy-fdpos.
      marker = excerpt-text(1).

      FIND marker IN text MATCH OFFSET marker_position ##SUBRC_OK.

      excerpt-context = text.

      inline_types_sa ?= inline_types->get( marker ).
      CLEAR continue_loop.
      LOOP AT inline_types_sa->get_data( ) ASSIGNING <inline_type>.
        CONCATENATE 'inline_' <inline_type> INTO method_name.
        TRANSLATE method_name TO UPPER CASE.
        CALL METHOD (method_name)
          EXPORTING
            excerpt = excerpt
          RECEIVING
            result  = inline.

        " makes sure that the inline belongs to "our" marker
        CHECK inline IS NOT INITIAL.
        CHECK inline-position <= marker_position.

        " sets a default inline position
        IF inline-position IS INITIAL.
          inline-position = marker_position.
        ELSE.
          inline-position = inline-position - 1.
        ENDIF.

        " the text that comes before the inline
        IF strlen( text ) >= inline-position.
          unmarked_text = text(inline-position).
        ELSE.
          unmarked_text = text.
        ENDIF.

        " compile the unmarked text
        markup_part = unmarked_text( unmarked_text ).
        CONCATENATE result markup_part INTO result.

        " compile the inline
        IF inline-markup IS NOT INITIAL.
          CONCATENATE result inline-markup INTO result.
        ELSE.
          markup_part = element( inline-element ).
          CONCATENATE result markup_part INTO result.
        ENDIF.

        " remove the examined text
        pos = inline-position + inline-extent.
        IF strlen( text ) >= pos.
          text = text+pos.
        ELSE.
          CLEAR text.
        ENDIF.

        continue_loop = abap_true.
        EXIT.
      ENDLOOP. "inline_types->data
      CHECK continue_loop IS INITIAL.

      " the marker does not belong to an inline
      marker_position = marker_position + 1.
      IF strlen( text ) >= marker_position.
        unmarked_text = text(marker_position).
      ELSE.
        unmarked_text = text.
      ENDIF.
      markup_part = unmarked_text( unmarked_text ).
      CONCATENATE result markup_part INTO result.
      IF strlen( text ) >= marker_position.
        text = text+marker_position.
      ELSE.
        CLEAR text.
      ENDIF.
    ENDWHILE.

    markup_part = unmarked_text( text ).
    CONCATENATE result markup_part INTO result.
  ENDMETHOD.                    "line


  METHOD magic_move.
    "!
    " Magic move-corresponding
    " Recursively handles any kind of structures
    "!
    DATA:
      td_from TYPE REF TO cl_abap_typedescr,
      td_to   TYPE REF TO cl_abap_typedescr,
      sd_from TYPE REF TO cl_abap_structdescr,
      sd_to   TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <tab_from>  TYPE table,
      <tab_to>    TYPE table,
      <any_from>  TYPE any,
      <any_to>    TYPE any,
      <comp_from> LIKE LINE OF sd_from->components,
      <comp_to>   LIKE LINE OF sd_to->components.

    td_from = cl_abap_typedescr=>describe_by_data( from ).
    td_to   = cl_abap_typedescr=>describe_by_data( to ).
    IF td_from->absolute_name = td_to->absolute_name.
      to = from.
      RETURN.
    ENDIF.

    "// Scenario 1 => simple to simple
    IF td_from->kind = td_to->kind AND
       td_from->kind = cl_abap_typedescr=>kind_elem.
      to = from.

      "// Scenario 2 => struct to struct
    ELSEIF td_from->kind = td_to->kind AND
           td_from->kind = cl_abap_typedescr=>kind_struct.
      sd_from ?= td_from.
      sd_to ?= td_to.
      LOOP AT sd_from->components ASSIGNING <comp_from>.
        READ TABLE sd_to->components ASSIGNING <comp_to>
          WITH KEY name = <comp_from>-name.
        CHECK sy-subrc = 0.
        IF <comp_to>-type_kind = cl_abap_typedescr=>typekind_table.
          ASSIGN COMPONENT <comp_to>-name OF STRUCTURE from TO <tab_from>.
          ASSERT sy-subrc = 0.
          ASSIGN COMPONENT <comp_to>-name OF STRUCTURE to TO <tab_to>.
          ASSERT sy-subrc = 0.
          LOOP AT <tab_from> ASSIGNING <any_from>.
            APPEND INITIAL LINE TO <tab_to> ASSIGNING <any_to>.

            magic_move(
              EXPORTING
                from = <any_from>
                name = <comp_to>-name
              CHANGING
                to   = <any_to> ).
          ENDLOOP.
        ELSE.
          ASSIGN COMPONENT <comp_to>-name OF STRUCTURE from TO <any_from>.
          ASSERT sy-subrc = 0.
          ASSIGN COMPONENT <comp_to>-name OF STRUCTURE to TO <any_to>.
          ASSERT sy-subrc = 0.

          magic_move(
            EXPORTING
              from = <any_from>
              name = <comp_to>-name
            CHANGING
              to   = <any_to> ).
        ENDIF.
      ENDLOOP.

      "// Scenario 3 => simple to struct
    ELSEIF td_from->kind = cl_abap_typedescr=>kind_elem AND
           td_to->kind = cl_abap_typedescr=>kind_struct AND
           name IS NOT INITIAL.
      sd_to ?= td_to.
      READ TABLE sd_to->components ASSIGNING <comp_to>
        WITH KEY name = name.
      IF sy-subrc = 0 AND
         <comp_to>-type_kind <> cl_abap_typedescr=>typekind_table.
        ASSIGN COMPONENT <comp_to>-name OF STRUCTURE to TO <any_to>.
        ASSERT sy-subrc = 0.

        magic_move(
          EXPORTING
            from = from
            name = <comp_to>-name
          CHANGING
            to   = <any_to> ).
      ENDIF.

      "// Scenario 4 => struct to simple
    ELSEIF td_from->kind = cl_abap_typedescr=>kind_struct AND
           td_to->kind = cl_abap_typedescr=>kind_elem AND
           name IS NOT INITIAL.
      sd_from ?= td_from.
      READ TABLE sd_from->components ASSIGNING <comp_from>
        WITH KEY name = name.
      IF sy-subrc = 0 AND
         <comp_from>-type_kind <> cl_abap_typedescr=>typekind_table.
        ASSIGN COMPONENT <comp_from>-name OF STRUCTURE to TO <any_from>.
        ASSERT sy-subrc = 0.

        magic_move(
          EXPORTING
            from = <any_from>
            name = <comp_from>-name
          CHANGING
            to   = to ).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "magic_move


  METHOD match_marked_string.
    "!
    " Workaround for an ungreedy regex match
    " Specific for regex matches with a delimiting marker
    "!
    CONSTANTS:
      c_regex       TYPE string VALUE '(^{&X}[ ]*(.+)[ ]*{&X}(?!{&1}))',
      c_regex_delim TYPE string VALUE '[^{&1}]{&X}(?!{&1})'.

    DATA:
      marker_ptn    TYPE string,
      submarker_ptn TYPE string,
      regex         TYPE string,
      regex_delim   TYPE string,
      offset        TYPE i.

    marker_ptn = marker.
    REPLACE ALL OCCURRENCES OF REGEX '([*?!+])' IN marker_ptn WITH '[$1]' ##REGEX_POSIX.
    submarker_ptn = marker(1).
    REPLACE ALL OCCURRENCES OF REGEX '([*?!+])' IN submarker_ptn WITH '[$1]' ##REGEX_POSIX.

    regex = c_regex.
    REPLACE ALL OCCURRENCES OF '{&1}' IN regex WITH submarker_ptn ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF '{&X}' IN regex WITH marker_ptn ##REGEX_POSIX.

    regex_delim = c_regex_delim.
    REPLACE ALL OCCURRENCES OF '{&1}' IN regex_delim WITH submarker_ptn ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF '{&X}' IN regex_delim WITH marker_ptn ##REGEX_POSIX.

    FIND REGEX regex IN subject SUBMATCHES m0 m1 ##REGEX_POSIX.
    IF sy-subrc = 0.
      FIND REGEX regex_delim IN m1 MATCH OFFSET offset ##REGEX_POSIX.
      IF sy-subrc = 0.
        offset = offset + 1.
        m1 = m1(offset).
        offset = strlen( m1 ) + ( strlen( marker ) * 2 ).
        m0 = m0(offset).
      ENDIF.
      not_found = abap_false.
    ELSE.
      not_found = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD paragraph.
    result-element-name = 'p'.
    result-element-text-text = line-text.
    result-element-handler = 'line'.
  ENDMETHOD.                    "paragraph


  METHOD sanitise_element.
    CONSTANTS c_good_attribute TYPE string VALUE '^[a-zA-Z0-9][a-zA-Z0-9_-]*$'.

    FIELD-SYMBOLS <attribute> LIKE LINE OF result-attributes.

    result = element.

    CASE result-name.
      WHEN 'a'.
        result = filter_unsafe_url_in_attribute(
          element   = result
          attribute = 'href' ).
      WHEN 'img'.
        result = filter_unsafe_url_in_attribute(
          element   = result
          attribute = 'src' ).
    ENDCASE.

    LOOP AT result-attributes ASSIGNING <attribute>.
      " filter out badly parsed attribute
      FIND REGEX c_good_attribute IN <attribute>-name ##REGEX_POSIX.
      IF sy-subrc <> 0.
        DELETE TABLE result-attributes FROM <attribute>.
        CONTINUE.
      ENDIF.
      " dump onevent attribute
      IF string_at_start(
        haystack = <attribute>-name
        needle   = 'on' ) = abap_true.
        DELETE TABLE result-attributes FROM <attribute>.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_breaks_enabled.
    breaks_enabled = breaks_enabled.
    result = me.
  ENDMETHOD.                    "set_breaks_enabled


  METHOD set_markup_escaped.
    markup_escaped = markup_escaped.
    result = me.
  ENDMETHOD.                    "set_markup_escaped


  METHOD set_safe_mode.
    safe_mode = iv_safe_mode.
    result = me.
  ENDMETHOD.


  METHOD set_urls_linked.
    urls_linked = urls_linked.
    result = me.
  ENDMETHOD.                    "set_urls_linked


  METHOD string_at_start.
    DATA len TYPE i.

    len = strlen( needle ).

    IF strlen( haystack ) < len.
      result = abap_false.
    ELSE.
      result = xsdbool( to_lower( haystack+0(len) ) = needle ).
    ENDIF.
  ENDMETHOD.


  METHOD styles.

    result = |.markdown\n|
      && |\{ background-color: #f2f2f2; padding: 15px; \}\n|
      && |.markdown .logo\n|
      && |\{ width: 36px; height: 22px; margin-top: -4px; \}\n|
      && |.markdown .header,\n|
      && |.markdown .content\n|
      && |\{ background-color: #ffffff; border: 1px solid #d8dee4; display: block; \}\n|
      && |.markdown .header\n|
      && |\{ font-size: larger; margin-bottom: 15px; padding: 15px; \}\n|
      && |.markdown .content\n|
      && |\{ padding: 25px; \}\n|
      && |.markdown .html\n|
      && |\{ max-width: 1024px; margin: 0 auto; padding: 25px; \}\n|
      " Markdown View
      && |.markdown .source\n|
      && |\{ font-family: Consolas,Courier,monospace; font-size: 12pt; padding: 25px;\n|
      && |  max-width: 1024px; margin: 0 auto; \}\n|
      && |.markdown .source table\n|
      && |\{ border: 1px solid #d8dee4; \}\n|
      && |.markdown .source td\n|
      && |\{ border-top: 0px; border-bottom: 0px; padding-top: 0; padding-bottom: 0;\n|
      && |  line-height: 20px; vertical-align: top; \}\n|
      " Syntax Highlight
      && |.markdown .syntax-hl .heading\n|
      && |\{ color: blue; \}\n|
      && |.markdown .syntax-hl .link\n|
      && |\{ color: purple; \}\n|
      && |.markdown .syntax-hl .url\n|
      && |\{ color: green; \}\n|
      && |.markdown .syntax-hl .html\n|
      && |\{ padding: 0; \}\n|
      && |.markdown .syntax-hl .bold\n|
      && |\{ font-weight: bold; \}\n|
      " HTML Tags
      && |.markdown h1,\n|
      && |.markdown h2\n|
      && |\{ border-bottom: 1px solid #d8dee4; box-sizing: border-box; \}\n|
      && |.markdown img\n|
      && |\{ border: 0; box-sizing: border-box; max-width: 100%; vertical-align: middle; \}\n|
      && |.markdown p,\n|
      && |.markdown li\n|
      && |\{ line-height: 1.5; \}\n|
      && |.markdown table\n|
      && |\{ border: 1px solid #ddd; border-radius: 3px; \}\n|
      && |.markdown th,\n|
      && |\{ color: #4078c0; background-color: #edf2f9; border-bottom-color: #ddd; \}\n|
      && |.markdown th,\n|
      && |.markdown td\n|
      && |\{ border: 1px solid #ddd; padding: 6px 13px; \}\n|
      && |.markdown tr:first-child td\n|
      && |\{ border-top: 0; \}\n|
      && |.markdown hr\n|
      && |\{ background-color: #eee; margin: 24px 0; overflow: hidden; padding: 0; \}\n|
      && |.markdown mark\n|
      && |\{ background-color: #fff8e0; border-radius: 6px; margin: 0; padding: .2em .4em; \}\n|
      && |.markdown blockquote\n|
      && |\{ background-color: #eee; border-left: 3px solid #303d36; border-radius: 6px;\n|
      && |  margin: 0 0 16px; padding: 1px 1em; \}\n|
      " GitHub Alerts
      && |.markdown blockquote.alert-note\n|
      && |\{ border-left: 3px solid #4493f8; \}\n|
      && |.markdown blockquote.alert-tip\n|
      && |\{ border-left: 3px solid #3fb950; \}\n|
      && |.markdown blockquote.alert-important\n|
      && |\{ border-left: 3px solid #ab7df8; \}\n|
      && |.markdown blockquote.alert-warning\n|
      && |\{ border-left: 3px solid #d29922; \}\n|
      && |.markdown blockquote.alert-caution\n|
      && |\{ border-left: 3px solid #f85149; \}\n|
      " Code blocks
      && |.markdown pre\n|
      && |\{ background-color: #eee; border-radius: 6px; display: block;\n|
      && |  margin-bottom: 16px; margin-top: 0; overflow: auto; overflow-wrap: normal;\n|
      && |  padding: 16px; word-break: normal; box-sizing: border-box;\n|
      && |  font-family: Consolas, Courier, monospace; font-size: 14px; \}\n|
      && |.markdown p code\n|
      && |\{ background-color: #eee; border-radius: 6px; margin: 0; padding: .2em .4em;\n|
      && |  font-family: Consolas, Courier, monospace; font-size: 14px; \}\n|
      && |.markdown pre code\n|
      && |\{ background-color: transparent; border-style: initial;\n|
      && |  border-width: 0; box-sizing: border-box; display: inline; margin: 0;\n|
      && |  overflow: visible; word-break: normal; overflow-wrap: normal;\n|
      && |  padding: 0; white-space: pre;\n|
      && |  font-family: Consolas, Courier, monospace; font-size: 14px; \}\n|
      && |kbd \{\n|
      && |  border: 1px solid rgba(61, 68, 77, .7);\n|
      && |  border-radius: 6px;\n|
      && |  box-shadow: inset 0 -1px 0 var(--borderColor-neutral-muted, var(--color-neutral-muted));\n|
      && |  display: inline-block;\n|
      && |  font-family: Consolas, Courier, monospace;\n|
      && |  font-kerning: auto;\n|
      && |  font-optical-sizing: auto;\n|
      && |  font-size: 11px;\n|
      && |  font-size-adjust: none;\n|
      && |  font-variant: normal;\n|
      && |  font-variant-emoji: normal;\n|
      && |  font-weight: 400;\n|
      && |  line-height: 10px;\n|
      && |  padding: 4px;\n|
      && |  vertical-align: middle;\}\n|.

  ENDMETHOD.


  METHOD syntax_highlighter.
    ">>> apm
    DATA:
      language        TYPE string,
      current_element TYPE ty_element.

    FIELD-SYMBOLS <attribute> LIKE LINE OF current_element-attributes.

    magic_move(
      EXPORTING
        from = element
      CHANGING
        to   = current_element ).

    READ TABLE current_element-attributes ASSIGNING <attribute> WITH TABLE KEY name = 'class'.
    IF sy-subrc = 0 AND <attribute>-value CP 'language-*'.
      language = <attribute>-value+9(*).

      result = /apmg/cl_apm_markdown_syn=>process(
        source   = current_element-text-text
        language = language ).
    ELSE.
      IF current_element-lines IS NOT INITIAL.
        CONCATENATE LINES OF current_element-lines INTO result SEPARATED BY %_newline.
      ELSE.
        result = current_element-text-text.
        result = _escape(
          text         = result
          allow_quotes = abap_true ).
      ENDIF.
    ENDIF.
    "<<< apm
  ENDMETHOD.


  METHOD text.
    " Parses the markdown text and returns the markup
    DATA:
      lines      TYPE string_table,
      alert      TYPE lcl_alerts=>ty_alert,
      alert_html TYPE string.

    " make sure no definitions are set
    definition_data = NEW #( value_type = 'lcl_hashmap:lcl_hashmap' ).

    " standardize line breaks
    REPLACE ALL OCCURRENCES OF REGEX '\r?\n' IN text WITH %_newline ##REGEX_POSIX.

    " remove surrounding line breaks
    text = trim(
      str  = text
      mask = '\n' ).

    " split text into lines
    SPLIT text AT %_newline INTO TABLE lines.

    " iterate through lines to identify blocks
    markup = _lines( lines ).

    " trim line breaks
    markup = trim(
      str  = markup
      mask = '\n' ).

    " >>> apm
    DO 5 TIMES.
      CASE sy-index.
        WHEN 1.
          alert = lcl_alerts=>get( '[!NOTE]' ).
        WHEN 2.
          alert = lcl_alerts=>get( '[!TIP]' ).
        WHEN 3.
          alert = lcl_alerts=>get( '[!IMPORTANT]' ).
        WHEN 4.
          alert = lcl_alerts=>get( '[!WARNING]' ).
        WHEN 5.
          alert = lcl_alerts=>get( '[!CAUTION]' ).
      ENDCASE.

      alert_html = |<span style="color:{ alert-color };display:flex;align-items:center;">|
        && |{ alert-icon }&nbsp;&nbsp;|
        && |<strong>{ alert-text }</strong></span></p><p>|.

      markup = replace(
        val  = markup
        sub  = alert-tag
        with = alert_html
        occ  = 0 ).
    ENDDO.
    " <<< apm

  ENDMETHOD.                    "text


  METHOD trim.
    DATA regex TYPE string.

    result = str.
    REPLACE ALL OCCURRENCES OF REGEX '([\.\?\*\+\|])' IN mask WITH '\\$1' ##REGEX_POSIX.
    CONCATENATE '(\A[' mask ']*)|([' mask ']*\Z)' INTO regex.
    REPLACE ALL OCCURRENCES OF REGEX regex IN result WITH '' ##REGEX_POSIX.
  ENDMETHOD.                    "trim


  METHOD unmarked_text.
    DATA break TYPE string.

    CONCATENATE '<br />' %_newline INTO break.
    result = text.

    IF breaks_enabled IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF REGEX '[ ]*\n' IN result WITH break ##REGEX_POSIX.
    ELSE.
      REPLACE ALL OCCURRENCES OF REGEX '(?:[ ][ ]+|[ ]*\\)\n' IN result WITH break ##REGEX_POSIX.
      REPLACE ALL OCCURRENCES OF REGEX ' \n' IN result WITH %_newline ##REGEX_POSIX.
    ENDIF.
  ENDMETHOD.                    "unmarked_text


  METHOD _adjust_a_href.
    ">>> apm
    result = _adjust_link(
      root   = config-root_href
      source = source ).

    " Open external links in new browser window
    IF config-sapevent = abap_true AND result CP 'http*'.
      result = 'sapevent:url?url=' && result.
    ENDIF.
    "<<< apm
  ENDMETHOD.


  METHOD _adjust_img_src.
    ">>> apm
    result = _adjust_link(
      root   = config-root_img
      source = source ).
    "<<< apm
  ENDMETHOD.


  METHOD _adjust_link.
    ">>> apm
    result = source.

    CHECK root IS NOT INITIAL
      AND source IS NOT INITIAL
      AND source NP 'http*'
      AND source(1) <> '#'.

    IF result CP '/*'.
      result = source.
    ELSEIF result CP './*'.
      result = config-path && source+2.
    ELSE.
      result = config-path && source.
    ENDIF.

    result = root && config-path_util->normalize( result ).
    "<<< apm
  ENDMETHOD.


  METHOD _adjust_markup.
    ">>> apm
    DATA:
      matches TYPE match_result_tab,
      href    TYPE string,
      url     TYPE string.

    FIELD-SYMBOLS:
      <match>    LIKE LINE OF matches,
      <submatch> LIKE LINE OF <match>-submatches.

    result = source.

    FIND ALL OCCURRENCES OF REGEX 'href\s*=\s*"([^"]*)"' IN result RESULTS matches ##SUBRC_OK ##REGEX_POSIX.

    SORT matches DESCENDING BY line DESCENDING offset.

    LOOP AT matches ASSIGNING <match>.
      READ TABLE <match>-submatches ASSIGNING <submatch> INDEX 1.
      ASSERT sy-subrc = 0.

      href = result+<submatch>-offset(<submatch>-length).
      url = _adjust_a_href( href ).
      REPLACE href IN result WITH url.
    ENDLOOP.

    CLEAR matches.

    FIND ALL OCCURRENCES OF REGEX 'src\s*=\s*"([^"]*)"' IN result RESULTS matches ##SUBRC_OK ##REGEX_POSIX.

    SORT matches DESCENDING BY line DESCENDING offset.

    LOOP AT matches ASSIGNING <match>.
      READ TABLE <match>-submatches ASSIGNING <submatch> INDEX 1.
      ASSERT sy-subrc = 0.

      href = result+<submatch>-offset(<submatch>-length).
      url = _adjust_img_src( href ).
      REPLACE href IN result WITH url.
    ENDLOOP.
    "<<< apm
  ENDMETHOD.


  METHOD _escape.
    result = htmlspecialchars(
      input        = text
      ent_html401  = abap_true
      ent_noquotes = allow_quotes
      ent_quotes   = boolc( allow_quotes IS INITIAL ) ).
  ENDMETHOD.


  METHOD _lines.
    DATA:
      current_block         TYPE ty_block,
      line                  TYPE string,
      chopped_line          TYPE string,
      parts                 TYPE string_table,
      shortage              TYPE i,
      spaces                TYPE string,
      indent                TYPE i,
      text                  TYPE string,
      continue_to_next_line TYPE abap_bool,
      current_line          TYPE ty_line,
      method_name           TYPE string,
      block                 TYPE ty_block,
      marker                TYPE string,
      ref_block_types       TYPE REF TO lcl_string_array,
      ref_sa                TYPE REF TO lcl_string_array,
      blocks                TYPE STANDARD TABLE OF ty_block WITH EMPTY KEY,
      block_markup          TYPE string.

    FIELD-SYMBOLS:
      <block>           LIKE LINE OF blocks,
      <block_type_name> TYPE string,
      <block_type>      TYPE lcl_hashmap=>ty_item,
      <part>            LIKE LINE OF parts.

    LOOP AT lines INTO line.

      chopped_line = line.
      REPLACE REGEX '\s+$' IN chopped_line WITH '' ##REGEX_POSIX.
      IF strlen( chopped_line ) = 0.
        current_block-interrupted = abap_true.
        CONTINUE.
      ENDIF.

      IF line CS %_horizontal_tab.
        SPLIT line AT %_horizontal_tab INTO TABLE parts.
        LOOP AT parts ASSIGNING <part>.
          AT FIRST.
            line = <part>.
            CONTINUE.
          ENDAT.
          shortage = 4 - ( strlen( line ) MOD 4 ).
          CLEAR spaces.
          DO shortage TIMES.
            CONCATENATE spaces space INTO spaces RESPECTING BLANKS.
          ENDDO.
          CONCATENATE line spaces <part> INTO line RESPECTING BLANKS.
        ENDLOOP. "lt_parts
      ENDIF.

      CLEAR spaces.
      FIND REGEX '^(\s+)' IN line SUBMATCHES spaces ##SUBRC_OK ##REGEX_POSIX.
      indent = strlen( spaces ).
      IF indent > 0.
        text = line+indent.
      ELSE.
        text = line.
      ENDIF.

      " ~

      CLEAR current_line.
      current_line-body = line.
      current_line-indent = indent.
      current_line-text = text.

      " ~

      IF current_block-continuable IS NOT INITIAL.
        CLEAR block.
        CONCATENATE 'block_' current_block-type '_continue' INTO method_name.
        TRANSLATE method_name TO UPPER CASE.

        CALL METHOD (method_name)
          EXPORTING
            line   = current_line
            block  = current_block
          RECEIVING
            result = block.

        IF block IS NOT INITIAL.
          current_block = block.
          CONTINUE.
        ELSE.
          CONCATENATE 'block_' current_block-type '_complete' INTO method_name.
          TRANSLATE method_name TO UPPER CASE.

          IF line_exists( methods[ table_line = method_name ] ).
            CALL METHOD (method_name)
              EXPORTING
                block  = current_block
              RECEIVING
                result = current_block.
          ENDIF.
        ENDIF. "ls_block is not initial.
        CLEAR current_block-continuable.
      ENDIF. "ls_current_block-continuable is not initial.

      " ~

      marker = text(1).

      " ~

      ref_block_types = NEW #( ).
      ref_block_types->lif_value_type~copy( unmarked_block_types ).

      DATA(block_types_data) = block_types->get_data( ).
      READ TABLE block_types_data ASSIGNING <block_type> WITH KEY key = marker.
      IF sy-subrc = 0.
        ref_sa ?= <block_type>-value.
        ref_block_types->append_array( ref_sa ).
      ENDIF.

      " ~

      LOOP AT ref_block_types->get_data( ) ASSIGNING <block_type_name>.
        CLEAR block.
        CONCATENATE 'block_' <block_type_name> INTO method_name.
        TRANSLATE method_name TO UPPER CASE.

        CALL METHOD (method_name)
          EXPORTING
            line   = current_line
            block  = current_block
          RECEIVING
            result = block.

        IF block IS NOT INITIAL.
          block-type = <block_type_name>.

          IF block-identified IS INITIAL.
            APPEND current_block TO blocks.
            block-identified = abap_true.
          ENDIF.

          CONCATENATE 'block_' <block_type_name> '_continue' INTO method_name.
          TRANSLATE method_name TO UPPER CASE.

          IF line_exists( methods[ table_line = method_name ] ).
            block-continuable = abap_true.
          ENDIF.

          current_block = block.
          continue_to_next_line = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP. "ref_block_types->data

      IF continue_to_next_line IS NOT INITIAL.
        CLEAR continue_to_next_line.
        CONTINUE.
      ENDIF.

      " ~

      IF current_block IS NOT INITIAL AND
         current_block-type IS INITIAL AND
         current_block-interrupted IS INITIAL.
        CONCATENATE current_block-element-text-text %_newline text
          INTO current_block-element-text-text.
      ELSE.
        APPEND current_block TO blocks.

        current_block = paragraph( current_line ).

        current_block-identified = abap_true.
      ENDIF.

    ENDLOOP. "lines

    " ~

    IF current_block-continuable IS NOT INITIAL.
      CONCATENATE 'block_' current_block-type '_complete' INTO method_name.
      TRANSLATE method_name TO UPPER CASE.

      IF line_exists( methods[ table_line = method_name ] ).
        CALL METHOD (method_name)
          EXPORTING
            block  = current_block
          RECEIVING
            result = current_block.
      ENDIF.
    ENDIF.

    APPEND current_block TO blocks.
    DELETE blocks INDEX 1.

    " ~

    LOOP AT blocks ASSIGNING <block>.
      CHECK <block>-hidden IS INITIAL.

      IF <block>-markup IS NOT INITIAL.
        block_markup = <block>-markup.
      ELSE.
        block_markup = element( <block>-element ).
      ENDIF.
      CONCATENATE result %_newline block_markup INTO result RESPECTING BLANKS.
    ENDLOOP.

    CONCATENATE result %_newline INTO result RESPECTING BLANKS.

  ENDMETHOD.                    "lines
ENDCLASS.
