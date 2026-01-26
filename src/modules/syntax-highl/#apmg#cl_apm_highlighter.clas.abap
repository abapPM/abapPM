CLASS /apmg/cl_apm_highlighter DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

************************************************************************
* Syntax Highlighter
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    METHODS process_line
      IMPORTING
        !line         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS set_hidden_chars
      IMPORTING
        !hidden_chars TYPE abap_bool.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_match,
        token    TYPE c LENGTH 1,  " Type of matches
        offset   TYPE i,      " Beginning position of the string that should be formatted
        length   TYPE i,      " Length of the string that should be formatted
        text_tag TYPE string, " Type of text tag
      END OF ty_match,
      ty_match_tt TYPE STANDARD TABLE OF ty_match WITH KEY token offset length,
      BEGIN OF ty_rule,
        regex             TYPE REF TO cl_abap_regex,
        token             TYPE c LENGTH 1,
        style             TYPE string,
        relevant_submatch TYPE i,
      END OF ty_rule.

    CONSTANTS c_token_none TYPE c VALUE '.'.

    DATA rules TYPE STANDARD TABLE OF ty_rule WITH KEY regex token style.
    DATA hidden_chars TYPE abap_bool.

    METHODS add_rule
      IMPORTING
        !regex    TYPE string
        !token    TYPE c
        !style    TYPE string
        !submatch TYPE i OPTIONAL.

    METHODS parse_line
      IMPORTING
        !line         TYPE string
      RETURNING
        VALUE(result) TYPE ty_match_tt.

    METHODS order_matches
      IMPORTING
        !line    TYPE string
      CHANGING
        !matches TYPE ty_match_tt.

    METHODS extend_matches
      IMPORTING
        !line    TYPE string
      CHANGING
        !matches TYPE ty_match_tt.

    METHODS format_line
      IMPORTING
        !line         TYPE string
        !matches      TYPE ty_match_tt
      RETURNING
        VALUE(result) TYPE string.

    METHODS apply_style
      IMPORTING
        !line         TYPE string
        !class        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS is_whitespace
      IMPORTING
        !string          TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS show_hidden_chars
      IMPORTING
        !line         TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_highlighter IMPLEMENTATION.


  METHOD add_rule.

    DATA(rule) = VALUE ty_rule(
      token             = token
      style             = style
      relevant_submatch = submatch ).

    IF regex IS NOT INITIAL.
      rule-regex = NEW #(
        pattern     = regex
        ignore_case = abap_true ) ##REGEX_POSIX.
    ENDIF.

    APPEND rule TO rules.

  ENDMETHOD.


  METHOD apply_style.

    DATA(escaped) = escape(
      val    = line
      format = cl_abap_format=>e_html_text ).

    escaped = show_hidden_chars( escaped ).

    IF class IS NOT INITIAL.
      result = |<span class="{ class }">{ escaped }</span>|.
    ELSE.
      result = escaped.
    ENDIF.

  ENDMETHOD.


  METHOD extend_matches.

    DATA(line_len) = strlen( line ).

    SORT matches BY offset.

    " Add entries referring to parts of text that should not be formatted
    DATA(last_pos) = 0.
    DATA(length)   = 0.
    LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
      IF <match>-offset > last_pos.
        length = <match>-offset - last_pos.
        DATA(match) = VALUE ty_match(
          token  = c_token_none
          offset = last_pos
          length = length ).
        INSERT match INTO matches INDEX sy-tabix.
      ENDIF.
      last_pos = <match>-offset + <match>-length.
    ENDLOOP.

    " Add remainder of the string
    IF line_len > last_pos.
      match = VALUE ty_match(
        token  = c_token_none
        offset = last_pos
        length = line_len - last_pos ).
      APPEND match TO matches.
    ENDIF.

  ENDMETHOD.


  METHOD format_line.

    TRY.
        LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
          DATA(chunk) = substring( val = line
                                   off = <match>-offset
                                   len = <match>-length ).

          " Failed read equals no style
          READ TABLE rules INTO DATA(rule) WITH KEY token = <match>-token.
          IF sy-subrc = 0.
            chunk = apply_style( line  = chunk
                                 class = rule-style ).
          ELSE.
            chunk = apply_style( chunk ).
          ENDIF.

          result = result && chunk.
        ENDLOOP.
      CATCH cx_sy_range_out_of_bounds.
        " If issue with invalid substring, then return unformatted line
        result = line.
    ENDTRY.

  ENDMETHOD.


  METHOD is_whitespace.

    "/^\s+$/
    DATA(whitespace) = ` ` && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.

    rv_result = xsdbool( string CO whitespace ).

  ENDMETHOD.


  METHOD order_matches.
  ENDMETHOD.


  METHOD parse_line.

    " Process syntax-dependent regex table and find all matches
    LOOP AT rules ASSIGNING FIELD-SYMBOL(<regex>) WHERE regex IS BOUND.
      DATA(regex)   = <regex>-regex.
      DATA(matcher) = regex->create_matcher( text = line ).
      DATA(matches) = matcher->find_all( ).

      " Save matches into custom table with predefined tokens
      LOOP AT matches ASSIGNING FIELD-SYMBOL(<result>).
        IF <regex>-relevant_submatch = 0.
          DATA(match) = VALUE ty_match(
            token  = <regex>-token
            offset = <result>-offset
            length = <result>-length ).
          APPEND match TO result.
        ELSE.
          READ TABLE <result>-submatches ASSIGNING FIELD-SYMBOL(<submatch>) INDEX <regex>-relevant_submatch.
          "submatch might be empty if only discarded parts matched
          IF sy-subrc = 0 AND <submatch>-offset >= 0 AND <submatch>-length > 0.
            match = VALUE ty_match(
              token  = <regex>-token
              offset = <submatch>-offset
              length = <submatch>-length ).
            APPEND match TO result.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_line.

    IF line IS INITIAL OR is_whitespace( line ) = abap_true.
      result = show_hidden_chars( line ).
      RETURN.
    ENDIF.

    DATA(matches) = parse_line( line ).

    order_matches( EXPORTING line    = line
                   CHANGING  matches = matches ).

    extend_matches( EXPORTING line    = line
                    CHANGING  matches = matches ).

    result = format_line( line    = line
                          matches = matches ).

  ENDMETHOD.


  METHOD set_hidden_chars.

    me->hidden_chars = hidden_chars.

  ENDMETHOD.


  METHOD show_hidden_chars.

    TYPES ty_bom TYPE x LENGTH 3.

    result = line.

    IF hidden_chars = abap_true.
      " The order matters :-)
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN result WITH '&nbsp;&rarr;&nbsp;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN result WITH '&para;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN result WITH '&crarr;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1)       IN result WITH '&para;'.
      REPLACE ALL OCCURRENCES OF ` `                                    IN result WITH '&middot;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN result
        WITH '<span class="red">&odash;</span>'.

      IF strlen( result ) BETWEEN 1 AND 2.
        TRY.
            DATA(bom) = CONV ty_bom( lcl_out=>convert( result ) ).
          CATCH /apmg/cx_apm_error ##NO_HANDLER.
        ENDTRY.
        IF bom(2) = cl_abap_char_utilities=>byte_order_mark_big.
          result = '<span class="red">&squf;</span>'. " UTF-16 big-endian (FE FF)
        ENDIF.
        IF bom(2) = cl_abap_char_utilities=>byte_order_mark_little.
          result = '<span class="red">&compfn;</span>'. " UTF-16 little-endian (FF FE)
        ENDIF.
        IF bom(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
          result = '<span class="red">&curren;</span>'. " UTF-8 (EF BB BF)
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
