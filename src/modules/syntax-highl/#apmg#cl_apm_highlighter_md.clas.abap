CLASS /apmg/cl_apm_highlighter_md DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_highlighter
  CREATE PUBLIC.

************************************************************************
* Syntax Highlighter
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_css,
        xml_tag  TYPE string VALUE 'xml_tag',
        attr     TYPE string VALUE 'attr',
        attr_val TYPE string VALUE 'attr_val',
        heading  TYPE string VALUE 'heading',
        link     TYPE string VALUE 'link',
        url      TYPE string VALUE 'url',
        strong   TYPE string VALUE 'strong',
        emphasis TYPE string VALUE 'emphasis',
        comment  TYPE string VALUE 'comment',
      END OF c_css,
      BEGIN OF c_token,
        xml_tag  TYPE c VALUE 'X',
        attr     TYPE c VALUE 'A',
        attr_val TYPE c VALUE 'V',
        heading  TYPE c VALUE 'H',
        link     TYPE c VALUE 'L',
        url      TYPE c VALUE 'U',
        strong   TYPE c VALUE 'S',
        emphasis TYPE c VALUE 'E',
        comment  TYPE c VALUE 'C',
      END OF c_token,
      BEGIN OF c_regex,
        xml_tag  TYPE string VALUE '(?:"[^"]*")|(?:''[^'']*'')|(?:`[^`]*`)|([<>])',
        attr     TYPE string VALUE '(?:^|\s)[-a-z:_0-9]+\s*(?==\s*["|''|`])',
        attr_val TYPE string VALUE '("[^"]*")|(''[^'']*'')|(`[^`]*`)',
        heading  TYPE string VALUE '^#\s*(.*)',
        link     TYPE string VALUE '\[[^]]+\]',
        url      TYPE string VALUE `http[s]*://[^>"'\)\s]+`,
        strong   TYPE string VALUE '\*\*[^*]+\*\*',
        emphasis TYPE string VALUE '__[^_]+__',
        " comments <!-- ... -->
        comment  TYPE string VALUE '[\<]!--.*--[\>]|[\<]!--|--[\>]',
      END OF c_regex.

    METHODS constructor.

  PROTECTED SECTION.

    CLASS-DATA comment TYPE abap_bool.

    METHODS order_matches REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_highlighter_md IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expressions

    add_rule( regex    = c_regex-xml_tag
              token    = c_token-xml_tag
              style    = c_css-xml_tag
              submatch = 1 ).

    add_rule( regex = c_regex-attr
              token = c_token-attr
              style = c_css-attr ).

    add_rule( regex = c_regex-attr_val
              token = c_token-attr_val
              style = c_css-attr_val ).

    add_rule( regex = c_regex-heading
              token = c_token-heading
              style = c_css-heading ).

    add_rule( regex = c_regex-link
              token = c_token-link
              style = c_css-link ).

    add_rule( regex = c_regex-url
              token = c_token-url
              style = c_css-url ).

    " TODO: Rules for strong and emphasis conflict with others
    " add_rule( regex = c_regex-strong
    "           token = c_token-strong
    "           style = c_css-strong )
    " add_rule( regex = c_regex-emphasis
    "           token = c_token-emphasis
    "           style = c_css-emphasis )

    add_rule( regex = c_regex-comment
              token = c_token-comment
              style = c_css-comment ).

  ENDMETHOD.


  METHOD order_matches.

    FIELD-SYMBOLS <prev_match> TYPE ty_match.

    SORT matches BY offset.

    DATA(line_len)   = strlen( line ).
    DATA(prev_token) = ''.
    DATA(state) = 'O'. " O - for open tag; C - for closed tag;

    " Check if this is part of multi-line comment and mark it accordingly
    IF comment = abap_true.
      IF NOT line_exists( matches[ token = c_token-comment ] ).
        CLEAR matches.
        APPEND INITIAL LINE TO matches ASSIGNING FIELD-SYMBOL(<match>).
        <match>-token = c_token-comment.
        <match>-offset = 0.
        <match>-length = line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT matches ASSIGNING <match>.
      DATA(index) = sy-tabix.

      DATA(match) = substring( val = line
                               off = <match>-offset
                               len = <match>-length ).

      CASE <match>-token.
        WHEN c_token-xml_tag.
          <match>-text_tag = match.

          " No other matches between two tags
          IF <match>-text_tag = '>' AND prev_token = c_token-xml_tag.
            state = 'C'.
            <prev_match>-length = <match>-offset - <prev_match>-offset + <match>-length.
            DELETE matches INDEX index.
            CONTINUE.

            " Adjust length and offset of closing tag
          ELSEIF <match>-text_tag = '>' AND prev_token <> c_token-xml_tag.
            state = 'C'.
            IF <prev_match> IS ASSIGNED.
              DATA(new_len) = <match>-offset - <prev_match>-offset - <prev_match>-length + <match>-length.
              IF new_len < 0.
                " Something went wrong. Ignore the match
                DELETE matches INDEX index.
                CONTINUE.
              ENDIF.
              <match>-length = new_len.
              <match>-offset = <prev_match>-offset + <prev_match>-length.
            ENDIF.
          ELSE.
            state = 'O'.
          ENDIF.

        WHEN c_token-comment.
          CASE match.
            WHEN '<!--'.
              DELETE matches WHERE offset > <match>-offset.
              DELETE matches WHERE offset = <match>-offset AND token = c_token-xml_tag.
              <match>-length = line_len - <match>-offset.
              comment = abap_true.
            WHEN '-->'.
              DELETE matches WHERE offset < <match>-offset.
              <match>-length = <match>-offset + 3.
              <match>-offset = 0.
              comment = abap_false.
            WHEN OTHERS.
              DATA(cmmt_end) = <match>-offset + <match>-length.
              DELETE matches WHERE offset > <match>-offset AND offset <= cmmt_end.
              DELETE matches WHERE offset = <match>-offset AND token = c_token-xml_tag.
          ENDCASE.

        WHEN OTHERS.
          IF prev_token = c_token-xml_tag.
            <prev_match>-length = <match>-offset - <prev_match>-offset. " Extend length of the opening tag
          ENDIF.

          IF state = 'C'.  " Delete all matches between tags
            DELETE matches INDEX index.
            CONTINUE.
          ENDIF.

      ENDCASE.

      prev_token = <match>-token.
      ASSIGN <match> TO <prev_match>.
      CHECK sy-subrc >= 0. "abaplint false positive
    ENDLOOP.

    "if the last XML tag is not closed, extend it to the end of the tag
    IF prev_token = c_token-xml_tag
        AND <prev_match> IS ASSIGNED
        AND <prev_match>-length  = 1
        AND <prev_match>-text_tag = '<'.

      FIND REGEX '<\s*[^\s]*' IN line+<prev_match>-offset MATCH LENGTH <prev_match>-length ##REGEX_POSIX.
      IF sy-subrc <> 0.
        <prev_match>-length = 1.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
