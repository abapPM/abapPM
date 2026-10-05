CLASS /apmg/cl_apm_highlighter_xml DEFINITION
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
        comment  TYPE string VALUE 'comment',
      END OF c_css,
      BEGIN OF c_token,
        xml_tag  TYPE c VALUE 'X',
        attr     TYPE c VALUE 'A',
        attr_val TYPE c VALUE 'V',
        comment  TYPE c VALUE 'C',
      END OF c_token,
      BEGIN OF c_regex,
        " For XML tags, we will use a submatch
        " main pattern includes quoted strings so we can ignore < and > in attr values
        xml_tag  TYPE string VALUE '(?:"[^"]*")|(?:''[^'']*'')|(?:`[^`]*`)|([<>])',
        attr     TYPE string VALUE '(?:^|\s)[-a-z:_.0-9]+\s*(?==\s*["''`])',
        attr_val TYPE string VALUE '("[^"]*")|(''[^'']*'')|(`[^`]*`)',
        " comments <!-- ... -->
        comment  TYPE string VALUE '<!--(?:(?!-->).)*-->|<!--|-->',
      END OF c_regex.

    METHODS constructor.

  PROTECTED SECTION.

    DATA comment TYPE abap_bool.

    METHODS order_matches REDEFINITION.
    METHODS parse_line REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_highlighter_xml IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR comment.

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

    add_rule( regex = c_regex-comment
              token = c_token-comment
              style = c_css-comment ).

  ENDMETHOD.


  METHOD order_matches.

    FIELD-SYMBOLS <prev_match> TYPE ty_match.

    DATA(line_len)   = strlen( line ).
    DATA(prev_token) = ''.
    DATA(prev_end) = 0.
    DATA(state) = 'O'. " O - for open tag; C - for closed tag;

    " A continued comment ends at the first delimiter, regardless of its content.
    IF comment = abap_true.
      FIND FIRST OCCURRENCE OF '-->' IN line MATCH OFFSET DATA(comment_end).
      IF sy-subrc <> 0.
        CLEAR matches.
        APPEND INITIAL LINE TO matches ASSIGNING FIELD-SYMBOL(<match>).
        <match>-token = c_token-comment.
        <match>-offset = 0.
        <match>-length = line_len.
        RETURN.
      ENDIF.
      comment_end = comment_end + 3.
      DELETE matches WHERE offset < comment_end.
      APPEND VALUE #( token = c_token-comment offset = 0 length = comment_end ) TO matches.
      comment = abap_false.
    ENDIF.

    " Longest matches, including any continued comment prefix.
    SORT matches BY offset length DESCENDING.

    LOOP AT matches ASSIGNING <match>.
      DATA(index) = sy-tabix.

      " Ignore comment delimiters and nested quotes inside an accepted match.
      IF <match>-offset < prev_end.
        DELETE matches INDEX index.
        CONTINUE.
      ENDIF.

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
              <match>-length = <match>-offset - <prev_match>-offset - <prev_match>-length + <match>-length.
              <match>-offset = <prev_match>-offset + <prev_match>-length.
            ENDIF.
          ELSE.
            state = 'O'.
          ENDIF.

        WHEN c_token-comment.
          state = 'C'.
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
              DELETE matches WHERE offset > <match>-offset AND offset < cmmt_end.
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

      prev_end = <match>-offset + <match>-length.
      prev_token = <match>-token.
      ASSIGN <match> TO <prev_match>.
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


  METHOD parse_line.

    DATA(line_len) = strlen( line ).
    DATA(segment_start) = 0.
    DATA(scan_offset) = 0.
    DATA(pattern) = c_regex-attr_val && '|<!--'.

    " Comments are parsed separately so their quotes cannot hide subsequent tags.
    IF comment = abap_true.
      FIND FIRST OCCURRENCE OF '-->' IN line MATCH OFFSET DATA(comment_end).
      IF sy-subrc <> 0.
        APPEND VALUE #( token = c_token-comment offset = 0 length = line_len ) TO result.
        RETURN.
      ENDIF.
      segment_start = comment_end + 3.
      scan_offset = segment_start.
      APPEND VALUE #( token = c_token-comment offset = 0 length = segment_start ) TO result.
    ENDIF.

    WHILE scan_offset < line_len.
      FIND FIRST OCCURRENCE OF REGEX pattern IN line+scan_offset
        MATCH OFFSET DATA(found_offset) MATCH LENGTH DATA(found_length) ##REGEX_POSIX.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      found_offset = found_offset + scan_offset.
      scan_offset = found_offset + found_length.
      IF substring( val = line off = found_offset len = found_length ) <> '<!--'.
        CONTINUE.
      ENDIF.

      DATA(segment) = substring( val = line off = segment_start len = found_offset - segment_start ).
      DATA(segment_matches) = super->parse_line( segment ).
      LOOP AT segment_matches ASSIGNING FIELD-SYMBOL(<segment_match>).
        <segment_match>-offset = <segment_match>-offset + segment_start.
      ENDLOOP.
      APPEND LINES OF segment_matches TO result.

      DATA(comment_length) = 4.
      FIND FIRST OCCURRENCE OF '-->' IN line+scan_offset MATCH OFFSET comment_end.
      IF sy-subrc = 0.
        scan_offset = scan_offset + comment_end + 3.
        comment_length = scan_offset - found_offset.
      ELSE.
        scan_offset = line_len.
      ENDIF.
      APPEND VALUE #( token  = c_token-comment offset = found_offset
                      length = comment_length ) TO result.
      segment_start = scan_offset.
    ENDWHILE.

    segment = substring( val = line off = segment_start ).
    segment_matches = super->parse_line( segment ).
    LOOP AT segment_matches ASSIGNING <segment_match>.
      <segment_match>-offset = <segment_match>-offset + segment_start.
    ENDLOOP.
    APPEND LINES OF segment_matches TO result.

  ENDMETHOD.
ENDCLASS.
