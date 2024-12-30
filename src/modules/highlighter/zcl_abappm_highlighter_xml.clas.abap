CLASS zcl_abappm_highlighter_xml DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_highlighter
  CREATE PUBLIC.

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
        attr     TYPE string VALUE '(?:^|\s)[-a-z:_0-9]+\s*(?==\s*["|''|`])',
        attr_val TYPE string VALUE '("[^"]*")|(''[^'']*'')|(`[^`]*`)',
        " comments <!-- ... -->
        comment  TYPE string VALUE '[\<]!--.*--[\>]|[\<]!--|--[\>]',
      END OF c_regex.

    METHODS constructor.

  PROTECTED SECTION.

    CLASS-DATA comment TYPE abap_bool.

    METHODS order_matches REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_highlighter_xml IMPLEMENTATION.


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

    " Longest matches
    SORT matches BY offset length DESCENDING.

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
              <match>-length = <match>-offset - <prev_match>-offset - <prev_match>-length + <match>-length.
              <match>-offset = <prev_match>-offset + <prev_match>-length.
            ENDIF.
          ELSE.
            state = 'O'.
          ENDIF.

        WHEN c_token-comment.
          IF match = '<!--'.
            DELETE matches WHERE offset > <match>-offset.
            DELETE matches WHERE offset = <match>-offset AND token = c_token-xml_tag.
            <match>-length = line_len - <match>-offset.
            comment = abap_true.
          ELSEIF match = '-->'.
            DELETE matches WHERE offset < <match>-offset.
            <match>-length = <match>-offset + 3.
            <match>-offset = 0.
            comment = abap_false.
          ELSE.
            DATA(cmmt_end) = <match>-offset + <match>-length.
            DELETE matches WHERE offset > <match>-offset AND offset <= cmmt_end.
            DELETE matches WHERE offset = <match>-offset AND token = c_token-xml_tag.
          ENDIF.

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
    ENDLOOP.

    "if the last XML tag is not closed, extend it to the end of the tag
    IF prev_token = c_token-xml_tag
        AND <prev_match> IS ASSIGNED
        AND <prev_match>-length  = 1
        AND <prev_match>-text_tag = '<'.

      FIND REGEX '<\s*[^\s]*' IN line+<prev_match>-offset MATCH LENGTH <prev_match>-length.
      IF sy-subrc <> 0.
        <prev_match>-length = 1.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
