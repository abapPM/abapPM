CLASS zcl_abappm_highlighter_json DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_highlighter
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      " JSON... This was easy :-)
      " JSONC... With comments
      BEGIN OF c_css,
        keyword TYPE string VALUE 'selectors',              "#EC NOTEXT
        text    TYPE string VALUE 'text',                   "#EC NOTEXT
        values  TYPE string VALUE 'properties',             "#EC NOTEXT
        comment TYPE string VALUE 'comment',                "#EC NOTEXT
      END OF c_css,
      BEGIN OF c_token,
        keyword TYPE c VALUE 'K',                           "#EC NOTEXT
        text    TYPE c VALUE 'T',                           "#EC NOTEXT
        values  TYPE c VALUE 'V',                           "#EC NOTEXT
        comment TYPE c VALUE 'C',                           "#EC NOTEXT
      END OF c_token,
      BEGIN OF c_regex,
        " comments /* ... */ or //
        comment TYPE string VALUE '\/\*.*\*\/|\/\*|\*\/|\/\/', "#EC NOTEXT
        " not much here
        keyword TYPE string VALUE 'true|false|null',        "#EC NOTEXT
        " double quoted strings
        text    TYPE string VALUE '"',                      "#EC NOTEXT
      END OF c_regex.

    METHODS constructor.

  PROTECTED SECTION.

    METHODS order_matches REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_highlighter_json IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expression

    add_rule( regex = c_regex-keyword
              token = c_token-keyword
              style = c_css-keyword ).

    " Style for keys
    add_rule( regex = c_regex-text
              token = c_token-text
              style = c_css-text ).

    " Style for values
    add_rule( regex = ''
              token = c_token-values
              style = c_css-values ).

    " JSONC comments
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
    DATA(prev_end)   = ''.

    LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
      " Delete matches after open text match
      IF prev_token = c_token-text AND <match>-token <> c_token-text.
        CLEAR <match>-token.
        CONTINUE.
      ENDIF.

      DATA(match) = substring( val = line
                               off = <match>-offset
                               len = <match>-length ).

      IF <match>-token = c_token-text.
        <match>-text_tag = match.
        IF prev_token = c_token-text.
          IF <match>-text_tag = <prev_match>-text_tag.
            <prev_match>-length = <match>-offset + <match>-length - <prev_match>-offset.
            CLEAR prev_token.
          ENDIF.
          CLEAR <match>-token.
          CONTINUE.
        ENDIF.
      ENDIF.

      prev_token = <match>-token.
      ASSIGN <match> TO <prev_match>.
    ENDLOOP.

    DELETE matches WHERE token IS INITIAL.

    " Switch style of second text match to values
    DATA(count) = 0.
    LOOP AT matches ASSIGNING <match> WHERE token = c_token-text.
      count = count + 1.
      IF count >= 2.
        <match>-token = c_token-values.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
