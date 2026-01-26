CLASS /apmg/cl_apm_highlighter_yaml DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_highlighter
  CREATE PUBLIC.

************************************************************************
* Syntax Highlighter
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
* Basic YAML formatting
* https://yaml.org/
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_css,
        keyword TYPE string VALUE 'selectors',
        text    TYPE string VALUE 'text',
        values  TYPE string VALUE 'properties',
        comment TYPE string VALUE 'comment',
        attr    TYPE string VALUE 'attr',
      END OF c_css,
      BEGIN OF c_token,
        keyword TYPE c VALUE 'K',
        text    TYPE c VALUE 'T',
        values  TYPE c VALUE 'V',
        comment TYPE c VALUE 'C',
        attr    TYPE c VALUE 'A',
      END OF c_token,
      BEGIN OF c_regex,
        " comments #
        comment TYPE string VALUE '#.+',
        " keywords
        keyword TYPE string VALUE '[-_a-zA-Z0-9]+',
        " not much here
        values  TYPE string VALUE 'true|false|null',
        " double quoted strings
        text    TYPE string VALUE '"',
        " YAML collections, structures, scalars, tags
        attr    TYPE string VALUE '- |: |---|\.\.\.|\[|\]|\{|\}|&|\*|\? |>|!|\|',
      END OF c_regex.

    METHODS constructor.

  PROTECTED SECTION.

    METHODS order_matches REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_highlighter_yaml IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expression

    " Comments
    add_rule( regex = c_regex-comment
              token = c_token-comment
              style = c_css-comment ).

    " Keywords
    add_rule( regex = c_regex-keyword
              token = c_token-keyword
              style = c_css-keyword ).

    " Style for keys
    add_rule( regex = c_regex-text
              token = c_token-text
              style = c_css-text ).

    " Style for values
    add_rule( regex = c_regex-values
              token = c_token-values
              style = c_css-values ).

    " YAML collections, structures, scalars, tags
    add_rule( regex = c_regex-attr
              token = c_token-attr
              style = c_css-attr ).

  ENDMETHOD.


  METHOD order_matches.

    FIELD-SYMBOLS <prev_match> TYPE ty_match.

    " Longest matches
    SORT matches BY offset length DESCENDING.

    DATA(prev_token) = ''.

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
