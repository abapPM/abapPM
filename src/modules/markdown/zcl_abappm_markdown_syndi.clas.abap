CLASS zcl_abappm_markdown_syndi DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_syntax_highlighter
  CREATE PUBLIC.

************************************************************************
* Markdown Syntax Highlighter Diff
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_css,
        ins     TYPE string VALUE 'diff_ins',
        del     TYPE string VALUE 'diff_del',
        test    TYPE string VALUE 'diff_upd',
        comment TYPE string VALUE 'comment',
      END OF c_css.
    CONSTANTS:
      BEGIN OF c_token,
        ins     TYPE c VALUE 'I',
        del     TYPE c VALUE 'D',
        test    TYPE c VALUE 'T',
        comment TYPE c VALUE 'C',
      END OF c_token.
    CONSTANTS:
      BEGIN OF c_regex,
        ins     TYPE string VALUE '^\+.*',
        del     TYPE string VALUE '^-.*',
        test    TYPE string VALUE '^!.*',
        comment TYPE string VALUE '^#.*',
      END OF c_regex.

    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_markdown_syndi IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expressions

    add_rule( iv_regex = c_regex-ins
              iv_token = c_token-ins
              iv_style = c_css-ins ).

    add_rule( iv_regex = c_regex-del
              iv_token = c_token-del
              iv_style = c_css-del ).

    add_rule( iv_regex = c_regex-test
              iv_token = c_token-test
              iv_style = c_css-test ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.
ENDCLASS.
