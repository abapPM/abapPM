CLASS /apmg/cl_apm_highlighter_po DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_highlighter
  FINAL
  CREATE PUBLIC.

************************************************************************
* Syntax Highlighter
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_style,
        msgid   TYPE string VALUE 'keyword',
        msgstr  TYPE string VALUE 'xml_tag',
        comment TYPE string VALUE 'comment',
      END OF c_style.
    CONSTANTS:
      BEGIN OF c_token,
        msgid   TYPE c VALUE 'I',
        msgstr  TYPE c VALUE 'S',
        comment TYPE c VALUE 'C',
      END OF c_token.
    CONSTANTS:
      BEGIN OF c_regex,
        msgid   TYPE string VALUE '^msgid\b',
        msgstr  TYPE string VALUE '^msgstr\b',
        comment TYPE string VALUE '^#.*',
      END OF c_regex.

ENDCLASS.



CLASS /apmg/cl_apm_highlighter_po IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    add_rule(
      regex = c_regex-msgid
      token = c_token-msgid
      style = c_style-msgid ).

    add_rule(
      regex = c_regex-msgstr
      token = c_token-msgstr
      style = c_style-msgstr ).

    add_rule(
      regex = c_regex-comment
      token = c_token-comment
      style = c_style-comment ).

    " TODO maybe add rule to highlight empty msgstr with red

  ENDMETHOD.
ENDCLASS.
