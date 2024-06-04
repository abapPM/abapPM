CLASS zcl_abappm_markdown_syn DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* Markdown Syntax Highlighter
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS process
      IMPORTING
        !iv_source       TYPE string
        !iv_language     TYPE string
      RETURNING
        VALUE(rv_source) TYPE string.

    CLASS-METHODS process_line
      IMPORTING
        !iv_line       TYPE string
        !iv_language   TYPE string
      RETURNING
        VALUE(rv_line) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      gv_language    TYPE string,
      go_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter.

    CLASS-METHODS create
      IMPORTING
        !iv_language       TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_syntax_highlighter.

ENDCLASS.



CLASS zcl_abappm_markdown_syn IMPLEMENTATION.


  METHOD create.
    CASE iv_language.
      WHEN 'markdown'.
        CREATE OBJECT ro_instance TYPE zcl_abappm_markdown_synmd.
      WHEN 'diff'.
        CREATE OBJECT ro_instance TYPE zcl_abappm_markdown_syndi.
      WHEN OTHERS.
        ro_instance = zcl_abapgit_syntax_factory=>create( |.{ iv_language }| ).
    ENDCASE.

    IF ro_instance IS INITIAL.
      ro_instance = zcl_abapgit_syntax_factory=>create( |.txt| ).
    ENDIF.
  ENDMETHOD.


  METHOD process.

    DATA:
      lv_line  TYPE string,
      lt_lines TYPE TABLE OF string.

    go_highlighter = create( iv_language ).

    SPLIT iv_source AT %_newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.
      IF rv_source IS NOT INITIAL.
        rv_source = rv_source && %_newline.
      ENDIF.
      rv_source = rv_source && go_highlighter->process_line( lv_line ).
    ENDLOOP.

  ENDMETHOD.


  METHOD process_line.
    IF go_highlighter IS INITIAL OR gv_language <> iv_language.
      gv_language = iv_language.
      go_highlighter = create( iv_language ).
    ENDIF.

    rv_line = go_highlighter->process_line( iv_line ).
  ENDMETHOD.
ENDCLASS.
