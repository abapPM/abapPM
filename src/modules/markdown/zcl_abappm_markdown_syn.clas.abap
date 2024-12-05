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
      go_highlighter TYPE REF TO zcl_abappm_highlighter.

    CLASS-METHODS create
      IMPORTING
        !iv_language       TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abappm_highlighter.

ENDCLASS.



CLASS zcl_abappm_markdown_syn IMPLEMENTATION.


  METHOD create.
    ro_instance = zcl_abappm_highlighter_factory=>create( |.{ iv_language }| ).

    IF ro_instance IS INITIAL.
      ro_instance = zcl_abappm_highlighter_factory=>create( |.txt| ).
    ENDIF.
  ENDMETHOD.


  METHOD process.

    DATA:
      lv_line  TYPE string,
      lt_lines TYPE TABLE OF string.

    go_highlighter = create( iv_language ).

    SPLIT iv_source AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.
      IF rv_source IS NOT INITIAL.
        rv_source = rv_source && cl_abap_char_utilities=>newline.
      ENDIF.
      rv_source = rv_source && go_highlighter->process_line( lv_line ).
    ENDLOOP.

  ENDMETHOD.


  METHOD process_line.
    IF go_highlighter IS INITIAL OR gv_language <> iv_language.
      gv_language    = iv_language.
      go_highlighter = create( iv_language ).
    ENDIF.

    IF go_highlighter IS BOUND.
      rv_line = go_highlighter->process_line( iv_line ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
