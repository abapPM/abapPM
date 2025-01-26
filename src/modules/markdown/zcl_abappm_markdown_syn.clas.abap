CLASS zcl_abappm_markdown_syn DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* Markdown Syntax highlighter
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS process
      IMPORTING
        !source       TYPE string
        !language     TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS process_line
      IMPORTING
        !line         TYPE string
        !language     TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      current_language TYPE string,
      highlighter      TYPE REF TO zcl_highlighter.

    CLASS-METHODS create
      IMPORTING
        !language     TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_highlighter.

ENDCLASS.



CLASS zcl_abappm_markdown_syn IMPLEMENTATION.


  METHOD create.
    result = zcl_highlighter_factory=>create( |.{ language }| ).

    IF result IS INITIAL.
      result = zcl_highlighter_factory=>create( |.txt| ).
    ENDIF.
  ENDMETHOD.


  METHOD process.

    DATA:
      line  TYPE string,
      lines TYPE string_table.

    highlighter = create( language ).

    SPLIT source AT cl_abap_char_utilities=>newline INTO TABLE lines.

    LOOP AT lines INTO line.
      IF result IS NOT INITIAL.
        result = result && cl_abap_char_utilities=>newline.
      ENDIF.
      result = result && highlighter->process_line( line ).
    ENDLOOP.

  ENDMETHOD.


  METHOD process_line.
    IF highlighter IS INITIAL OR language <> current_language.
      current_language    = language.
      highlighter = create( language ).
    ENDIF.

    IF highlighter IS BOUND.
      result = highlighter->process_line( line ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
