CLASS zcl_abappm_highlighter_txt DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_highlighter
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS process_line REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_highlighter_txt IMPLEMENTATION.


  METHOD process_line.

    rv_line = apply_style(
      iv_line  = iv_line
      iv_class = '' ).

  ENDMETHOD.
ENDCLASS.
