CLASS /apmg/cl_apm_highlighter_txt DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_highlighter
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS process_line REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_highlighter_txt IMPLEMENTATION.


  METHOD process_line.

    result = apply_style(
      line  = line
      class = '' ).

  ENDMETHOD.
ENDCLASS.
