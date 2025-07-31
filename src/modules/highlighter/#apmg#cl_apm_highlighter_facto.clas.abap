CLASS /apmg/cl_apm_highlighter_facto DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

************************************************************************
* Syntax Highlighter
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !filename     TYPE string
        !hidden_chars TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_highlighter.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_highlighter_facto IMPLEMENTATION.


  METHOD create.

    " Create instance of highlighter dynamically dependent on syntax type
    IF filename CP '*.abap'.
      result = NEW /apmg/cl_apm_highlighter_abap( ).
    ELSEIF filename CP '*.xml' OR filename CP '*.html'.
      result = NEW /apmg/cl_apm_highlighter_xml( ).
    ELSEIF filename CP '*.css' OR filename CP '*.scss' OR filename CP '*.sass'.
      result = NEW /apmg/cl_apm_highlighter_css( ).
    ELSEIF filename CP '*.js'.
      result = NEW /apmg/cl_apm_highlighter_js( ).
    ELSEIF filename CP '*.json' OR filename CP '*.jsonc' OR filename CP '*.json5'.
      result = NEW /apmg/cl_apm_highlighter_json( ).
    ELSEIF filename CP '*.txt' OR filename CP '*.ini'  OR filename CP '*.text'.
      result = NEW /apmg/cl_apm_highlighter_txt( ).
    ELSEIF filename CP '*.md' OR filename CP '*.markdown'.
      result = NEW /apmg/cl_apm_highlighter_md( ).
    ELSEIF filename CP '*.diff'.
      result = NEW /apmg/cl_apm_highlighter_diff( ).
    ELSE.
      CLEAR result.
    ENDIF.

    IF result IS BOUND.
      result->set_hidden_chars( hidden_chars ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
