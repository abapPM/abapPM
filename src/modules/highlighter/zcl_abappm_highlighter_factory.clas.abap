CLASS zcl_abappm_highlighter_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !filename     TYPE string
        !hidden_chars TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_highlighter.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_highlighter_factory IMPLEMENTATION.


  METHOD create.

    " Create instance of highlighter dynamically dependent on syntax type
    IF filename CP '*.abap'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_abap.
    ELSEIF filename CP '*.xml' OR filename CP '*.html'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_xml.
    ELSEIF filename CP '*.css'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_css.
    ELSEIF filename CP '*.js'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_js.
    ELSEIF filename CP '*.json' OR filename CP '*.jsonc'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_json.
    ELSEIF filename CP '*.txt' OR filename CP '*.ini'  OR filename CP '*.text'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_txt.
    ELSEIF filename CP '*.md' OR filename CP '*.markdown'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_md.
    ELSEIF filename CP '*.diff'.
      CREATE OBJECT result TYPE zcl_abappm_highlighter_diff.
    ELSE.
      CLEAR result.
    ENDIF.

    IF result IS BOUND.
      result->set_hidden_chars( hidden_chars ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
