CLASS zcl_abappm_highlighter_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_filename       TYPE string
        !iv_hidden_chars   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abappm_highlighter.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_highlighter_factory IMPLEMENTATION.


  METHOD create.

    " Create instance of highlighter dynamically dependent on syntax type
    IF iv_filename CP '*.abap'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_abap.
    ELSEIF iv_filename CP '*.xml' OR iv_filename CP '*.html'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_xml.
    ELSEIF iv_filename CP '*.css'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_css.
    ELSEIF iv_filename CP '*.js'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_js.
    ELSEIF iv_filename CP '*.json' OR iv_filename CP '*.jsonc'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_json.
    ELSEIF iv_filename CP '*.txt' OR iv_filename CP '*.ini'  OR iv_filename CP '*.text'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_txt.
    ELSEIF iv_filename CP '*.md' OR iv_filename CP '*.markdown'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_md.
    ELSEIF iv_filename CP '*.diff'.
      CREATE OBJECT ro_instance TYPE zcl_abappm_highlighter_diff.
    ELSE.
      CLEAR ro_instance.
    ENDIF.

    IF ro_instance IS BOUND.
      ro_instance->set_hidden_chars( iv_hidden_chars ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
