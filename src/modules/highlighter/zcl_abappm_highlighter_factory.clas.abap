CLASS ZCL_ABAPPM_HIGHLIGHTER_FACTORY DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_filename       TYPE string
        !iv_hidden_chars   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_instance) TYPE REF TO ZCL_ABAPPM_HIGHLIGHTER.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPPM_HIGHLIGHTER_FACTORY IMPLEMENTATION.


  METHOD create.

    " Create instance of highlighter dynamically dependent on syntax type
    IF iv_filename CP '*.abap'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_ABAP.
    ELSEIF iv_filename CP '*.xml' OR iv_filename CP '*.html'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_XML.
    ELSEIF iv_filename CP '*.css'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_CSS.
    ELSEIF iv_filename CP '*.js'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_JS.
    ELSEIF iv_filename CP '*.json' OR iv_filename CP '*.jsonc'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_JSON.
    ELSEIF iv_filename CP '*.txt' OR iv_filename CP '*.ini'  OR iv_filename CP '*.text'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_TXT.
    ELSEIF iv_filename CP '*.md' OR iv_filename CP '*.markdown'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_MD.
    ELSEIF iv_filename CP '*.diff'.
      CREATE OBJECT ro_instance TYPE ZCL_ABAPPM_HIGHLIGHTER_DIFF.
    ELSE.
      CLEAR ro_instance.
    ENDIF.

    IF ro_instance IS BOUND.
      ro_instance->set_hidden_chars( iv_hidden_chars ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
