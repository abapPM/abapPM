CLASS zcl_abappm_code_import_rules DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    " TODO: replace logging with ABAP Logger (wait for v2 of it)

    CLASS-METHODS get
      IMPORTING
        !programs     TYPE zif_abappm_importer=>ty_programs
        !is_logging   TYPE abap_bool
        !default_rule TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_importer=>ty_rules
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_width TYPE i VALUE 150.

    CLASS-METHODS get_import_rules
      IMPORTING
        !program      TYPE zif_abappm_importer=>ty_program
        !is_logging   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE zif_abappm_importer=>ty_rules
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_code_import_rules IMPLEMENTATION.


  METHOD get.

    LOOP AT programs ASSIGNING FIELD-SYMBOL(<program>).
      IF is_logging = abap_true.
        FORMAT COLOR COL_HEADING.
        WRITE: / 'Include:', AT c_width space.
        SKIP.
        FORMAT COLOR OFF.
        WRITE: / <program>-program, AT c_width space.
        SKIP.
      ENDIF.

      DATA(rules) = get_import_rules(
        program    = <program>
        is_logging = is_logging ).

      INSERT LINES OF rules INTO TABLE result.
    ENDLOOP.

    LOOP AT result ASSIGNING FIELD-SYMBOL(<rule>) WHERE old_object = '*'.
      <rule>-old_object = default_rule.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_import_rules.

    DATA:
      tabix  TYPE sy-tabix,
      pos    TYPE string,
      tokens TYPE TABLE OF stokesx,
      rule   TYPE zif_abappm_importer=>ty_rule.

    tokens = zcl_abappm_code_importer=>scan( program-program ).

    " Process all IMPORT statements
    "
    " Example 1:
    "
    " IMPORT zif_ajson TO zif_my_app_ajson FROM '@sbcgua/ajson'.
    "
    " IMPORT  c
    " ZIF_AJSON  r
    " TO  b
    " ZIF_MY_APP_AJSON  m
    " FROM  b
    " '@sbcgua/ajson'  !
    "
    " Example 2:
    "
    " IMPORT '*' TO 'z$1_my_app$2' FROM '@sbcgua/ajson'.
    "
    " IMPORT  c
    " '*'  !
    " TO  b
    " 'z$1_my_app$2'  m
    " FROM  b
    " '@sbcgua/ajson'  m
    LOOP AT tokens TRANSPORTING NO FIELDS WHERE type = 'c' AND str = 'IMPORT'.
      tabix = sy-tabix.
      CLEAR rule.
      DO 5 TIMES.
        pos = |Tabix { tabix } Index { sy-index }|.
        READ TABLE tokens ASSIGNING FIELD-SYMBOL(<token>) INDEX tabix + sy-index.
        IF sy-subrc <> 0.
          zcx_abappm_error=>raise( |Error parsing IMPORT statement. { pos }| ).
        ENDIF.

        CASE sy-index.
          WHEN 1.
            CASE <token>-type.
              WHEN 'r'. " reference
                rule-old_object = <token>-str.
              WHEN '!' OR 'm'. " literal
                rule-old_object = to_upper( replace(
                  val  = <token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
              WHEN OTHERS.
                zcx_abappm_error=>raise( |Unknown identifier { <token>-str }. { pos }| ).
            ENDCASE.
          WHEN 2.
            IF <token>-type <> 'b' OR <token>-str <> 'TO'.
              zcx_abappm_error=>raise( |Error parsing IMPORT statement. Expecting "TO". { pos }| ).
            ENDIF.
          WHEN 3.
            CASE <token>-type.
              WHEN 'r'. " reference
                rule-new_object = <token>-str.
              WHEN '!' OR 'm'. " literal
                rule-new_object = to_upper( replace(
                  val  = <token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
                IF rule-new_object CS '/'.
                  SPLIT rule-new_object AT '/' INTO rule-target_package rule-new_object.
                  " Install into a sub package of where the IMPORT was found
                  " Note: new_package is the folder name, which is mapped to
                  " an ABAP package based on prefix folder rules
                  rule-target_package = |{ program-package }_{ rule-target_package }|.
                  " FUTURE: support full and mixed folder modes
                ELSE.
                  " Install into the same package where the IMPORT was found
                  rule-target_package = program-package.
                ENDIF.
              WHEN OTHERS.
                zcx_abappm_error=>raise( |Unknown identifier { <token>-str }. { pos }| ).
            ENDCASE.
          WHEN 4.
            IF <token>-type <> 'b' OR <token>-str <> 'FROM'.
              zcx_abappm_error=>raise( |Error parsing IMPORT statement. Expecting "FROM". { pos }| ).
            ENDIF.
          WHEN 5.
            CASE <token>-type.
              WHEN 'r'. " reference
                rule-name = <token>-str.
              WHEN '!' OR 'm'. " literal
                rule-name = to_lower( replace(
                  val  = <token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
              WHEN OTHERS.
                zcx_abappm_error=>raise( |Unknown identifier { <token>-str }. { pos }| ).
            ENDCASE.
            IF rule-name IS INITIAL.
              zcx_abappm_error=>raise( |Initial package spec { <token>-str }. { pos }| ).
            ENDIF.
            " name, @scope/name, name@x.y.z, @scope/name@x.y.z, name@tag, @scope/name@tag
            IF rule-name+1(*) CS '@'.
              SPLIT rule-name AT '@' INTO rule-name rule-version.
            ELSE.
              rule-version = 'latest'.
            ENDIF.
        ENDCASE.
      ENDDO.

      INSERT rule INTO TABLE result.
    ENDLOOP.

    IF is_logging = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Rules:', AT c_width space.
      SKIP.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<rule>).
        FORMAT COLOR COL_POSITIVE.
        WRITE: /5 <rule>-old_object,
          AT 37 <rule>-target_package, AT 69 <rule>-new_object,
          AT 94 <rule>-name, AT 120 <rule>-version, AT c_width space.
      ENDLOOP.
      SKIP.
      FORMAT COLOR OFF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
