CLASS /apmg/cl_apm_code_import_rules DEFINITION PUBLIC FINAL CREATE PUBLIC.

************************************************************************
* apm Code Import Rules
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* TODO: replace logging with ABAP Logger (wait for v2 of it)
************************************************************************
  PUBLIC SECTION.


    CLASS-METHODS get
      IMPORTING
        !programs     TYPE /apmg/if_apm_importer=>ty_programs
        !is_logging   TYPE abap_bool
        !default_rule TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_rules
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_width TYPE i VALUE 150.

    CLASS-METHODS get_import_rules
      IMPORTING
        !program      TYPE /apmg/if_apm_importer=>ty_program
        !is_logging   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_rules
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_old_object
      IMPORTING
        !token        TYPE stokesx
        !pos          TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_rule-old_object
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_new_object
      IMPORTING
        !token        TYPE stokesx
        !pos          TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_rule-new_object
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_module_name
      IMPORTING
        !token        TYPE stokesx
        !pos          TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_importer=>ty_rule-name
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS check_result
      IMPORTING
        !result     TYPE /apmg/if_apm_importer=>ty_rules
        !is_logging TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_code_import_rules IMPLEMENTATION.


  METHOD check_result.

    LOOP AT result INTO DATA(rule).
      LOOP AT result FROM sy-tabix INTO DATA(rule_check)
        WHERE name <> rule-name AND target_package = rule-target_package.

        IF is_logging = abap_true.
          FORMAT COLOR COL_TOTAL.
          WRITE: / rule-old_object,
            AT 37 rule-target_package, AT 69 rule-new_object,
            AT 94 rule-name, AT 120 rule-version, AT c_width space.

          FORMAT COLOR COL_NEGATIVE.
          WRITE: / rule_check-old_object,
            AT 37 rule_check-target_package, AT 69 rule_check-new_object,
            AT 94 rule_check-name, AT 120 rule_check-version, AT c_width space.
        ENDIF.

        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text     = 'Inconsistent mapping of modules to SAP packages'
            longtext = |Modules { rule-name } and { rule_check-name } are mapped to SAP package | &&
                       |{ rule-target_package }. Specify a different SAP package in "TO" (see documentation)|.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


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

    DATA rule TYPE /apmg/if_apm_importer=>ty_rule.

    DATA(tokens) = /apmg/cl_apm_code_importer=>scan( program-program ).

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
      DATA(tabix) = sy-tabix.
      CLEAR rule.
      DO 5 TIMES.
        DATA(pos) = |(Tabix { tabix } Index { sy-index })|.
        READ TABLE tokens ASSIGNING FIELD-SYMBOL(<token>) INDEX tabix + sy-index.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
            EXPORTING
              text = |Error parsing IMPORT statement { pos }|.
        ENDIF.

        CASE sy-index.
          WHEN 1.
            " mapping from old object
            rule-old_object = get_old_object( token = <token> pos = pos ).

          WHEN 2.
            IF <token>-type <> 'b' OR <token>-str <> 'TO'.
              RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
                EXPORTING
                  text = |Error parsing IMPORT statement. Expecting "TO" { pos }|.
            ENDIF.
          WHEN 3.
            " mapping to new object and package
            rule-new_object = get_new_object( token = <token> pos = pos ).

            " Check if mapping includes a specific target package
            DATA(separator) = ''.
            IF rule-new_object CS ':'.
              separator = ':'.
            ENDIF.

            IF separator IS NOT INITIAL.
              SPLIT rule-new_object AT separator INTO rule-target_package rule-new_object.
              " Install into a sub package of where the IMPORT was found
              " Note: This assumes prefix folder mode
              " FUTURE: support full and mixed folder modes
              rule-parent_package = program-package.
              rule-target_package = |{ program-package }_{ rule-target_package }|.
            ENDIF.

          WHEN 4.
            IF <token>-type <> 'b' OR <token>-str <> 'FROM'.
              RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
                EXPORTING
                  text = |Error parsing IMPORT statement. Expecting "FROM" { pos }|.
            ENDIF.

          WHEN 5.
            " Module name
            rule-name = get_module_name( token = <token> pos = pos ).

            " name, @scope/name, name@x.y.z, @scope/name@x.y.z, name@tag, @scope/name@tag
            IF rule-name+1(*) CS '@'.
              SPLIT rule-name AT '@' INTO rule-name rule-version.
            ELSE.
              rule-version = 'latest'.
            ENDIF.

            " Default mapping uses module name
            " Note: this might truncate or lead to conflicts which we check for below
            IF rule-target_package IS INITIAL.
              DATA(target_package) = to_upper( rule-name ).
              IF target_package(1) = '@' AND target_package CS '/'.
                SPLIT target_package AT '/' INTO DATA(rest) target_package ##NEEDED.
              ENDIF.

              " Install into a sub package of where the IMPORT was found
              " Note: This assumes prefix folder mode
              " FUTURE: support full and mixed folder modes
              rule-parent_package = program-package.
              rule-target_package = |{ program-package }_{ target_package }|.
            ENDIF.
        ENDCASE.
      ENDDO.

      INSERT rule INTO TABLE result.
    ENDLOOP.

    " Check that we don't have mapping of different modules to same SAP package
    check_result(
      result     = result
      is_logging = is_logging ).

    IF is_logging = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Rules:', AT c_width space.
      SKIP.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<rule>).
        FORMAT COLOR COL_POSITIVE.
        WRITE: / <rule>-old_object,
          AT 37 <rule>-target_package, AT 69 <rule>-new_object,
          AT 94 <rule>-name, AT 120 <rule>-version, AT c_width space.
      ENDLOOP.
      SKIP.
      FORMAT COLOR OFF.
    ENDIF.

  ENDMETHOD.


  METHOD get_module_name.

    CASE token-type.
      WHEN 'r'. " reference
        result = token-str.
      WHEN '!' OR 'm'. " literal
        result = to_lower( replace(
          val  = token-str
          sub  = ''''
          with = ''
          occ  = 0 ) ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Unknown identifier: { token-str } { pos }|.
    ENDCASE.

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Initial package spec: { token-str } { pos }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_new_object.

    CASE token-type.
      WHEN 'r'. " reference
        result = token-str.
      WHEN '!' OR 'm'. " literal
        result = to_upper( replace(
          val  = token-str
          sub  = ''''
          with = ''
          occ  = 0 ) ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Unknown identifier: { token-str } { pos }|.
    ENDCASE.

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Initial new object: { token-str } { pos }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_old_object.

    CASE token-type.
      WHEN 'r'. " reference
        result = token-str.
      WHEN '!' OR 'm'. " literal
        result = to_upper( replace(
          val  = token-str
          sub  = ''''
          with = ''
          occ  = 0 ) ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Unknown identifier: { token-str } { pos }|.
    ENDCASE.

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Initial original object: { token-str } { pos }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
