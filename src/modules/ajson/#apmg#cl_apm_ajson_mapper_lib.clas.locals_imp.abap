CLASS lcl_rename IMPLEMENTATION.

  METHOD constructor.
    mt_rename_map = it_rename_map.
    mv_rename_by = iv_rename_by.
  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_mapper~rename_node.

    DATA lv_full_path TYPE string.
    DATA lv_pair_found TYPE abap_bool.
    FIELD-SYMBOLS <r> LIKE LINE OF mt_rename_map.

    CASE mv_rename_by.
      WHEN /apmg/cl_apm_ajson_mapper_lib=>rename_by-attr_name.
        READ TABLE mt_rename_map ASSIGNING <r> WITH TABLE KEY by_name COMPONENTS from = cv_name.
        lv_pair_found = boolc( sy-subrc = 0 ).
      WHEN /apmg/cl_apm_ajson_mapper_lib=>rename_by-full_path.
        lv_full_path = is_node-path && cv_name.
        READ TABLE mt_rename_map ASSIGNING <r> WITH TABLE KEY by_name COMPONENTS from = lv_full_path.
        lv_pair_found = boolc( sy-subrc = 0 ).
      WHEN /apmg/cl_apm_ajson_mapper_lib=>rename_by-pattern.
        lv_full_path = is_node-path && cv_name.
        LOOP AT mt_rename_map ASSIGNING <r>.
          IF lv_full_path CP <r>-from.
            lv_pair_found = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      WHEN OTHERS.
        lv_pair_found = abap_false. " No rename
    ENDCASE.

    IF lv_pair_found = abap_true.
      cv_name = <r>-to.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_mapping_to_upper IMPLEMENTATION.

  METHOD /apmg/if_apm_ajson_mapper~rename_node.
    cv_name = to_upper( cv_name ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_mapping_to_lower IMPLEMENTATION.

  METHOD /apmg/if_apm_ajson_mapper~rename_node.
    cv_name = to_lower( cv_name ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_compound_mapper IMPLEMENTATION.

  METHOD constructor.
    mt_queue = it_queue.
  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_mapper~rename_node.

    DATA ls_node LIKE is_node.
    DATA li_mapper LIKE LINE OF mt_queue.

    ls_node = is_node.

    LOOP AT mt_queue INTO li_mapper.
      li_mapper->rename_node(
        EXPORTING
          is_node = ls_node
        CHANGING
          cv_name = cv_name ).
      ls_node-name = cv_name.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_to_snake IMPLEMENTATION.

  METHOD /apmg/if_apm_ajson_mapper~rename_node.

    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN cv_name WITH `$1_$2` ##REGEX_POSIX. "#EC NOTEXT
    cv_name = to_lower( cv_name ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_to_camel IMPLEMENTATION.

  METHOD constructor.
    mv_first_json_upper = iv_first_json_upper.
  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_mapper~rename_node.

    TYPES lty_token TYPE c LENGTH 255.
    CONSTANTS lc_forced_underscore_marker TYPE c LENGTH 1 VALUE cl_abap_char_utilities=>horizontal_tab.

    DATA lt_tokens TYPE STANDARD TABLE OF lty_token.
    DATA lv_from TYPE i.
    FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.

    IF mv_first_json_upper = abap_true.
      lv_from = 1.
    ELSE.
      lv_from = 2.
    ENDIF.
    REPLACE ALL OCCURRENCES OF `__` IN cv_name WITH lc_forced_underscore_marker. " Force underscore

    SPLIT cv_name AT `_` INTO TABLE lt_tokens.
    DELETE lt_tokens WHERE table_line IS INITIAL.
    LOOP AT lt_tokens ASSIGNING <token> FROM lv_from.
      TRANSLATE <token>+0(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF lt_tokens INTO cv_name.
    REPLACE ALL OCCURRENCES OF lc_forced_underscore_marker IN cv_name WITH `_`.

  ENDMETHOD.

ENDCLASS.
