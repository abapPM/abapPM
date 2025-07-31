"
" MAPPINGS
"
CLASS lcl_to_camel_case_underscore DEFINITION.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapping.
ENDCLASS.

CLASS lcl_to_camel_case_underscore IMPLEMENTATION.

  METHOD /apmg/if_apm_ajson_mapping~rename_node.

    TYPES ty_token TYPE c LENGTH 255.

    DATA from TYPE i.
    DATA tokens TYPE STANDARD TABLE OF ty_token WITH KEY table_line.

    FIELD-SYMBOLS <token> LIKE LINE OF tokens.

    from = 2.
    IF cv_name(1) = '_'.
      from = 3.
    ENDIF.

    SPLIT cv_name AT '_' INTO TABLE tokens.

    LOOP AT tokens ASSIGNING <token> FROM from.
      TRANSLATE <token>+0(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO cv_name.

    IF from = 3.
      cv_name = '_' && cv_name.
    ENDIF.

    TRANSLATE cv_name USING '/_:_~_*_'.

  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_mapping~to_abap. " deprecated
    ASSERT 0 = 0.
  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_mapping~to_json. " deprecated
    ASSERT 0 = 0.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_from_camel_case_underscore DEFINITION.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapping.
ENDCLASS.

CLASS lcl_from_camel_case_underscore IMPLEMENTATION.

  METHOD /apmg/if_apm_ajson_mapping~rename_node.
    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN cv_name WITH `$1_$2`.
  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_mapping~to_abap. " deprecated
    ASSERT 0 = 0.
  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_mapping~to_json. " deprecated
    ASSERT 0 = 0.
  ENDMETHOD.

ENDCLASS.

"
" FILTERS
"
CLASS lcl_empty_zero_null DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_filter.
ENDCLASS.

CLASS lcl_empty_zero_null IMPLEMENTATION.

  METHOD /apmg/if_apm_ajson_filter~keep_node.

    rv_keep = boolc(
      ( iv_visit = /apmg/if_apm_ajson_filter=>visit_type-value AND
        ( is_node-type = /apmg/if_apm_ajson_types=>node_type-string AND is_node-value IS NOT INITIAL OR
          is_node-type = /apmg/if_apm_ajson_types=>node_type-boolean OR
          is_node-type = /apmg/if_apm_ajson_types=>node_type-number AND is_node-value <> 0 ) ) OR
      ( iv_visit <> /apmg/if_apm_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).

  ENDMETHOD.

ENDCLASS.
