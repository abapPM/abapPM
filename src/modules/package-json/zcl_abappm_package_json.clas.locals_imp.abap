* Same as zcl_ajson_filter_lib=>create_empty_filter( ) but also removing initial numbers and null
CLASS lcl_ajson_filters DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES ZIF_ABAPPM_AJSON_FILTER.

    CLASS-METHODS create_empty_filter
      RETURNING
        VALUE(ri_filter) TYPE REF TO ZIF_ABAPPM_AJSON_FILTER
      RAISING
        ZCX_ABAPPM_AJSON_ERROR .

ENDCLASS.

CLASS lcl_ajson_filters IMPLEMENTATION.

  METHOD create_empty_filter.
    CREATE OBJECT ri_filter TYPE lcl_ajson_filters.
  ENDMETHOD.

  METHOD ZIF_ABAPPM_AJSON_FILTER~KEEP_NODE.

    rv_keep = boolc(
      ( iv_visit = ZIF_ABAPPM_AJSON_FILTER=>VISIT_TYPE-VALUE AND
        ( is_node-type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-STRING AND is_node-value IS NOT INITIAL OR
          is_node-type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-BOOLEAN OR
          is_node-type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NUMBER AND is_node-value <> 0 ) ) OR
      ( iv_visit <> ZIF_ABAPPM_AJSON_FILTER=>VISIT_TYPE-VALUE AND is_node-children > 0 ) ).

  ENDMETHOD.

ENDCLASS.
