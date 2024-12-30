* Same as zcl_ajson_filter_lib=>create_empty_filter( ) but also removing initial numbers and null
CLASS lcl_ajson_filters DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_abappm_ajson_filter.

    CLASS-METHODS create_empty_filter
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_ajson_filter
      RAISING
        zcx_abappm_ajson_error .

ENDCLASS.

CLASS lcl_ajson_filters IMPLEMENTATION.

  METHOD create_empty_filter.

    result = NEW lcl_ajson_filters( ).

  ENDMETHOD.

  METHOD zif_abappm_ajson_filter~keep_node.

    rv_keep = xsdbool(
      ( iv_visit = zif_abappm_ajson_filter=>visit_type-value AND
        ( is_node-type = zif_abappm_ajson_types=>node_type-string AND is_node-value IS NOT INITIAL OR
          is_node-type = zif_abappm_ajson_types=>node_type-boolean OR
          is_node-type = zif_abappm_ajson_types=>node_type-number AND is_node-value <> 0 ) ) OR
      ( iv_visit <> zif_abappm_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).

  ENDMETHOD.

ENDCLASS.
