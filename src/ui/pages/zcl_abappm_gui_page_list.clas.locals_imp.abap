CLASS lcl_table_scheme DEFINITION FINAL.
  " TODO: move to a global class, when table is separated as a component
  PUBLIC SECTION.

    DATA columns TYPE zif_abapgit_definitions=>ty_col_spec_tt READ-ONLY.

    METHODS add_column
      IMPORTING
        tech_name      TYPE string OPTIONAL
        display_name   TYPE string OPTIONAL
        css_class      TYPE string OPTIONAL
        add_tz         TYPE abap_bool OPTIONAL
        title          TYPE string OPTIONAL
        allow_order_by TYPE any OPTIONAL
      RETURNING
        VALUE(result)  TYPE REF TO lcl_table_scheme.

ENDCLASS.

CLASS lcl_table_scheme IMPLEMENTATION.

  METHOD add_column.

    APPEND INITIAL LINE TO columns ASSIGNING FIELD-SYMBOL(<column>).
    <column>-display_name   = display_name.
    <column>-tech_name      = tech_name.
    <column>-title          = title.
    <column>-css_class      = css_class.
    <column>-add_tz         = add_tz.
    <column>-allow_order_by = allow_order_by.

    result = me.

  ENDMETHOD.

ENDCLASS.
