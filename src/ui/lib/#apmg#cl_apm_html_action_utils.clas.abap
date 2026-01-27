CLASS /apmg/cl_apm_html_action_utils DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS jump_encode
      IMPORTING
        !iv_obj_type     TYPE tadir-object
        !iv_obj_name     TYPE tadir-obj_name
        !iv_filename     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_name_value,
        name  TYPE string,
        value TYPE string,
      END OF ty_name_value,
      ty_name_value_tt TYPE STANDARD TABLE OF ty_name_value WITH DEFAULT KEY.

    CLASS-METHODS add_field
      IMPORTING
        !iv_name  TYPE string
        !ig_field TYPE any
      CHANGING
        !ct_field TYPE ty_name_value_tt.

    CLASS-METHODS fields_to_string
      IMPORTING
        !it_fields       TYPE ty_name_value_tt
      RETURNING
        VALUE(rv_string) TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_html_action_utils IMPLEMENTATION.


  METHOD add_field.

    DATA ls_field LIKE LINE OF ct_field.

    FIELD-SYMBOLS <lg_src> TYPE any.

    ls_field-name = iv_name.

    CASE cl_abap_typedescr=>describe_by_data( ig_field )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        ls_field-value = ig_field.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT iv_name OF STRUCTURE ig_field TO <lg_src>.
        ASSERT <lg_src> IS ASSIGNED.
        ls_field-value = <lg_src>.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    APPEND ls_field TO ct_field.

  ENDMETHOD.


  METHOD fields_to_string.

* There is no equivalent to cl_http_utility=>fields_to_string released in ABAP Cloud,
* see cl_web_http_utility

    DATA lt_tab   TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_str   TYPE string.
    DATA ls_field LIKE LINE OF it_fields.

    LOOP AT it_fields INTO ls_field.
      ls_field-value = escape( val = ls_field-value format = cl_abap_format=>e_url ).
      lv_str = ls_field-name && '=' && ls_field-value.
      APPEND lv_str TO lt_tab.
    ENDLOOP.
    rv_string = concat_lines_of(
      table = lt_tab
      sep   = '&' ).

  ENDMETHOD.


  METHOD jump_encode.

    DATA lt_fields TYPE ty_name_value_tt.


    add_field( EXPORTING iv_name = 'TYPE'
                         ig_field = iv_obj_type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'NAME'
                         ig_field = iv_obj_name CHANGING ct_field = lt_fields ).

    IF iv_filename IS NOT INITIAL.
      add_field( EXPORTING iv_name = 'FILE'
                           ig_field = iv_filename CHANGING ct_field = lt_fields ).
    ENDIF.

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.
ENDCLASS.
