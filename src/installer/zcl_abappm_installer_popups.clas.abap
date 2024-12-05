CLASS zcl_abappm_installer_popups DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_alv_column,
        name   TYPE string,
        text   TYPE string,
        length TYPE lvc_outlen,
        key    TYPE abap_bool,
      END OF ty_alv_column.

    TYPES:
      ty_alv_column_tt TYPE STANDARD TABLE OF ty_alv_column WITH KEY name.

    TYPES:
      BEGIN OF ty_popup_position,
        start_column LIKE  sy-cucol,
        start_row    LIKE  sy-curow,
        end_column   LIKE  sy-cucol,
        end_row      LIKE  sy-curow,
      END OF ty_popup_position.

    CONSTANTS c_default_column TYPE lvc_fname VALUE `DEFAULT_COLUMN` ##NO_TEXT.

    METHODS popup_to_enter_packaging
      IMPORTING
        !iv_name            TYPE csequence OPTIONAL
        !iv_version         TYPE csequence OPTIONAL
      RETURNING
        VALUE(rs_packaging) TYPE zif_abappm_installer_dot=>ty_packaging
      RAISING
        zcx_abappm_error.

    METHODS popup_to_confirm
      IMPORTING
        !iv_title                 TYPE csequence
        !iv_question              TYPE csequence
        !iv_text_button_1         TYPE csequence DEFAULT 'Yes'
        !iv_icon_button_1         TYPE icon-name DEFAULT space
        !iv_text_button_2         TYPE csequence DEFAULT 'No'
        !iv_icon_button_2         TYPE icon-name DEFAULT space
        !iv_default_button        TYPE sy-input DEFAULT '1'
        !iv_display_cancel_button TYPE sy-input DEFAULT abap_true
      RETURNING
        VALUE(rv_answer)          TYPE sy-input
      RAISING
        zcx_abappm_error.

    METHODS popup_to_select_from_list
      IMPORTING
        !it_list               TYPE STANDARD TABLE
        !iv_title              TYPE lvc_title DEFAULT space
        !iv_header_text        TYPE csequence DEFAULT space
        !iv_start_column       TYPE i DEFAULT 2
        !iv_end_column         TYPE i DEFAULT 65
        !iv_start_line         TYPE i DEFAULT 8
        !iv_end_line           TYPE i DEFAULT 20
        !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
        !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
        !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
        !iv_select_column_text TYPE csequence DEFAULT space
        !it_columns_to_display TYPE ty_alv_column_tt
      EXPORTING
        VALUE(et_list)         TYPE STANDARD TABLE
      RAISING
        zcx_abappm_error.

    CLASS-METHODS center
      IMPORTING
        !iv_width          TYPE i
        !iv_height         TYPE i
      RETURNING
        VALUE(rs_position) TYPE ty_popup_position.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_fieldname_selected TYPE lvc_fname VALUE `SELECTED` ##NO_TEXT.

    DATA:
      mo_select_list_popup TYPE REF TO cl_salv_table,
      mr_table             TYPE REF TO data,
      mv_cancel            TYPE abap_bool VALUE abap_false,
      mo_table_descr       TYPE REF TO cl_abap_tabledescr.

    METHODS _create_new_table
      IMPORTING
        !it_list TYPE STANDARD TABLE.

    METHODS _get_selected_rows
      EXPORTING
        !et_list TYPE INDEX TABLE.

    METHODS _on_select_list_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.

    METHODS _on_select_list_function_click
      FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        !e_salv_function.

    METHODS _on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.

ENDCLASS.



CLASS zcl_abappm_installer_popups IMPLEMENTATION.


  METHOD center.

    CONSTANTS:
      lc_min_size TYPE i VALUE 10,
      lc_min_pos  TYPE i VALUE 5.

    " Magic math to approximate starting position of popup
    IF sy-scols > lc_min_size AND iv_width > 0 AND sy-scols > iv_width.
      rs_position-start_column = nmax(
        val1 = ( sy-scols - iv_width ) / 2
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_column = lc_min_pos.
    ENDIF.

    IF sy-srows > lc_min_size AND iv_height > 0 AND sy-srows > iv_height.
      rs_position-start_row = nmax(
        val1 = ( sy-srows - iv_height ) / 2 - 1
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_row = lc_min_pos.
    ENDIF.

    rs_position-end_column = rs_position-start_column + iv_width.
    rs_position-end_row = rs_position-start_row + iv_height.

  ENDMETHOD.


  METHOD popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_question
        text_button_1         = iv_text_button_1
        icon_button_1         = iv_icon_button_1
        text_button_2         = iv_text_button_2
        icon_button_2         = iv_icon_button_2
        default_button        = iv_default_button
        display_cancel_button = iv_display_cancel_button
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.


  METHOD popup_to_enter_packaging.

    TYPES:
      BEGIN OF ty_free_sel_field,
        name             TYPE fieldname,
        only_parameter   TYPE abap_bool,
        param_obligatory TYPE abap_bool,
        value            TYPE string,
        value_range      TYPE rsds_selopt_t,
        ddic_tabname     TYPE tabname,
        ddic_fieldname   TYPE fieldname,
        text             TYPE rsseltext,
      END OF ty_free_sel_field,
      ty_free_sel_field_tab TYPE STANDARD TABLE OF ty_free_sel_field WITH DEFAULT KEY.

    DATA:
      lt_fields TYPE ty_free_sel_field_tab,
      lo_dialog TYPE REF TO lcl_abapgit_free_sel_dialog,
      lx_error  TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS:
      <ls_field> TYPE ty_free_sel_field.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name             = 'NAME'.
    <ls_field>-text             = 'Name'.
    <ls_field>-only_parameter   = abap_true.
    <ls_field>-ddic_tabname     = 'E071'.
    <ls_field>-ddic_fieldname   = 'OBJ_NAME'.
    <ls_field>-param_obligatory = abap_true.
    <ls_field>-value            = iv_name.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-name             = 'VERSION'.
    <ls_field>-text             = 'Version'.
    <ls_field>-only_parameter   = abap_true.
    <ls_field>-ddic_tabname     = 'TTREV'.
    <ls_field>-ddic_fieldname   = 'VERSION'.
    <ls_field>-param_obligatory = abap_true.
    <ls_field>-value            = iv_version.

    TRY.
        CREATE OBJECT lo_dialog
          EXPORTING
            iv_title      = |apm|
            iv_frame_text = |Packaging Details|.

        lo_dialog->set_fields( CHANGING ct_fields = lt_fields ).
        lo_dialog->show( ).

        LOOP AT lt_fields ASSIGNING <ls_field>.
          CASE <ls_field>-name.
            WHEN 'NAME'.
              rs_packaging-name = <ls_field>-value.
            WHEN 'VERSION'.
              rs_packaging-version = <ls_field>-value.
              rs_packaging-sem_version = zcl_abapgit_version=>conv_str_to_version( rs_packaging-version ).
          ENDCASE.
        ENDLOOP.

      CATCH zcx_abapgit_cancel.
        RETURN.
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD popup_to_select_from_list.

    DATA:
      lv_pfstatus     TYPE sypfkey,
      lo_events       TYPE REF TO cl_salv_events_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lt_columns      TYPE salv_t_column_ref,
      ls_column       TYPE salv_s_column_ref,
      lo_column       TYPE REF TO cl_salv_column_list,
      lo_table_header TYPE REF TO cl_salv_form_text.

    FIELD-SYMBOLS:
      <lt_table>             TYPE STANDARD TABLE,
      <ls_column_to_display> TYPE ty_alv_column.

    CLEAR: et_list.

    _create_new_table( it_list ).

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = mo_select_list_popup
                                CHANGING  t_table = <lt_table> ).

        CASE iv_selection_mode.
          WHEN if_salv_c_selection_mode=>single.
            lv_pfstatus = '110'.

          WHEN OTHERS.
            lv_pfstatus = '102'.

        ENDCASE.

        mo_select_list_popup->set_screen_status( pfstatus = lv_pfstatus
                                                 report = 'SAPMSVIM' ).

        mo_select_list_popup->set_screen_popup( start_column = iv_start_column
                                                end_column   = iv_end_column
                                                start_line   = iv_start_line
                                                end_line     = iv_end_line ).

        lo_events = mo_select_list_popup->get_event( ).

        SET HANDLER _on_select_list_link_click FOR lo_events.
        SET HANDLER _on_select_list_function_click FOR lo_events.
        SET HANDLER _on_double_click FOR lo_events.

        IF iv_title CN ' _0'.
          mo_select_list_popup->get_display_settings( )->set_list_header( iv_title ).
        ENDIF.

        IF iv_header_text CN ' _0'.
          CREATE OBJECT lo_table_header
            EXPORTING
              text = iv_header_text.
          mo_select_list_popup->set_top_of_list( lo_table_header ).
        ENDIF.

        mo_select_list_popup->get_display_settings( )->set_striped_pattern( iv_striped_pattern ).
        mo_select_list_popup->get_selections( )->set_selection_mode( iv_selection_mode ).

        lo_columns = mo_select_list_popup->get_columns( ).
        lt_columns = lo_columns->get( ).
        lo_columns->set_optimize( iv_optimize_col_width ).

        LOOP AT lt_columns INTO ls_column.

          lo_column ?= ls_column-r_column.

          IF iv_selection_mode = if_salv_c_selection_mode=>multiple
            AND ls_column-columnname = c_fieldname_selected.
            lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
            lo_column->set_output_length( 20 ).
            lo_column->set_short_text( |{ iv_select_column_text }| ).
            lo_column->set_medium_text( |{ iv_select_column_text }| ).
            lo_column->set_long_text( |{ iv_select_column_text }| ).
            CONTINUE.
          ENDIF.

          READ TABLE it_columns_to_display
            ASSIGNING <ls_column_to_display>
            WITH KEY name = ls_column-columnname.

          CASE sy-subrc.
            WHEN 0.
              IF <ls_column_to_display>-text CN ' _0'.
                lo_column->set_short_text( |{ <ls_column_to_display>-text }| ).
                lo_column->set_medium_text( |{ <ls_column_to_display>-text }| ).
                lo_column->set_long_text( |{ <ls_column_to_display>-text }| ).
              ENDIF.

              IF <ls_column_to_display>-length > 0.
                lo_column->set_output_length( <ls_column_to_display>-length ).
              ENDIF.

              lo_column->set_key( <ls_column_to_display>-key ).

            WHEN OTHERS.
              " Hide column
              lo_column->set_technical( abap_true ).

          ENDCASE.

        ENDLOOP.

        mo_select_list_popup->display( ).

      CATCH cx_salv_msg.
        zcx_abappm_error=>raise( 'Error from POPUP_TO_SELECT_FROM_LIST' ).
    ENDTRY.

    IF mv_cancel = abap_true.
      mv_cancel = abap_false.
      RETURN.
    ENDIF.

    _get_selected_rows( IMPORTING et_list = et_list ).

    CLEAR: mo_select_list_popup,
           mr_table,
           mo_table_descr.

  ENDMETHOD.


  METHOD _create_new_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA:
      lr_struct        TYPE REF TO data,
      lt_components    TYPE cl_abap_structdescr=>component_table,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_elem_descr    TYPE REF TO cl_abap_elemdescr,
      lo_struct_descr  TYPE REF TO cl_abap_structdescr,
      lo_struct_descr2 TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <lt_table>     TYPE STANDARD TABLE,
      <ls_component> TYPE abap_componentdescr,
      <lg_line>      TYPE data,
      <lg_data>      TYPE any,
      <lg_value>     TYPE any.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_list ).
    lo_data_descr = mo_table_descr->get_table_line_type( ).

    CASE lo_data_descr->kind.
      WHEN cl_abap_elemdescr=>kind_elem.
        lo_elem_descr ?= mo_table_descr->get_table_line_type( ).
        INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
        <ls_component>-name = c_default_column.
        <ls_component>-type = lo_elem_descr.

      WHEN cl_abap_elemdescr=>kind_struct.
        lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
        lt_components = lo_struct_descr->get_components( ).

    ENDCASE.

    IF lt_components IS INITIAL.
      RETURN.
    ENDIF.

    INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
    <ls_component>-name = c_fieldname_selected.
    <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    lo_struct_descr2 = cl_abap_structdescr=>create( lt_components ).
    mo_table_descr = cl_abap_tabledescr=>create( lo_struct_descr2 ).

    CREATE DATA mr_table TYPE HANDLE mo_table_descr.
    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_struct TYPE HANDLE lo_struct_descr2.
    ASSIGN lr_struct->* TO <lg_line>.
    ASSERT sy-subrc = 0.

    LOOP AT it_list ASSIGNING <lg_data>.
      CLEAR <lg_line>.
      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <lg_data> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <lg_line> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <lg_data> TO <lg_line>.

      ENDCASE.
      INSERT <lg_line> INTO TABLE <lt_table>.
    ENDLOOP.

  ENDMETHOD.


  METHOD _get_selected_rows.

    DATA:
      lv_condition TYPE string,
      lr_exporting TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_exporting>    TYPE any,
      <lt_table>        TYPE STANDARD TABLE,
      <lg_line>         TYPE any,
      <lg_value>        TYPE any,
      <lv_selected>     TYPE abap_bool,
      <lv_selected_row> TYPE LINE OF salv_t_row.

    DATA: lo_data_descr    TYPE REF TO cl_abap_datadescr,
          lo_selections    TYPE REF TO cl_salv_selections,
          lt_selected_rows TYPE salv_t_row.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    lo_selections = mo_select_list_popup->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.

      lt_selected_rows = lo_selections->get_selected_rows( ).

      LOOP AT lt_selected_rows ASSIGNING <lv_selected_row>.

        READ TABLE <lt_table>
          ASSIGNING <lg_line>
          INDEX <lv_selected_row>.
        CHECK <lg_line> IS ASSIGNED.

        ASSIGN COMPONENT c_fieldname_selected
           OF STRUCTURE <lg_line>
           TO <lv_selected>.
        CHECK <lv_selected> IS ASSIGNED.

        <lv_selected> = abap_true.

      ENDLOOP.

    ENDIF.

    lv_condition = |{ c_fieldname_selected } = ABAP_TRUE|.

    CREATE DATA lr_exporting LIKE LINE OF et_list.
    ASSIGN lr_exporting->* TO <lg_exporting>.
    ASSERT sy-subrc = 0.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( et_list ).
    lo_data_descr = mo_table_descr->get_table_line_type( ).

    LOOP AT <lt_table> ASSIGNING <lg_line> WHERE (lv_condition).
      CLEAR <lg_exporting>.

      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <lg_line> TO <lg_value>.
          ASSERT sy-subrc = 0.
          <lg_exporting> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <lg_line> TO <lg_exporting>.

      ENDCASE.
      APPEND <lg_exporting> TO et_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD _on_double_click.

    DATA lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = mo_select_list_popup->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.
      mo_select_list_popup->close_screen( ).
    ENDIF.

  ENDMETHOD.


  METHOD _on_select_list_function_click.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lg_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CASE e_salv_function.
      WHEN 'O.K.'.
        mv_cancel = abap_false.
        mo_select_list_popup->close_screen( ).

      WHEN 'ABR'.
        "Canceled: clear list to overwrite nothing
        CLEAR <lt_table>.
        mv_cancel = abap_true.
        mo_select_list_popup->close_screen( ).

      WHEN 'SALL'.
        LOOP AT <lt_table> ASSIGNING <lg_line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <lg_line>
                 TO <lv_selected>.
          ASSERT sy-subrc = 0.

          <lv_selected> = abap_true.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN 'DSEL'.
        LOOP AT <lt_table> ASSIGNING <lg_line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <lg_line>
                 TO <lv_selected>.
          ASSERT sy-subrc = 0.

          <lv_selected> = abap_false.

        ENDLOOP.

        mo_select_list_popup->refresh( ).

      WHEN OTHERS.
        CLEAR <lt_table>.
        mo_select_list_popup->close_screen( ).
    ENDCASE.

  ENDMETHOD.


  METHOD _on_select_list_link_click.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lg_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    READ TABLE <lt_table> ASSIGNING <lg_line> INDEX row.
    IF sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected
             OF STRUCTURE <lg_line>
             TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF <lv_selected> = abap_true.
        <lv_selected> = abap_false.
      ELSE.
        <lv_selected> = abap_true.
      ENDIF.

    ENDIF.

    mo_select_list_popup->refresh( ).

  ENDMETHOD.
ENDCLASS.
