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

    CONSTANTS c_defaucolumn TYPE lvc_fname VALUE `DEFAUCOLUMN` ##NO_TEXT.

    METHODS popup_to_enter_packaging
      IMPORTING
        !name         TYPE csequence OPTIONAL
        !version      TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE zif_abappm_installer_dot=>ty_packaging
      RAISING
        zcx_abappm_error.

    METHODS popup_to_confirm
      IMPORTING
        !title                 TYPE csequence
        !question              TYPE csequence
        !text_button_1         TYPE csequence DEFAULT 'Yes'
        !icon_button_1         TYPE icon-name DEFAULT space
        !text_button_2         TYPE csequence DEFAULT 'No'
        !icon_button_2         TYPE icon-name DEFAULT space
        !default_button        TYPE sy-input DEFAULT '1'
        !display_cancel_button TYPE sy-input DEFAULT abap_true
      RETURNING
        VALUE(result)          TYPE sy-input
      RAISING
        zcx_abappm_error.

    METHODS popup_to_select_from_list
      IMPORTING
        !import_list        TYPE STANDARD TABLE
        !title              TYPE lvc_title DEFAULT space
        !header_text        TYPE csequence DEFAULT space
        !start_column       TYPE i DEFAULT 2
        !end_column         TYPE i DEFAULT 65
        !start_line         TYPE i DEFAULT 8
        !end_line           TYPE i DEFAULT 20
        !striped_pattern    TYPE abap_bool DEFAULT abap_false
        !optimize_col_width TYPE abap_bool DEFAULT abap_true
        !selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
        !select_column_text TYPE csequence DEFAULT space
        !columns_to_display TYPE ty_alv_column_tt
      EXPORTING
        VALUE(export_list)  TYPE STANDARD TABLE
      RAISING
        zcx_abappm_error.

    CLASS-METHODS center
      IMPORTING
        !width        TYPE i
        !height       TYPE i
      RETURNING
        VALUE(result) TYPE ty_popup_position.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_fieldname_selected TYPE lvc_fname VALUE `SELECTED` ##NO_TEXT.

    DATA:
      select_list_popup TYPE REF TO cl_salv_table,
      data_table        TYPE REF TO data,
      is_cancelled      TYPE abap_bool VALUE abap_false,
      table_descr       TYPE REF TO cl_abap_tabledescr.

    METHODS _create_new_table
      IMPORTING
        !import_list TYPE STANDARD TABLE.

    METHODS _get_selected_rows
      EXPORTING
        !export_list TYPE INDEX TABLE.

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
      c_min_size TYPE i VALUE 10,
      c_min_pos  TYPE i VALUE 5.

    " Magic math to approximate starting position of popup
    IF sy-scols > c_min_size AND width > 0 AND sy-scols > width.
      result-start_column = nmax(
        val1 = ( sy-scols - width ) / 2
        val2 = c_min_pos ).
    ELSE.
      result-start_column = c_min_pos.
    ENDIF.

    IF sy-srows > c_min_size AND height > 0 AND sy-srows > height.
      result-start_row = nmax(
        val1 = ( sy-srows - height ) / 2 - 1
        val2 = c_min_pos ).
    ELSE.
      result-start_row = c_min_pos.
    ENDIF.

    result-end_column = result-start_column + width.
    result-end_row = result-start_row + height.

  ENDMETHOD.


  METHOD popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = title
        text_question         = question
        text_button_1         = text_button_1
        icon_button_1         = icon_button_1
        text_button_2         = text_button_2
        icon_button_2         = icon_button_2
        default_button        = default_button
        display_cancel_button = display_cancel_button
      IMPORTING
        answer                = result
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

    DATA fields TYPE ty_free_sel_field_tab.

    APPEND INITIAL LINE TO fields ASSIGNING FIELD-SYMBOL(<field>).
    <field>-name             = 'NAME'.
    <field>-text             = 'Name'.
    <field>-only_parameter   = abap_true.
    <field>-ddic_tabname     = 'E071'.
    <field>-ddic_fieldname   = 'OBJ_NAME'.
    <field>-param_obligatory = abap_true.
    <field>-value            = name.

    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-name             = 'VERSION'.
    <field>-text             = 'Version'.
    <field>-only_parameter   = abap_true.
    <field>-ddic_tabname     = 'TTREV'.
    <field>-ddic_fieldname   = 'VERSION'.
    <field>-param_obligatory = abap_true.
    <field>-value            = version.

    TRY.
        DATA(dialog) = NEW lcl_abapgit_free_sel_dialog(
          title      = |apm|
          frame_text = |Packaging Details| ).

        dialog->set_fields( CHANGING field_table = fields ).
        dialog->show( ).

        LOOP AT fields ASSIGNING <field>.
          CASE <field>-name.
            WHEN 'NAME'.
              result-name = <field>-value.
            WHEN 'VERSION'.
              result-version     = <field>-value.
              result-sem_version = zcl_abapgit_version=>conv_str_to_version( result-version ).
          ENDCASE.
        ENDLOOP.

      CATCH zcx_abapgit_cancel.
        RETURN.
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD popup_to_select_from_list.

    DATA:
      pfstatus      TYPE sypfkey,
      events        TYPE REF TO cl_salv_events_table,
      columns_table TYPE REF TO cl_salv_columns_table,
      columns       TYPE salv_t_column_ref,
      column        TYPE salv_s_column_ref,
      column_list   TYPE REF TO cl_salv_column_list,
      table_header  TYPE REF TO cl_salv_form_text.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    CLEAR export_list.

    _create_new_table( import_list ).

    ASSIGN data_table->* TO <table>.
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = select_list_popup
                                CHANGING  t_table = <table> ).

        CASE selection_mode.
          WHEN if_salv_c_selection_mode=>single.
            pfstatus = '110'.

          WHEN OTHERS.
            pfstatus = '102'.

        ENDCASE.

        select_list_popup->set_screen_status( pfstatus = pfstatus
                                                 report = 'SAPMSVIM' ).

        select_list_popup->set_screen_popup( start_column = start_column
                                                end_column   = end_column
                                                start_line   = start_line
                                                end_line     = end_line ).

        events = select_list_popup->get_event( ).

        SET HANDLER _on_select_list_link_click FOR events.
        SET HANDLER _on_select_list_function_click FOR events.
        SET HANDLER _on_double_click FOR events.

        IF title CN ' _0'.
          select_list_popup->get_display_settings( )->set_list_header( title ).
        ENDIF.

        IF header_text CN ' _0'.
          CREATE OBJECT table_header
            EXPORTING
              text = header_text.
          select_list_popup->set_top_of_list( table_header ).
        ENDIF.

        select_list_popup->get_display_settings( )->set_striped_pattern( striped_pattern ).
        select_list_popup->get_selections( )->set_selection_mode( selection_mode ).

        columns_table = select_list_popup->get_columns( ).
        columns = columns_table->get( ).
        columns_table->set_optimize( optimize_col_width ).

        LOOP AT columns INTO column.

          column_list ?= column-r_column.

          IF selection_mode = if_salv_c_selection_mode=>multiple
            AND column-columnname = c_fieldname_selected.
            column_list->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
            column_list->set_output_length( 20 ).
            column_list->set_short_text( |{ select_column_text }| ).
            column_list->set_medium_text( |{ select_column_text }| ).
            column_list->set_long_text( |{ select_column_text }| ).
            CONTINUE.
          ENDIF.

          READ TABLE columns_to_display
            ASSIGNING FIELD-SYMBOL(<column_to_display>)
            WITH KEY name = column-columnname.

          CASE sy-subrc.
            WHEN 0.
              IF <column_to_display>-text CN ' _0'.
                column_list->set_short_text( |{ <column_to_display>-text }| ).
                column_list->set_medium_text( |{ <column_to_display>-text }| ).
                column_list->set_long_text( |{ <column_to_display>-text }| ).
              ENDIF.

              IF <column_to_display>-length > 0.
                column_list->set_output_length( <column_to_display>-length ).
              ENDIF.

              column_list->set_key( <column_to_display>-key ).

            WHEN OTHERS.
              " Hide column
              column_list->set_technical( abap_true ).

          ENDCASE.

        ENDLOOP.

        select_list_popup->display( ).

      CATCH cx_salv_msg.
        zcx_abappm_error=>raise( 'Error from POPUP_TO_SELECT_FROM_LIST' ).
    ENDTRY.

    IF is_cancelled = abap_true.
      is_cancelled = abap_false.
      RETURN.
    ENDIF.

    _get_selected_rows( IMPORTING export_list = export_list ).

    CLEAR: select_list_popup,
           data_table,
           table_descr.

  ENDMETHOD.


  METHOD _create_new_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA:
      structure     TYPE REF TO data,
      components    TYPE cl_abap_structdescr=>component_table,
      data_descr    TYPE REF TO cl_abap_datadescr,
      elem_descr    TYPE REF TO cl_abap_elemdescr,
      struct_descr  TYPE REF TO cl_abap_structdescr,
      struct_descr2 TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <table>     TYPE STANDARD TABLE,
      <component> TYPE abap_componentdescr,
      <line>      TYPE data,
      <data>      TYPE any,
      <value>     TYPE any.

    table_descr ?= cl_abap_tabledescr=>describe_by_data( import_list ).
    data_descr = table_descr->get_table_line_type( ).

    CASE data_descr->kind.
      WHEN cl_abap_elemdescr=>kind_elem.
        elem_descr ?= table_descr->get_table_line_type( ).
        INSERT INITIAL LINE INTO components ASSIGNING <component> INDEX 1.
        <component>-name = c_defaucolumn.
        <component>-type = elem_descr.

      WHEN cl_abap_elemdescr=>kind_struct.
        struct_descr ?= table_descr->get_table_line_type( ).
        components = struct_descr->get_components( ).

    ENDCASE.

    IF components IS INITIAL.
      RETURN.
    ENDIF.

    INSERT INITIAL LINE INTO components ASSIGNING <component> INDEX 1.
    <component>-name = c_fieldname_selected.
    <component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    struct_descr2 = cl_abap_structdescr=>create( components ).
    table_descr = cl_abap_tabledescr=>create( struct_descr2 ).

    CREATE DATA data_table TYPE HANDLE table_descr.
    ASSIGN data_table->* TO <table>.
    ASSERT sy-subrc = 0.

    CREATE DATA structure TYPE HANDLE struct_descr2.
    ASSIGN structure->* TO <line>.
    ASSERT sy-subrc = 0.

    LOOP AT import_list ASSIGNING <data>.
      CLEAR <line>.
      CASE data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_defaucolumn OF STRUCTURE <data> TO <value>.
          ASSERT <value> IS ASSIGNED.
          <line> = <value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <data> TO <line>.

      ENDCASE.
      INSERT <line> INTO TABLE <table>.
    ENDLOOP.

  ENDMETHOD.


  METHOD _get_selected_rows.

    DATA:
      condition TYPE string,
      exporting TYPE REF TO data.

    FIELD-SYMBOLS:
      <exporting>    TYPE any,
      <table>        TYPE STANDARD TABLE,
      <line>         TYPE any,
      <value>        TYPE any,
      <selected>     TYPE abap_bool,
      <selected_row> TYPE LINE OF salv_t_row.

    DATA: data_descr    TYPE REF TO cl_abap_datadescr,
          selections    TYPE REF TO cl_salv_selections,
          selected_rows TYPE salv_t_row.

    ASSIGN data_table->* TO <table>.
    ASSERT sy-subrc = 0.

    selections = select_list_popup->get_selections( ).

    IF selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.

      selected_rows = selections->get_selected_rows( ).

      LOOP AT selected_rows ASSIGNING <selected_row>.

        READ TABLE <table>
          ASSIGNING <line>
          INDEX <selected_row>.
        CHECK <line> IS ASSIGNED.

        ASSIGN COMPONENT c_fieldname_selected
           OF STRUCTURE <line>
           TO <selected>.
        CHECK <selected> IS ASSIGNED.

        <selected> = abap_true.

      ENDLOOP.

    ENDIF.

    condition = |{ c_fieldname_selected } = ABAP_TRUE|.

    CREATE DATA exporting LIKE LINE OF export_list.
    ASSIGN exporting->* TO <exporting>.
    ASSERT sy-subrc = 0.

    table_descr ?= cl_abap_tabledescr=>describe_by_data( export_list ).
    data_descr = table_descr->get_table_line_type( ).

    LOOP AT <table> ASSIGNING <line> WHERE (condition).
      CLEAR <exporting>.

      CASE data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_defaucolumn OF STRUCTURE <line> TO <value>.
          ASSERT sy-subrc = 0.
          <exporting> = <value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <line> TO <exporting>.

      ENDCASE.
      APPEND <exporting> TO export_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD _on_double_click.

    DATA(selections) = select_list_popup->get_selections( ).

    IF selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.
      select_list_popup->close_screen( ).
    ENDIF.

  ENDMETHOD.


  METHOD _on_select_list_function_click.

    FIELD-SYMBOLS:
      <table>    TYPE STANDARD TABLE,
      <line>     TYPE any,
      <selected> TYPE abap_bool.

    ASSIGN data_table->* TO <table>.
    ASSERT sy-subrc = 0.

    CASE e_salv_function.
      WHEN 'O.K.'.
        is_cancelled = abap_false.
        select_list_popup->close_screen( ).

      WHEN 'ABR'.
        "Canceled: clear list to overwrite nothing
        CLEAR <table>.
        is_cancelled = abap_true.
        select_list_popup->close_screen( ).

      WHEN 'SALL'.
        LOOP AT <table> ASSIGNING <line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <line>
                 TO <selected>.
          ASSERT sy-subrc = 0.

          <selected> = abap_true.

        ENDLOOP.

        select_list_popup->refresh( ).

      WHEN 'DSEL'.
        LOOP AT <table> ASSIGNING <line>.

          ASSIGN COMPONENT c_fieldname_selected
                 OF STRUCTURE <line>
                 TO <selected>.
          ASSERT sy-subrc = 0.

          <selected> = abap_false.

        ENDLOOP.

        select_list_popup->refresh( ).

      WHEN OTHERS.
        CLEAR <table>.
        select_list_popup->close_screen( ).
    ENDCASE.

  ENDMETHOD.


  METHOD _on_select_list_link_click.

    FIELD-SYMBOLS:
      <table>    TYPE STANDARD TABLE,
      <line>     TYPE any,
      <selected> TYPE abap_bool.

    ASSIGN data_table->* TO <table>.
    ASSERT sy-subrc = 0.

    READ TABLE <table> ASSIGNING <line> INDEX row.
    IF sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected
             OF STRUCTURE <line>
             TO <selected>.
      ASSERT sy-subrc = 0.

      IF <selected> = abap_true.
        <selected> = abap_false.
      ELSE.
        <selected> = abap_true.
      ENDIF.

    ENDIF.

    select_list_popup->refresh( ).

  ENDMETHOD.
ENDCLASS.
