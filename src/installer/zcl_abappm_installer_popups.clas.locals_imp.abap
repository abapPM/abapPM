"! Free Selections Dialog
CLASS lcl_abapgit_free_sel_dialog DEFINITION.

  PUBLIC SECTION.
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
      ty_free_sel_field_tab TYPE STANDARD TABLE OF ty_free_sel_field WITH DEFAULT KEY,
      ty_syst_title         TYPE c LENGTH 70.

    METHODS:
      constructor
        IMPORTING
          title      TYPE ty_syst_title OPTIONAL
          frame_text TYPE ty_syst_title OPTIONAL,
      set_fields
        CHANGING
          field_table TYPE ty_free_sel_field_tab,
      show
        RAISING
          zcx_abapgit_cancel
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_field_text_tab TYPE STANDARD TABLE OF rsdstexts WITH DEFAULT KEY.

    METHODS:
      convert_input_field_table
        EXPORTING
          default_values TYPE rsds_trange
          restriction    TYPE sscr_restrict_ds
          field_table    TYPE rsdsfields_t
          field_texts    TYPE ty_field_text_tab,
      free_selections_init
        IMPORTING
          default_values TYPE rsds_trange
          restriction    TYPE sscr_restrict_ds
        EXPORTING
          selection_id   TYPE dynselid
        CHANGING
          field_table    TYPE rsdsfields_t
          field_texts    TYPE ty_field_text_tab
        RAISING
          zcx_abapgit_exception,
      free_selections_dialog
        IMPORTING
          selection_id  TYPE dynselid
        EXPORTING
          result_ranges TYPE rsds_trange
        CHANGING
          field_table   TYPE rsdsfields_t
        RAISING
          zcx_abapgit_cancel
          zcx_abapgit_exception,
      validate_results
        IMPORTING
          result_ranges TYPE rsds_trange
        RAISING
          zcx_abapgit_exception,
      transfer_results_to_input
        IMPORTING
          result_ranges TYPE rsds_trange.

    DATA:
      field_table_ref TYPE REF TO ty_free_sel_field_tab,
      title           TYPE ty_syst_title,
      frame_text      TYPE ty_syst_title.

ENDCLASS.



CLASS lcl_abapgit_free_sel_dialog IMPLEMENTATION.


  METHOD constructor.
    me->title      = title.
    me->frame_text = frame_text.
  ENDMETHOD.


  METHOD convert_input_field_table.

    CONSTANTS c_only_eq_optlist_name TYPE c LENGTH 10 VALUE 'ONLYEQ'.

    DATA parameter_opt_list TYPE sscr_opt_list.

    LOOP AT field_table_ref->* ASSIGNING FIELD-SYMBOL(<input_field>).
      APPEND INITIAL LINE TO  field_table ASSIGNING FIELD-SYMBOL(<free_sel_field>).
      <free_sel_field>-fieldname = <input_field>-ddic_fieldname.
      <free_sel_field>-tablename = <input_field>-ddic_tabname.

      IF <input_field>-only_parameter = abap_true.
        IF restriction IS INITIAL.
          parameter_opt_list-name       = c_only_eq_optlist_name.
          parameter_opt_list-options-eq = abap_true.
          APPEND parameter_opt_list TO restriction-opt_list_tab.
        ENDIF.

        APPEND INITIAL LINE TO restriction-ass_tab ASSIGNING FIELD-SYMBOL(<restriction_ass>).
        <restriction_ass>-kind      = 'S'.
        <restriction_ass>-fieldname = <input_field>-ddic_fieldname.
        <restriction_ass>-tablename = <input_field>-ddic_tabname.
        <restriction_ass>-sg_main   = 'I'.
        <restriction_ass>-sg_addy   = 'N'.
        <restriction_ass>-op_main   = c_only_eq_optlist_name.
      ENDIF.

      IF <input_field>-text IS NOT INITIAL.
        APPEND INITIAL LINE TO  field_texts ASSIGNING FIELD-SYMBOL(<text>).
        <text>-fieldname = <input_field>-ddic_fieldname.
        <text>-tablename = <input_field>-ddic_tabname.
        <text>-text      = <input_field>-text.
      ENDIF.

      IF <input_field>-value IS NOT INITIAL OR <input_field>-value_range IS NOT INITIAL.
        READ TABLE default_values
          WITH KEY tablename = <input_field>-ddic_tabname
          ASSIGNING FIELD-SYMBOL(<default_value>).
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO default_values ASSIGNING <default_value>.
          <default_value>-tablename = <input_field>-ddic_tabname.
        ENDIF.

        APPEND INITIAL LINE TO <default_value>-frange_t ASSIGNING FIELD-SYMBOL(<default_value_range>).
        <default_value_range>-fieldname = <input_field>-ddic_fieldname.

        IF <input_field>-value IS NOT INITIAL.
          APPEND INITIAL LINE TO <default_value_range>-selopt_t ASSIGNING FIELD-SYMBOL(<default_val_range_line>).
          <default_val_range_line>-sign   = 'I'.
          <default_val_range_line>-option = 'EQ'.
          <default_val_range_line>-low    = <input_field>-value.
        ELSEIF <input_field>-value_range IS NOT INITIAL.
          <default_value_range>-selopt_t = <input_field>-value_range.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD free_selections_dialog.

    DATA(position) = zcl_abapgit_popups=>center(
      iv_width  = 60
      iv_height = lines(  field_table ) + 15 ).

    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = selection_id
        title           = title
        frame_text      = frame_text
        status          = 1
        start_col       = position-start_column
        start_row       = position-start_row
        as_window       = abap_true
        no_intervals    = abap_true
        tree_visible    = abap_false
      IMPORTING
        field_ranges    = result_ranges
      TABLES
        field_table_tab = field_table
      EXCEPTIONS
        internal_error  = 1
        no_action       = 2
        selid_not_found = 3
        illegal_status  = 4
        OTHERS          = 5.

    CASE sy-subrc.
      WHEN 0 ##NEEDED.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_abapgit_cancel.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Error from FREE_SELECTIONS_DIALOG: { sy-subrc }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD free_selections_init.

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                       = 'F'
        field_ranges_int           = default_values
        restriction                = restriction
      IMPORTING
        selection_id               = selection_id
      TABLES
        field_table_tab            = field_table
        field_texts                = field_texts
      EXCEPTIONS
        field_table_incomplete     = 1
        field_table_no_join        = 2
        field_not_found            = 3
        no_tables                  = 4
        table_not_found            = 5
        expression_not_supported   = 6
        incorrect_expression       = 7
        illegal_kind               = 8
        area_not_found             = 9
        inconsistent_area          = 10
        kind_f_no_field_table_left = 11
        kind_f_no_field_table      = 12
        too_many_field_table       = 13
        dup_field                  = 14
        field_no_type              = 15
        field_ill_type             = 16
        dup_event_field            = 17
        node_not_in_ldb            = 18
        area_no_field              = 19
        OTHERS                     = 20.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from FREE_SELECTIONS_INIT: { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD set_fields.
    GET REFERENCE OF field_table INTO field_table_ref.
  ENDMETHOD.


  METHOD show.

    DATA:
      default_values TYPE rsds_trange,
      restriction    TYPE sscr_restrict_ds,
      field_table    TYPE rsdsfields_t,
      field_texts    TYPE ty_field_text_tab,
      repeat_dialog  TYPE abap_bool VALUE abap_true,
      selection_id   TYPE dynselid,
      results        TYPE rsds_trange.

    convert_input_field_table(
      IMPORTING
        default_values = default_values
        restriction    = restriction
        field_table    = field_table
        field_texts    = field_texts ).

    WHILE repeat_dialog = abap_true.
      repeat_dialog = abap_false.

      free_selections_init(
        EXPORTING
           default_values = default_values
           restriction    = restriction
        IMPORTING
           selection_id   = selection_id
        CHANGING
           field_table    = field_table
           field_texts    = field_texts ).

      free_selections_dialog(
        EXPORTING
          selection_id  = selection_id
        IMPORTING
          result_ranges = results
        CHANGING
          field_table   = field_table ).

      TRY.
          validate_results( results ).
        CATCH zcx_abapgit_exception INTO DATA(error).
          repeat_dialog = abap_true.
          default_values =  results.
          MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
          CONTINUE.
      ENDTRY.

      transfer_results_to_input(  results ).
    ENDWHILE.

  ENDMETHOD.


  METHOD transfer_results_to_input.

    LOOP AT field_table_ref->* ASSIGNING FIELD-SYMBOL(<input_field>).
      READ TABLE  result_ranges
        WITH KEY tablename = <input_field>-ddic_tabname
        ASSIGNING FIELD-SYMBOL(<result_range_for_tab>).
      IF sy-subrc = 0.
        READ TABLE <result_range_for_tab>-frange_t
          WITH KEY fieldname = <input_field>-ddic_fieldname
          ASSIGNING FIELD-SYMBOL(<result_range_line>).
        IF sy-subrc = 0 AND <result_range_line>-selopt_t IS NOT INITIAL.
          IF <input_field>-only_parameter = abap_true.
            ASSERT lines( <result_range_line>-selopt_t ) = 1.

            READ TABLE <result_range_line>-selopt_t INDEX 1 ASSIGNING FIELD-SYMBOL(<selopt_line>).
            ASSERT sy-subrc = 0.

            ASSERT <selopt_line>-sign = 'I'
               AND <selopt_line>-option = 'EQ'
               AND <selopt_line>-high IS INITIAL.

            <input_field>-value = <selopt_line>-low.
          ELSE.
            <input_field>-value_range = <result_range_line>-selopt_t.
          ENDIF.
        ELSE.
          CLEAR: <input_field>-value, <input_field>-value_range.
        ENDIF.
      ELSE.
        CLEAR: <input_field>-value, <input_field>-value_range.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_results.

    DATA:
      error_msg      TYPE symsg,
      ddut_fieldname TYPE fnam_____4,
      value          TYPE rsdsselop_.

    LOOP AT  result_ranges ASSIGNING FIELD-SYMBOL(<result_range_for_tab>).
      LOOP AT <result_range_for_tab>-frange_t ASSIGNING FIELD-SYMBOL(<result_range_line>).
        READ TABLE field_table_ref->*
          WITH KEY
          ddic_tabname = <result_range_for_tab>-tablename
          ddic_fieldname = <result_range_line>-fieldname
          ASSIGNING FIELD-SYMBOL(<input_field>).
        ASSERT sy-subrc = 0.
        IF <input_field>-only_parameter = abap_false.
          CONTINUE.
        ENDIF.

        CASE lines( <result_range_line>-selopt_t ).
          WHEN 0.
            CLEAR value.
          WHEN 1.
            READ TABLE <result_range_line>-selopt_t INDEX 1 ASSIGNING FIELD-SYMBOL(<selopt_line>).
            ASSERT sy-subrc = 0.
            value = <selopt_line>-low.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

        CLEAR error_msg.
        ddut_fieldname = <input_field>-ddic_fieldname.

        CALL FUNCTION 'DDUT_INPUT_CHECK'
          EXPORTING
            tabname            = <input_field>-ddic_tabname
            fieldname          = ddut_fieldname
            value              = value
            accept_all_initial = abap_true
            value_list         = 'S'
          IMPORTING
            msgid              = error_msg-msgid
            msgty              = error_msg-msgty
            msgno              = error_msg-msgno
            msgv1              = error_msg-msgv1
            msgv2              = error_msg-msgv2
            msgv3              = error_msg-msgv3
            msgv4              = error_msg-msgv4.
        IF error_msg IS NOT INITIAL.
          zcx_abapgit_exception=>raise_t100(
            iv_msgid = error_msg-msgid
            iv_msgno = error_msg-msgno
            iv_msgv1 = error_msg-msgv1
            iv_msgv2 = error_msg-msgv2
            iv_msgv3 = error_msg-msgv3
            iv_msgv4 = error_msg-msgv4 ).
        ELSEIF <input_field>-param_obligatory = abap_true AND value IS INITIAL.
          zcx_abapgit_exception=>raise( |Field '{ <input_field>-name }' is obligatory| ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
