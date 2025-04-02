CLASS zcl_abappm_gui_chunk_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    CLASS-METHODS render_error
      IMPORTING
        !ix_error       TYPE REF TO zcx_abappm_error OPTIONAL
        !iv_error       TYPE string OPTIONAL
        !iv_extra_style TYPE string OPTIONAL
      RETURNING
        VALUE(ri_html)  TYPE REF TO zif_abappm_html.

    CLASS-METHODS render_success
      IMPORTING
        !iv_message    TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abappm_html.

    CLASS-METHODS render_js_error_banner
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    CLASS-METHODS render_error_message_box
      IMPORTING
        !ix_error      TYPE REF TO zcx_abappm_error
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abappm_html.

    CLASS-METHODS render_table_header
      IMPORTING
        !it_col_spec         TYPE zif_abapgit_definitions=>ty_col_spec_tt
        !iv_order_by         TYPE string
        !iv_order_descending TYPE abap_bool
      RETURNING
        VALUE(ri_html)       TYPE REF TO zif_abappm_html.

    CLASS-METHODS render_table_footer
      IMPORTING
        !iv_message    TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abappm_html.

    CLASS-METHODS render_warning_banner
      IMPORTING
        !iv_text       TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abappm_html.

    CLASS-METHODS render_infopanel
      IMPORTING
        !iv_div_id     TYPE string
        !iv_title      TYPE string
        !iv_hide       TYPE abap_bool DEFAULT abap_true
        !iv_hint       TYPE string OPTIONAL
        !iv_scrollable TYPE abap_bool DEFAULT abap_true
        !io_content    TYPE REF TO zif_abappm_html
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    CLASS-METHODS render_package_name
      IMPORTING
        !iv_package        TYPE devclass
        !iv_interactive    TYPE abap_bool DEFAULT abap_true
        !iv_suppress_title TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html)     TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    CLASS-METHODS render_user_name
      IMPORTING
        !iv_username       TYPE syuname
        !iv_interactive    TYPE abap_bool DEFAULT abap_true
        !iv_icon_only      TYPE abap_bool DEFAULT abap_false
        !iv_suppress_title TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html)     TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    CLASS-METHODS render_transport
      IMPORTING
        !iv_transport   TYPE trkorr
        !iv_interactive TYPE abap_bool DEFAULT abap_true
        !iv_icon_only   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html)  TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    CLASS-METHODS render_timestamp
      IMPORTING
        !iv_timestamp      TYPE timestampl
      RETURNING
        VALUE(rv_rendered) TYPE string.

    CLASS-METHODS render_text_input
      IMPORTING
        !iv_name       TYPE string
        !iv_label      TYPE string
        !iv_value      TYPE string OPTIONAL
        !iv_max_length TYPE string OPTIONAL
        !iv_autofocus  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abappm_html.

    CLASS-METHODS render_label_list
      IMPORTING
        !it_labels           TYPE string_table
        !io_label_colors     TYPE REF TO zcl_abapgit_string_map
        !iv_clickable_action TYPE string OPTIONAL
        !iv_unlisted         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_html)       TYPE string.

    CLASS-METHODS render_help_hint
      IMPORTING
        !iv_text_to_wrap TYPE string
        !iv_add_class    TYPE string OPTIONAL
      RETURNING
        VALUE(rv_html)   TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gv_time_zone TYPE timezone .

    CLASS-METHODS get_t100_text
      IMPORTING
        !iv_msgid      TYPE scx_t100key-msgid
        !iv_msgno      TYPE scx_t100key-msgno
      RETURNING
        VALUE(rv_text) TYPE string .

    CLASS-METHODS normalize_program_name
      IMPORTING
        !iv_program_name                  TYPE sy-repid
      RETURNING
        VALUE(rv_normalized_program_name) TYPE string .

ENDCLASS.



CLASS zcl_abappm_gui_chunk_lib IMPLEMENTATION.


  METHOD class_constructor.

    DATA lv_fm TYPE string.
    lv_fm = 'GET_SYSTEM_TIMEZONE'.

    TRY.
        CALL METHOD ('CL_ABAP_TSTMP')=>get_system_timezone
          RECEIVING
            system_timezone = gv_time_zone.
      CATCH cx_sy_dyn_call_illegal_method.
        CALL FUNCTION lv_fm
          IMPORTING
            timezone            = gv_time_zone
          EXCEPTIONS
            customizing_missing = 1
            OTHERS              = 2 ##FM_SUBRC_OK.
    ENDTRY.

  ENDMETHOD.


  METHOD get_t100_text.

    MESSAGE ID iv_msgid TYPE 'S' NUMBER iv_msgno WITH '&1' '&2' '&3' '&4' INTO rv_text.

    " Don't return any generic messages like `&1 &2 &3 &4`
    IF rv_text CO ' 0123456789&'.
      CLEAR rv_text.
    ENDIF.

  ENDMETHOD.


  METHOD normalize_program_name.

    rv_normalized_program_name = substring_before(
      val   = iv_program_name
      regex = `(=+CP)?$` ).

  ENDMETHOD.


  METHOD render_error.

    DATA lv_error TYPE string.
    DATA lv_class TYPE string VALUE 'panel error center'.

    IF iv_extra_style IS NOT INITIAL.
      lv_class = lv_class && ` ` && iv_extra_style.
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    IF ix_error IS BOUND.
      lv_error = ix_error->get_text( ).
    ELSE.
      lv_error = iv_error.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_error_message_box.

    CONSTANTS:
      BEGIN OF c_section_text,
        cause           TYPE string VALUE `Cause`,
        system_response TYPE string VALUE `System response`,
        what_to_do      TYPE string VALUE `Procedure`,
        sys_admin       TYPE string VALUE `System administration`,
      END OF c_section_text .
    CONSTANTS:
      BEGIN OF c_section_token,
        cause           TYPE string VALUE `&CAUSE&`,
        system_response TYPE string VALUE `&SYSTEM_RESPONSE&`,
        what_to_do      TYPE string VALUE `&WHAT_TO_DO&`,
        sys_admin       TYPE string VALUE `&SYS_ADMIN&`,
      END OF c_section_token .

    DATA:
      lv_error_text          TYPE string,
      lv_longtext            TYPE string,
      lt_longtext_paragraphs TYPE string_table,
      lv_program_name        TYPE sy-repid,
      lv_title               TYPE string,
      lv_text                TYPE string.
    FIELD-SYMBOLS:
      <lv_longtext_paragraph> TYPE string.


    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    lv_error_text = ix_error->get_text( ).
    lv_longtext = ix_error->if_message~get_longtext( abap_true ).

    IF lv_longtext IS NOT INITIAL.
      lv_error_text = |{ lv_error_text } <span class="emphasis">More...</span>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ c_section_text-cause }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ c_section_text-system_response }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ c_section_text-what_to_do }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ c_section_text-sys_admin }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN lv_longtext
        WITH cl_abap_char_utilities=>newline.

      SPLIT lv_longtext AT cl_abap_char_utilities=>newline INTO TABLE lt_longtext_paragraphs.
      CLEAR lv_longtext.

      LOOP AT lt_longtext_paragraphs ASSIGNING <lv_longtext_paragraph>.
        CONDENSE <lv_longtext_paragraph>.

        IF <lv_longtext_paragraph> IS INITIAL.
          CONTINUE.
        ENDIF.

        lv_longtext = |{ lv_longtext }<p>{ <lv_longtext_paragraph> }</p>{ cl_abap_char_utilities=>newline }|.
      ENDLOOP.

      lv_longtext = |{ lv_longtext }<br>|.
    ENDIF.

    ri_html->add( |<div id="message" class="message-panel">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error_text }| ).
    ri_html->add( |<div class="message-panel-bar">| ).

    ri_html->add_a(
      iv_txt   = `&#x274c;`
      iv_act   = `toggleDisplay('message')`
      iv_class = `close-btn`
      iv_typ   = zif_abappm_html=>c_action_type-onclick ).

    ri_html->add( |</div>| ).

    ri_html->add( |<div class="message-panel-bar message-panel-commands">| ).

    IF ix_error->if_t100_message~t100key-msgid IS NOT INITIAL.

      lv_title = get_t100_text(
        iv_msgid = ix_error->if_t100_message~t100key-msgid
        iv_msgno = ix_error->if_t100_message~t100key-msgno ).

      IF lv_title IS NOT INITIAL.
        lv_text = |Message E{ ix_error->if_t100_message~t100key-msgno }({ ix_error->if_t100_message~t100key-msgid })|.

        ri_html->add_a(
          iv_txt   = lv_text
          iv_typ   = zif_abappm_html=>c_action_type-sapevent
          iv_act   = zif_abapgit_definitions=>c_action-goto_message
          iv_title = lv_title
          iv_id    = `a_goto_message` ).
      ENDIF.
    ENDIF.

    ix_error->get_source_position( IMPORTING program_name = lv_program_name ).

    lv_title = normalize_program_name( lv_program_name ).

    ri_html->add_a(
      iv_txt   = `Goto source`
      iv_act   = zif_abapgit_definitions=>c_action-goto_source
      iv_typ   = zif_abappm_html=>c_action_type-sapevent
      iv_title = lv_title
      iv_id    = `a_goto_source` ).

    ri_html->add_a(
      iv_txt = `Callstack`
      iv_act = zif_abapgit_definitions=>c_action-show_callstack
      iv_typ = zif_abappm_html=>c_action_type-sapevent
      iv_id  = `a_callstack` ).

    ri_html->add( |</div>| ).
    ri_html->add( |<div class="message-panel-commands">| ).
    ri_html->add( |{ lv_longtext }| ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_help_hint.

    DATA li_html TYPE REF TO zif_abappm_html.
    DATA lv_add_class TYPE string.

    li_html = zcl_abappm_html=>create( ).

    IF iv_add_class IS NOT INITIAL.
      lv_add_class = ` ` && iv_add_class.
    ENDIF.

    li_html->add( |<div class="form-field-help-tooltip{ lv_add_class }">| ).
    li_html->add_icon(
      iv_name = 'question-circle-solid'
      iv_class = 'blue' ).
    li_html->add( `<div class="form-field-help-tooltip-text">` ).
    li_html->add( iv_text_to_wrap ).
    li_html->add( `</div>` ).
    li_html->add( `</div>` ).

    rv_html = li_html->render( iv_no_line_breaks = abap_true ).

  ENDMETHOD.


  METHOD render_infopanel.

    DATA lv_display TYPE string.
    DATA lv_class TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    IF iv_hide = abap_true. " Initially hide
      lv_display = 'display:none'.
    ENDIF.

    lv_class = 'info-panel'.
    IF iv_scrollable = abap_false. " Initially hide
      lv_class = lv_class && ' info-panel-fixed'.
    ENDIF.

    ri_html->add( |<div id="{ iv_div_id }" class="{ lv_class }" style="{ lv_display }">| ).

    ri_html->add( |<div class="info-title">{ iv_title }|
               && '<div class="float-right">'
               && ri_html->a(
                    iv_txt   = '&#x274c;'
                    iv_typ   = zif_abappm_html=>c_action_type-onclick
                    iv_act   = |toggleDisplay('{ iv_div_id }')|
                    iv_class = 'close-btn' )
               && '</div></div>' ).

    IF iv_hint IS NOT INITIAL.
      ri_html->add( '<div class="info-hint">'
        && ri_html->icon( iv_name = 'exclamation-triangle'
                          iv_class = 'pad-right' )
        && iv_hint
        && '</div>' ).
    ENDIF.

    ri_html->add( |<div class="info-list">| ).
    ri_html->add( io_content ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_js_error_banner.
    CREATE OBJECT ri_html TYPE zcl_abappm_html.
    ri_html->add( '<div id="js-error-banner" class="dummydiv error">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/red' ) }| &&
                  ' If this does not disappear soon,' &&
                  ' then there is a JS init error, please log an issue' ).
    ri_html->add( '</div>' ).
  ENDMETHOD.


  METHOD render_label_list.

    " FUTURE

  ENDMETHOD.


  METHOD render_package_name.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      lv_jump     TYPE string,
      lv_title    TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    IF iv_package IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_suppress_title = abap_false.
      lv_title = zcl_abapgit_factory=>get_sap_package( iv_package )->read_description( ).
    ENDIF.

    lv_obj_name = iv_package.
    lv_jump = zcl_abapgit_html_action_utils=>jump_encode(
      iv_obj_type = 'DEVC'
      iv_obj_name = lv_obj_name ).

    ri_html->add( |<span class="package-box">| ).
    ri_html->add_icon( iv_name = 'box/grey70'
                       iv_hint = 'SAP package' ).
    IF iv_interactive = abap_true.
      ri_html->add_a( iv_act   = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_jump }|
                      iv_title = lv_title
                      iv_txt   = |{ iv_package }| ).
    ELSE.
      ri_html->add( iv_package ).
    ENDIF.
    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_success.

    ri_html = zcl_abappm_html=>create( ).
    ri_html->add( '<div class="dummydiv success">' ).
    ri_html->add_icon( 'check' ).
    ri_html->add( iv_message ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_table_footer.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    ri_html->add( '<tfoot>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td colspan="100%">' ).

    ri_html->add( iv_message ).

    ri_html->add( '</td>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</tfoot>' ).

  ENDMETHOD.


  METHOD render_table_header.

    DATA:
      lv_tmp       TYPE string,
      lv_disp_name TYPE string.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF it_col_spec.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).

    LOOP AT it_col_spec ASSIGNING <ls_col>.
      " e.g. <th class="ro-detail">Created at [{ gv_time_zone }]</th>
      lv_tmp = '<th'.
      IF <ls_col>-css_class IS NOT INITIAL.
        lv_tmp = lv_tmp && | class="{ <ls_col>-css_class }"|.
      ENDIF.
      lv_tmp = lv_tmp && '>'.

      IF <ls_col>-display_name IS NOT INITIAL.
        lv_disp_name = <ls_col>-display_name.
        IF <ls_col>-add_tz = abap_true.
          lv_disp_name = lv_disp_name && | [{ gv_time_zone }]|.
        ENDIF.
        IF <ls_col>-tech_name = iv_order_by.
          IF iv_order_descending = abap_true.
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ zif_abapgit_definitions=>c_action-change_order_by }|
              iv_title = <ls_col>-title ).
          ELSE.
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ zif_abapgit_definitions=>c_action-direction }?direction=DESCENDING|
              iv_title = <ls_col>-title ).
          ENDIF.
        ELSEIF <ls_col>-allow_order_by = abap_true.
          lv_tmp = lv_tmp && ri_html->a(
            iv_txt   = lv_disp_name
            iv_act   = |{ zif_abapgit_definitions=>c_action-change_order_by }?orderBy={ <ls_col>-tech_name }|
            iv_title = <ls_col>-title ).
        ELSE.
          lv_tmp = lv_tmp && lv_disp_name.
        ENDIF.
      ENDIF.
      IF <ls_col>-tech_name = iv_order_by
      AND iv_order_by IS NOT INITIAL.
        IF iv_order_descending = abap_true.
          lv_tmp = lv_tmp && | &#x25BE;|. " arrow down
        ELSE.
          lv_tmp = lv_tmp && | &#x25B4;|. " arrow up
        ENDIF.
      ENDIF.

      lv_tmp = lv_tmp && '</th>'.
      ri_html->add( lv_tmp ).
    ENDLOOP.

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = lv_attrs && | maxlength="{ iv_max_length }"|.
    ENDIF.

    IF iv_autofocus = abap_true.
      lv_attrs = lv_attrs && | autofocus|.
    ENDIF.

    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).

  ENDMETHOD.


  METHOD render_timestamp.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    CONVERT TIME STAMP iv_timestamp
      TIME ZONE gv_time_zone
      INTO DATE lv_date
      TIME lv_time.

    rv_rendered = |{ lv_date DATE = USER } { lv_time TIME = USER }|.

  ENDMETHOD.


  METHOD render_transport.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    IF iv_transport IS INITIAL.
      RETURN.
    ENDIF.

    lv_title = zcl_abapgit_factory=>get_cts_api( )->read_description( iv_transport ).

    lv_jump = |{ zif_abapgit_definitions=>c_action-jump_transport }?transport={ iv_transport }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = |Transport { iv_transport }|
                      iv_txt   = zcl_abappm_html=>icon( 'truck-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="transport-box">| ).

      ri_html->add_icon( iv_name = 'truck-solid/grey70'
                         iv_hint = 'Transport' ).
      IF iv_interactive = abap_true.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_transport }| ).
      ELSE.
        ri_html->add( iv_transport ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.


  METHOD render_user_name.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.

    IF iv_username IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_username <> zcl_abapgit_objects_super=>c_user_unknown AND iv_suppress_title = abap_false.
      lv_title = zcl_abapgit_user_record=>get_title( iv_username ).
    ENDIF.

    lv_jump = |{ zif_abapgit_definitions=>c_action-jump_user }?user={ iv_username }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = lv_title
                      iv_txt   = zcl_abappm_html=>icon( 'user-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="user-box">| ).

      ri_html->add_icon( iv_name = 'user-solid/grey70'
                         iv_hint = 'User name' ).
      IF iv_interactive = abap_true AND iv_username <> zcl_abapgit_objects_super=>c_user_unknown.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_username }| ).
      ELSE.
        ri_html->add( iv_username ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.


  METHOD render_warning_banner.

    CREATE OBJECT ri_html TYPE zcl_abappm_html.
    ri_html->add( '<div class="dummydiv warning">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/yellow' ) } { iv_text }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
