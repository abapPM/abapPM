CLASS /apmg/cl_apm_popup_to_confirm DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_renderable.

    TYPES:
      ty_char1 TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF c_action,
        button_1 TYPE string VALUE 'button_1',
        button_2 TYPE string VALUE 'button_2',
        cancel   TYPE string VALUE 'cancel',
      END OF c_action.

    CLASS-METHODS create
      IMPORTING
        !titlebar              TYPE clike
        !text_question         TYPE clike
        !text_button_1         TYPE clike DEFAULT 'Yes'
        !action_button_1       TYPE clike DEFAULT c_action-button_1
        !text_button_2         TYPE clike DEFAULT 'No'
        !action_button_2       TYPE clike DEFAULT c_action-button_2
        !default_button        TYPE ty_char1 DEFAULT '1'
        !display_cancel_button TYPE abap_bool DEFAULT abap_true
        !action_cancel         TYPE clike DEFAULT c_action-cancel
        !popup_type            TYPE clike DEFAULT 'ICON_MESSAGE_QUESTION'
      RETURNING
        VALUE(result)          TYPE REF TO /apmg/cl_apm_popup_to_confirm
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !titlebar              TYPE clike
        !text_question         TYPE clike
        !text_button_1         TYPE clike
        !action_button_1       TYPE clike
        !text_button_2         TYPE clike
        !action_button_2       TYPE clike
        !default_button        TYPE ty_char1
        !display_cancel_button TYPE abap_bool
        !action_cancel         TYPE clike
        !popup_type            TYPE clike
      RAISING
        /apmg/cx_apm_error.

    METHODS close.

    METHODS was_closed
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        title TYPE string VALUE 'title',
        icon  TYPE string VALUE 'icon',
        text  TYPE string VALUE 'text',
      END OF c_id.

    DATA form TYPE REF TO /apmg/cl_apm_html_form.
    DATA form_data TYPE REF TO /apmg/cl_apm_string_map.
    DATA closed TYPE abap_bool.

    DATA titlebar TYPE string.
    DATA text_question TYPE string.
    DATA text_button_1 TYPE string.
    DATA text_button_2 TYPE string.
    DATA action_button_1 TYPE string.
    DATA action_button_2 TYPE string.
    DATA default_button TYPE c LENGTH 1.
    DATA display_cancel_button TYPE abap_bool.
    DATA action_cancel TYPE string.
    DATA popup_type TYPE string.

    METHODS get_form_schema
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_form
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_popup_to_confirm IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN action_cancel OR /apmg/if_apm_gui_router=>c_action-go_back.
        " Handle go_back as a "graceful back" - implicit cancel by F3/ESC
        close( ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    ri_html = /apmg/cl_apm_html=>create( form->render( form_data ) ).

  ENDMETHOD.


  METHOD close.
    closed = abap_true.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    me->titlebar              = titlebar.
    me->text_question         = text_question.
    me->text_button_1         = text_button_1.
    me->text_button_2         = text_button_2.
    me->action_button_1       = action_button_1.
    me->action_button_2       = action_button_2.
    me->default_button        = default_button.
    me->display_cancel_button = display_cancel_button.
    me->action_cancel         = action_cancel.
    me->popup_type            = popup_type.

    form_data = NEW #( ).
    form      = get_form_schema( ).

  ENDMETHOD.


  METHOD create.

    CREATE OBJECT result
      EXPORTING
        titlebar              = titlebar
        text_question         = text_question
        text_button_1         = text_button_1
        action_button_1       = action_button_1
        text_button_2         = text_button_2
        action_button_2       = action_button_2
        default_button        = default_button
        display_cancel_button = display_cancel_button
        action_cancel         = action_cancel
        popup_type            = popup_type.

  ENDMETHOD.


  METHOD get_form_schema.

    DATA icon TYPE string.
    DATA text TYPE string.
    DATA default_1 TYPE i VALUE /apmg/if_apm_html_form=>c_cmd_type-input.
    DATA default_2 TYPE i VALUE /apmg/if_apm_html_form=>c_cmd_type-input.
    DATA default_cancel TYPE i VALUE /apmg/if_apm_html_form=>c_cmd_type-input.

    result = /apmg/cl_apm_html_form=>create( iv_form_id = 'popup_to_confirm' ).

    result->start_group(
      iv_name  = c_id-title
      iv_label = titlebar ).

    CASE popup_type.
      WHEN 'ICON_MESSAGE_QUESTION'.
        icon = 's_m_ques.gif'.
      WHEN 'ICON_MESSAGE_INFORMATION'.
        icon = 's_m_info.gif'.
      WHEN 'ICON_MESSAGE_WARNING'.
        icon = 's_m_warn.gif'.
      WHEN 'ICON_MESSAGE_ERROR'.
        icon = 's_m_erro.gif'.
      WHEN 'ICON_MESSAGE_CRITICAL'.
        icon = 's_m_crit.gif'.
    ENDCASE.

    IF icon IS NOT INITIAL.
      result->icon(
        iv_name = c_id-icon
        iv_icon = icon ).
    ENDIF.

    text = text_question.
    REPLACE ALL OCCURRENCES OF |\n| IN text WITH '<br>'.

    result->freetext(
      iv_name = c_id-text
      iv_text = text ).

    CASE default_button.
      WHEN '1'.
        default_1 = /apmg/if_apm_html_form=>c_cmd_type-input_main.
      WHEN '2'.
        default_2 = /apmg/if_apm_html_form=>c_cmd_type-input_main.
      WHEN 'A'.
        default_cancel = /apmg/if_apm_html_form=>c_cmd_type-input_main.
    ENDCASE.

    result->command(
      iv_label    = text_button_1
      iv_cmd_type = default_1
      iv_action   = action_button_1
    )->command(
      iv_label    = text_button_2
      iv_cmd_type = default_2
      iv_action   = action_button_2 ).

    IF display_cancel_button = abap_true.
      result->command(
        iv_label    = 'Cancel'
        iv_cmd_type = default_cancel
        iv_action   = action_cancel ).
    ENDIF.

  ENDMETHOD.


  METHOD was_closed.
    result = closed.
  ENDMETHOD.
ENDCLASS.
