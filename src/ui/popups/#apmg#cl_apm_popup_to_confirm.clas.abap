CLASS /apmg/cl_apm_popup_to_confirm DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI HTML Confirmation Popup
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_renderable.

    CONSTANTS:
      BEGIN OF c_action,
        button_1 TYPE string VALUE 'button_1',
        button_2 TYPE string VALUE 'button_2',
        cancel   TYPE string VALUE 'cancel',
      END OF c_action.

    TYPES ty_char1 TYPE c LENGTH 1.

    CLASS-METHODS create
      IMPORTING
        !title                 TYPE clike
        !question              TYPE clike
        !button_1_text         TYPE clike DEFAULT 'Yes'
        !button_1_action       TYPE clike DEFAULT c_action-button_1
        !button_2_text         TYPE clike DEFAULT 'No'
        !button_2_action       TYPE clike DEFAULT c_action-button_2
        !default_button        TYPE ty_char1 DEFAULT '1'
        !display_cancel_button TYPE abap_bool DEFAULT abap_true
        !cancel_action         TYPE clike DEFAULT c_action-cancel
        !popup_type            TYPE clike DEFAULT 'ICON_MESSAGE_QUESTION'
      RETURNING
        VALUE(result)          TYPE REF TO /apmg/cl_apm_popup_to_confirm
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !title                 TYPE clike
        !question              TYPE clike
        !button_1_text         TYPE clike
        !button_1_action       TYPE clike
        !button_2_text         TYPE clike
        !button_2_action       TYPE clike
        !default_button        TYPE ty_char1
        !display_cancel_button TYPE abap_bool
        !cancel_action         TYPE clike
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

    DATA form      TYPE REF TO /apmg/cl_apm_html_form.
    DATA form_data TYPE REF TO /apmg/cl_apm_string_map.
    DATA closed    TYPE abap_bool.

    DATA title                 TYPE string.
    DATA question              TYPE string.
    DATA button_1_text         TYPE string.
    DATA button_2_text         TYPE string.
    DATA button_1_action       TYPE string.
    DATA button_2_action       TYPE string.
    DATA default_button        TYPE c LENGTH 1.
    DATA display_cancel_button TYPE abap_bool.
    DATA cancel_action         TYPE string.
    DATA popup_type            TYPE string.

    METHODS get_form_schema
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_form
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_popup_to_confirm IMPLEMENTATION.


  METHOD close.

    closed = abap_true.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    me->title                 = title.
    me->question              = question.
    me->button_1_text         = button_1_text.
    me->button_2_text         = button_2_text.
    me->button_1_action       = button_1_action.
    me->button_2_action       = button_2_action.
    me->default_button        = default_button.
    me->display_cancel_button = display_cancel_button.
    me->cancel_action         = cancel_action.
    me->popup_type            = popup_type.

    form_data = NEW #( ).
    form      = get_form_schema( ).

  ENDMETHOD.


  METHOD create.

    result = NEW /apmg/cl_apm_popup_to_confirm(
      title                 = title
      question              = question
      button_1_text         = button_1_text
      button_1_action       = button_1_action
      button_2_text         = button_2_text
      button_2_action       = button_2_action
      default_button        = default_button
      display_cancel_button = display_cancel_button
      cancel_action         = cancel_action
      popup_type            = popup_type ).

  ENDMETHOD.


  METHOD get_form_schema.

    DATA(icon) = ``.
    DATA(text) = question.
    DATA(default_1) = /apmg/if_apm_html_form=>c_cmd_type-input.
    DATA(default_2) = /apmg/if_apm_html_form=>c_cmd_type-input.
    DATA(default_cancel) = /apmg/if_apm_html_form=>c_cmd_type-input.

    result = /apmg/cl_apm_html_form=>create( iv_form_id = 'popup_to_confirm' ).

    result->start_group(
      iv_name  = c_id-title
      iv_label = title ).

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
      iv_label    = button_1_text
      iv_cmd_type = default_1
      iv_action   = button_1_action
    )->command(
      iv_label    = button_2_text
      iv_cmd_type = default_2
      iv_action   = button_2_action ).

    IF display_cancel_button = abap_true.
      result->command(
        iv_label    = 'Cancel'
        iv_cmd_type = default_cancel
        iv_action   = cancel_action ).
    ENDIF.

  ENDMETHOD.


  METHOD was_closed.

    result = closed.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN cancel_action OR /apmg/if_apm_gui_router=>c_action-go_back.
        close( ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    ri_html = form->render( io_values = form_data ).

  ENDMETHOD.
ENDCLASS.
