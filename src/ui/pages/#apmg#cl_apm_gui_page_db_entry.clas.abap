CLASS /apmg/cl_apm_gui_page_db_entry DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Database Entry
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_menu_provider,
      /apmg/if_apm_gui_renderable,
      /apmg/if_apm_gui_page_title.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING
        !key          TYPE /apmg/if_apm_persist_apm=>ty_key
        !edit_mode    TYPE abap_bool DEFAULT abap_false
        !back_on_save TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !key          TYPE /apmg/if_apm_persist_apm=>ty_key
        !edit_mode    TYPE abap_bool
        !back_on_save TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        update      TYPE string VALUE 'update',
        switch_mode TYPE string VALUE 'switch_mode',
      END OF c_action.

    CONSTANTS:
      c_edit_form_id TYPE string VALUE 'db_form',
      c_css_url      TYPE string VALUE 'css/page_db_entry.css'.

    CLASS-DATA db_persist TYPE REF TO /apmg/if_apm_persist_apm.

    DATA:
      db_entry     TYPE /apmg/if_apm_persist_apm=>ty_zabappm,
      content_type TYPE string,
      edit_mode    TYPE abap_bool,
      back_on_save TYPE abap_bool.

    METHODS load_entry
      IMPORTING
        !key          TYPE /apmg/if_apm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_persist_apm=>ty_zabappm
      RAISING
        /apmg/cx_apm_error.

    METHODS register_stylesheet
      RAISING
        /apmg/cx_apm_error.

    METHODS get_scripts
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_view
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_edit
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_header
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS get_entry_tag
      RETURNING
        VALUE(result) TYPE string.

    METHODS do_update
      IMPORTING
        !key   TYPE csequence
        !value TYPE csequence
      RAISING
        /apmg/cx_apm_error.

    METHODS escape_percent_sign
      IMPORTING
        !value        TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_gui_page_db_entry IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-switch_mode.

        edit_mode        = xsdbool( edit_mode = abap_false ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-update.

        do_update(
          key   = ii_event->form_data( )->get( 'KEYS' )
          value = ii_event->form_data( )->get( 'VALUE' ) ).

        edit_mode = abap_false.
        IF back_on_save = abap_true.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-go_back_to_bookmark.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_menu_provider~get_menu.

    DATA(toolbar) = /apmg/cl_apm_html_toolbar=>create( 'apm-database-entry' ).

    IF edit_mode = abap_true.
      toolbar->add(
        iv_txt = 'Save'
        iv_act = |submitFormById('{ c_edit_form_id }');|
        iv_typ = /apmg/if_apm_html=>c_action_type-onclick
        iv_opt = /apmg/if_apm_html=>c_html_opt-strong ).
    ELSE.
      IF edit_mode = abap_true.
        DATA(txt) = `Display`.
      ELSE.
        txt = `Edit`.
      ENDIF.
      toolbar->add(
        iv_txt = txt
        iv_act = |{ c_action-switch_mode }| ).
    ENDIF.

    toolbar->add(
      iv_txt = 'Back'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_back ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_page_title~get_page_title.

    IF edit_mode = abap_true.
      rv_title = 'Config Edit'.
    ELSE.
      rv_title = 'Config Display'.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->add( '<div class="db-entry">' ).

    render_header( html ).

    IF edit_mode = abap_true.
      TRY.
          db_persist->lock( db_entry-keys ).
        CATCH /apmg/cx_apm_error INTO DATA(error).
          edit_mode = abap_false.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
      ENDTRY.

      render_edit( html ).
    ELSE.
      render_view( html ).
    ENDIF.

    html->add( '</div>' ).

    register_deferred_script( get_scripts( ) ).

    ri_html = html.

  ENDMETHOD.


  METHOD class_constructor.

    db_persist = /apmg/cl_apm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    register_stylesheet( ).

    me->edit_mode    = edit_mode.
    me->back_on_save = back_on_save.
    content_type     = /apmg/cl_apm_persist_apm=>explain_key( key )-content_type.
    db_entry         = load_entry( key ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_page_db_entry(
      key          = key
      edit_mode    = edit_mode
      back_on_save = back_on_save ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      extra_css_url       = c_css_url
      page_title_provider = component
      child_component     = component ).

  ENDMETHOD.


  METHOD do_update.

    ASSERT key IS NOT INITIAL AND strlen( key ) < /apmg/if_apm_persist_apm=>c_max_key_len.

    " Validation might raise exception but we want to keep the edited (inconsistent) value
    db_entry-value = escape_percent_sign( value ).

    IF content_type = /apmg/if_apm_persist_apm=>c_content_type-json.
      db_entry-value = /apmg/cl_apm_json=>validate_and_prettify( value ).
    ENDIF.

    db_persist->save(
      key   = db_entry-keys
      value = db_entry-value ).

    COMMIT WORK.

    MESSAGE 'Entry successfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD escape_percent_sign.

    " cl_apm_gui_event->unescape removes '%25' but it's valid in URLs that can be part of the content.
    " This is no general solution but we add it back here for /namespaces/ which are encoded as
    " #namespace#.
    result = replace(
      val   = value
      regex = '%23([a-zA-Z0-9]{3,8})%23'
      with  = '%2523$1%2523'
      occ   = 0 ).

  ENDMETHOD.


  METHOD get_entry_tag.

    result =
      |<dl class="entry-tag">| &&
      |<div><dt>Key</dt><dd>{ db_entry-keys }</dd></div>| &&
      |</dl>|.

  ENDMETHOD.


  METHOD get_scripts.

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    " TODO: Replace with
    " html->add( lcl_json_editor=>get_javascript( ) )

    result = html.

  ENDMETHOD.


  METHOD load_entry.

    result = db_persist->load( key ).

  ENDMETHOD.


  METHOD register_stylesheet.

    DATA(buffer) = NEW zcl_abapgit_string_buffer( ).

    " @@abapmerge include zabapgit_css_page_db_entry.w3mi.data.css > buffer->add( '$$' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB_ENTRY'
      iv_inline    = buffer->join_w_newline_and_flush( ) ).

  ENDMETHOD.


  METHOD render_edit.

    html->add( |<form id="{ c_edit_form_id }" method="post" action="sapevent:{ c_action-update }">| ).
    html->add( |<input type="hidden" name="keys" value="{ escape(
      val    = db_entry-keys
      format = cl_abap_format=>e_html_attr ) }">| ).

    DATA(value) = escape(
      val    = db_entry-value
      format = cl_abap_format=>e_html_text ).

    IF content_type = /apmg/if_apm_persist_apm=>c_content_type-json.
      html->add( |<textarea rows="41" cols="200" name="value" id="editor">{ value }</textarea>| ).
      " TODO: Replace with
      " ii_html->add( |<json-editor value="{ lv_value }" indent="2"></json-editor>| )
    ELSE.
      html->add( |<textarea rows="40" cols="200" name="value" id="editor">{ value }</textarea>| ).
    ENDIF.

    html->add( '</form>' ).

  ENDMETHOD.


  METHOD render_header.

    html->add( '<div class="toolbar">' ).
    html->add( get_entry_tag( ) ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_view.

    " Better not to use syntax highlighter so we see the actual, unmodified data
    html->add( |<pre class="syntax-hl">{ db_entry-value }</pre>| ).

  ENDMETHOD.
ENDCLASS.
