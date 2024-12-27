CLASS zcl_abappm_gui_page_db_entry DEFINITION PUBLIC FINAL CREATE PUBLIC
  INHERITING FROM zcl_abappm_gui_component.

************************************************************************
* apm GUI Database Entry
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable,
      zif_abapgit_gui_page_title.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING
        !key          TYPE zif_abappm_persist_apm=>ty_key
        !edit_mode    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !key       TYPE zif_abappm_persist_apm=>ty_key
        !edit_mode TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

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

    CLASS-DATA db_persist TYPE REF TO zif_abappm_persist_apm.

    DATA:
      db_entry     TYPE zif_abappm_persist_apm=>ty_zabappm,
      content_type TYPE string,
      edit_mode    TYPE abap_bool.

    METHODS load_entry
      IMPORTING
        !key          TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE zif_abappm_persist_apm=>ty_zabappm
      RAISING
        zcx_abapgit_exception.

    METHODS register_stylesheet
      RAISING
        zcx_abapgit_exception.

    METHODS get_scripts
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_view
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_edit
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS get_entry_tag
      RETURNING
        VALUE(result) TYPE string.

    METHODS validate_and_pretty_json
      IMPORTING
        !value        TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS do_update
      IMPORTING
        !key   TYPE csequence
        !value TYPE csequence
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_page_db_entry IMPLEMENTATION.


  METHOD class_constructor.

    db_persist = zcl_abappm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    register_stylesheet( ).

    me->edit_mode = edit_mode.
    content_type  = zcl_abappm_persist_apm=>explain_key( key )-content_type.
    db_entry      = load_entry( key ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW zcl_abappm_gui_page_db_entry(
      key       = key
      edit_mode = edit_mode ).

    result = zcl_abappm_gui_page_hoc=>create(
      extra_css_url       = c_css_url
      page_title_provider = component
      child_component     = component ).

  ENDMETHOD.


  METHOD do_update.

    ASSERT key IS NOT INITIAL AND strlen( key ) < zif_abappm_persist_apm=>c_max_key_len.

    " Validation might raise expection but we want to keep the edited (inconsistent) value
    db_entry-value = value.

    IF content_type = zif_abappm_persist_apm=>c_content_type-json.
      db_entry-value = validate_and_pretty_json( value ).
    ENDIF.

    TRY.
        db_persist->save(
          key   = db_entry-keys
          value = db_entry-value ).
      CATCH zcx_abappm_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

    COMMIT WORK.

    MESSAGE 'Entry successfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD get_entry_tag.

    result =
      |<dl class="entry-tag">| &&
      |<div><dt>Key</dt><dd>{ db_entry-keys }</dd></div>| &&
      |</dl>|.

  ENDMETHOD.


  METHOD get_scripts.

    DATA(html) = zcl_abapgit_html=>create( ).

    html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    " TODO: Replace with
    " html->add( lcl_json_editor=>get_javascript( ) )

    result = html.

  ENDMETHOD.


  METHOD load_entry.

    TRY.
        result = db_persist->load( key ).
      CATCH zcx_abappm_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

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

    IF content_type = zif_abappm_persist_apm=>c_content_type-json.
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


  METHOD validate_and_pretty_json.

    TRY.
        result = zcl_abappm_ajson=>parse(
          iv_json            = value
          iv_keep_item_order = abap_true )->stringify( 2 ).
      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-switch_mode.
        edit_mode        = xsdbool( edit_mode = abap_false ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-update.
        do_update(
          key   = ii_event->form_data( )->get( 'KEYS' )
          value = ii_event->form_data( )->get( 'VALUE' ) ).
        edit_mode        = abap_false.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA(toolbar) = zcl_abapgit_html_toolbar=>create( 'main' ).

    IF edit_mode = abap_true.
      toolbar->add(
        iv_txt = 'Save'
        iv_act = |submitFormById('{ c_edit_form_id }');|
        iv_typ = zif_abapgit_html=>c_action_type-onclick
        iv_opt = zif_abapgit_html=>c_html_opt-strong ).
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
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.

    IF edit_mode = abap_true.
      rv_title = 'Config Edit'.
    ELSE.
      rv_title = 'Config Display'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    DATA(html) = zcl_abapgit_html=>create( ).

    html->add( '<div class="db-entry">' ).

    render_header( html ).

    IF edit_mode = abap_true.
      TRY.
          db_persist->lock( db_entry-keys ).
        CATCH zcx_abappm_error INTO DATA(error).
          edit_mode = abap_false.
          zcx_abapgit_exception=>raise_with_text( error ).
      ENDTRY.

      render_edit( html ).
    ELSE.
      render_view( html ).
    ENDIF.

    html->add( '</div>' ).

    register_deferred_script( get_scripts( ) ).

    ri_html = html.

  ENDMETHOD.
ENDCLASS.
