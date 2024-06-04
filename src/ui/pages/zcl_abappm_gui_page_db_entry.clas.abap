CLASS zcl_abappm_gui_page_db_entry DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Database Entry
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_page_title.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING
        !iv_key        TYPE zif_abappm_persist_apm=>ty_key
        !iv_edit_mode  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_key       TYPE zif_abappm_persist_apm=>ty_key
        !iv_edit_mode TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        update      TYPE string VALUE 'update',
        switch_mode TYPE string VALUE 'switch_mode',
      END OF c_action.

    CONSTANTS c_edit_form_id TYPE string VALUE `db_form`.
    CONSTANTS c_css_url TYPE string VALUE 'css/page_db_entry.css'.

    CLASS-DATA gi_persist TYPE REF TO zif_abappm_persist_apm.

    DATA ms_data TYPE zif_abappm_persist_apm=>ty_zabappm.
    DATA mv_edit_mode TYPE abap_bool.

    METHODS register_stylesheet
      RAISING
        zcx_abapgit_exception.

    METHODS render_view
      IMPORTING
        iv_raw_db_value TYPE zif_abappm_persist_apm=>ty_zabappm-value
        ii_html         TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_edit
      IMPORTING
        iv_raw_db_value TYPE zif_abappm_persist_apm=>ty_zabappm-value
        ii_html         TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html.

    CLASS-METHODS render_entry_tag
      IMPORTING
        is_key         TYPE zif_abappm_persist_apm=>ty_zabappm
      RETURNING
        VALUE(rv_html) TYPE string.

    CLASS-METHODS dbcontent_decode
      IMPORTING
        io_form_data      TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rs_content) TYPE zif_abappm_persist_apm=>ty_zabappm
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS validate_json
      IMPORTING
        iv_value TYPE string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS do_update
      IMPORTING
        is_content TYPE zif_abappm_persist_apm=>ty_zabappm
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_page_db_entry IMPLEMENTATION.


  METHOD class_constructor.
    gi_persist = zcl_abappm_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.

    DATA lx_error TYPE REF TO zcx_abappm_persist_apm.

    super->constructor( ).

    register_stylesheet( ).
    mv_edit_mode = iv_edit_mode.

    TRY.
        ms_data = gi_persist->load( iv_key ).
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_page_db_entry.

    CREATE OBJECT lo_component
      EXPORTING
        iv_edit_mode = iv_edit_mode
        iv_key       = iv_key.

    ri_page = zcl_abappm_gui_page_hoc=>create(
      iv_extra_css_url       = c_css_url
      ii_page_title_provider = lo_component
      ii_child_component     = lo_component ).

  ENDMETHOD.


  METHOD dbcontent_decode.
    rs_content-keys  = io_form_data->get( 'KEYS' ).
    rs_content-value = io_form_data->get( 'VALUE' ).
  ENDMETHOD.


  METHOD do_update.

    DATA lx_error TYPE REF TO zcx_abappm_persist_apm.

    ASSERT is_content-keys IS NOT INITIAL.

    validate_json( is_content-value ).

    TRY.
        gi_persist->save(
          iv_key   = is_content-keys
          iv_value = is_content-value ).
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    COMMIT WORK.

    MESSAGE 'Entry successfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD register_stylesheet.

    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    CREATE OBJECT lo_buf.

    " @@abapmerge include zabapgit_css_page_db_entry.w3mi.data.css > lo_buf->add( '$$' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB_ENTRY'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.


  METHOD render_edit.

    ii_html->add( |<form id="{ c_edit_form_id }" method="post" action="sapevent:{ c_action-update }">| ).
    ii_html->add( |<input type="hidden" name="keys" value="{ ms_data-keys }">| ).
    ii_html->add( |<textarea rows="30" cols="200" name="value">{ iv_raw_db_value }</textarea>| ).
    ii_html->add( '</form>' ).

  ENDMETHOD.


  METHOD render_entry_tag.

    rv_html =
      |<dl class="entry-tag">| &&
      |<div><dt>Key</dt><dd>{ is_key-keys }</dd></div>| &&
      |</dl>|.

  ENDMETHOD.


  METHOD render_header.

    ii_html->add( '<div class="toolbar">' ).
    ii_html->add( render_entry_tag( ms_data ) ).
    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_view.

    DATA lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter.
    DATA lv_formatted   TYPE string.

    " Create syntax highlighter
    lo_highlighter = zcl_abapgit_syntax_factory=>create( '*.json' ).
    lv_formatted   = lo_highlighter->process_line( iv_raw_db_value ).

    ii_html->add( |<pre class="syntax-hl">{ lv_formatted }</pre>| ).

  ENDMETHOD.


  METHOD validate_json.

    DATA lx_error TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        zcl_abappm_ajson=>new( )->parse( iv_value ).
      CATCH zcx_abappm_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-switch_mode.
        mv_edit_mode = boolc( mv_edit_mode = abap_false ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-update.
        do_update( dbcontent_decode( ii_event->form_data( ) ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    IF mv_edit_mode = abap_true.
      ro_toolbar->add(
        iv_txt = 'Save'
        iv_act = |submitFormById('{ c_edit_form_id }');|
        iv_typ = zif_abapgit_html=>c_action_type-onclick
        iv_opt = zif_abapgit_html=>c_html_opt-strong ).
    ELSE.
      ro_toolbar->add(
        iv_txt = 'Edit'
        iv_act = |{ c_action-switch_mode }| ).
    ENDIF.

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.

    IF mv_edit_mode = abap_true.
      rv_title = 'Config Edit'.
    ELSE.
      rv_title = 'Config Display'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA:
      lx_error        TYPE REF TO zcx_abappm_persist_apm,
      lv_raw_db_value TYPE zif_abappm_persist_apm=>ty_zabappm-value.

    register_handlers( ).

    TRY.
        lv_raw_db_value = gi_persist->load( ms_data-keys )-value.
      CATCH zcx_abappm_persist_apm ##NO_HANDLER.
    ENDTRY.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="db-entry">' ).

    render_header( ri_html ).

    IF mv_edit_mode = abap_true.
      TRY.
          gi_persist->lock( ms_data-keys ).
        CATCH zcx_abappm_persist_apm INTO lx_error.
          zcx_abapgit_exception=>raise_with_text( lx_error ).
      ENDTRY.

      render_edit(
        iv_raw_db_value = lv_raw_db_value
        ii_html         = ri_html ).
    ELSE.
      render_view(
        iv_raw_db_value = lv_raw_db_value
        ii_html         = ri_html ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
