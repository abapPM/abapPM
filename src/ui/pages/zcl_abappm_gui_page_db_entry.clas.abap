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
        !iv_key          TYPE zif_abappm_persist_apm=>ty_key
        !iv_edit_mode    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_page)   TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_key          TYPE zif_abappm_persist_apm=>ty_key
        !iv_edit_mode    TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        update      TYPE string VALUE 'update',
        switch_mode TYPE string VALUE 'switch_mode',
      END OF c_action.

    CONSTANTS c_edit_form_id TYPE string VALUE 'db_form'.
    CONSTANTS c_css_url TYPE string VALUE 'css/page_db_entry.css'.

    CLASS-DATA gi_persist TYPE REF TO zif_abappm_persist_apm.

    DATA:
      ms_data         TYPE zif_abappm_persist_apm=>ty_zabappm,
      ms_previous     TYPE zif_abappm_persist_apm=>ty_zabappm,
      mv_content_type TYPE string,
      mv_edit_mode    TYPE abap_bool.

    METHODS load_entry
      IMPORTING
        !iv_key        TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(rs_data) TYPE zif_abappm_persist_apm=>ty_zabappm
      RAISING
        zcx_abapgit_exception.

    METHODS register_stylesheet
      RAISING
        zcx_abapgit_exception.

    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_view
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_edit
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS get_entry_tag
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS dbcontent_decode
      IMPORTING
        io_form_data      TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rs_content) TYPE zif_abappm_persist_apm=>ty_zabappm
      RAISING
        zcx_abapgit_exception.

    METHODS validate_and_pretty_json
      IMPORTING
        iv_value       TYPE string
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS do_update
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

    super->constructor( ).

    register_stylesheet( ).

    mv_edit_mode    = iv_edit_mode.
    mv_content_type = zcl_abappm_persist_apm=>explain_key( iv_key )-content_type.
    ms_data         = load_entry( iv_key ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_page_db_entry.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key       = iv_key
        iv_edit_mode = iv_edit_mode.

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

    DATA lx_error TYPE REF TO zcx_abappm_error.

    ASSERT is_content-keys IS NOT INITIAL.

    " Validation might raise expection but we want to keep the edited (inconsistent) value
    ms_data-value = is_content-value.

    IF mv_content_type = zif_abappm_persist_apm=>c_content_type-json.
      ms_data-value = validate_and_pretty_json( is_content-value ).
    ENDIF.

    TRY.
        gi_persist->save(
          iv_key   = is_content-keys
          iv_value = is_content-value ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    COMMIT WORK.

    MESSAGE 'Entry successfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD get_entry_tag.
    rv_html =
      |<dl class="entry-tag">| &&
      |<div><dt>Key</dt><dd>{ ms_data-keys }</dd></div>| &&
      |</dl>|.
  ENDMETHOD.


  METHOD load_entry.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    TRY.
        rs_data = gi_persist->load( iv_key ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

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

    DATA lv_value TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( |<form id="{ c_edit_form_id }" method="post" action="sapevent:{ c_action-update }">| ).
    ri_html->add( |<input type="hidden" name="keys" value="{ escape(
      val    = ms_data-keys
      format = cl_abap_format=>e_html_attr ) }">| ).

    lv_value = escape(
      val    = ms_data-value
      format = cl_abap_format=>e_html_text ).

    IF mv_content_type = zif_abappm_persist_apm=>c_content_type-json.
      ri_html->add( |<textarea rows="40" cols="200" name="value" id="editor">{ lv_value }</textarea>| ).
      " TODO: Replace with
      " ii_html->add( |<json-editor value="{ lv_value }" indent="2"></json-editor>| ).
    ELSE.
      ri_html->add( |<textarea rows="40" cols="200" name="value" id="editor">{ lv_value }</textarea>| ).
    ENDIF.

    ri_html->add( '</form>' ).

  ENDMETHOD.


  METHOD render_header.
    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="toolbar">' ).
    ri_html->add( get_entry_tag( ) ).
    ri_html->add( '</div>' ).
  ENDMETHOD.


  METHOD render_scripts.
    ri_html = zcl_abapgit_html=>create( ).

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    " TODO: Replace with
    " ri_html->add( lcl_json_editor=>get_javascript( ) ).
  ENDMETHOD.


  METHOD render_view.
    ri_html = zcl_abapgit_html=>create( ).

    " Better not to use syntax highlighter so we see the actual, unmodified data
    ri_html->add( |<pre class="syntax-hl">{ ms_data-value }</pre>| ).
  ENDMETHOD.


  METHOD validate_and_pretty_json.

    DATA lx_error TYPE REF TO zcx_abappm_ajson_error.

    TRY.
        rv_json = zcl_abappm_ajson=>new( )->parse(
          iv_json            = iv_value
          iv_keep_item_order = abap_true )->stringify( 2 ).
      CATCH zcx_abappm_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-switch_mode.
        mv_edit_mode     = boolc( mv_edit_mode = abap_false ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-update.
        do_update( dbcontent_decode( ii_event->form_data( ) ) ).
        mv_edit_mode     = abap_false.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA lv_txt TYPE string.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'main' ).

    IF mv_edit_mode = abap_true.
      ro_toolbar->add(
        iv_txt = 'Save'
        iv_act = |submitFormById('{ c_edit_form_id }');|
        iv_typ = zif_abapgit_html=>c_action_type-onclick
        iv_opt = zif_abapgit_html=>c_html_opt-strong ).
    ELSE.
      IF mv_edit_mode = abap_true.
        lv_txt = 'Display'.
      ELSE.
        lv_txt = 'Edit'.
      ENDIF.
      ro_toolbar->add(
        iv_txt = lv_txt
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

    DATA lx_error TYPE REF TO zcx_abappm_error.

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="db-entry">' ).

    ri_html->add( render_header( ) ).

    IF mv_edit_mode = abap_true.
      TRY.
          gi_persist->lock( ms_data-keys ).
        CATCH zcx_abappm_error INTO lx_error.
          mv_edit_mode = abap_false.
          zcx_abapgit_exception=>raise_with_text( lx_error ).
      ENDTRY.

      ri_html->add( render_edit( ) ).
    ELSE.
      ri_html->add( render_view( ) ).
    ENDIF.

    ri_html->add( '</div>' ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
ENDCLASS.
