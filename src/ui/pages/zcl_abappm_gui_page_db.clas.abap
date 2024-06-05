CLASS zcl_abappm_gui_page_db DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Database Utility
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_html_table.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        delete     TYPE string VALUE 'delete',
        backup     TYPE string VALUE 'backup',
        restore    TYPE string VALUE 'restore',
        db_display TYPE string VALUE 'db_display',
        db_edit    TYPE string VALUE 'db_edit',
      END OF c_action.

    CONSTANTS c_css_url TYPE string VALUE 'css/page_db.css'.
    CONSTANTS c_toc_filename TYPE string VALUE '#_Table_of_Content_#.txt'.

    TYPES:
      BEGIN OF ty_explanation,
        value TYPE string,
        extra TYPE string,
      END OF ty_explanation.

    CLASS-DATA gi_persist TYPE REF TO zif_abappm_persist_apm.

    METHODS register_stylesheet
      RAISING
        zcx_abapgit_exception.

    METHODS render_stats
      IMPORTING
        it_db_entries  TYPE zif_abappm_persist_apm=>ty_list
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_table
      IMPORTING
        it_db_entries  TYPE zif_abappm_persist_apm=>ty_list
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS do_backup_db
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS do_delete_entry
      IMPORTING
        !iv_key TYPE zif_abappm_persist_apm=>ty_key
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS do_restore_db
      RAISING
        zcx_abapgit_exception.

    METHODS explain_content
      IMPORTING
        !is_data       TYPE zif_abappm_persist_apm=>ty_zabappm
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_page_db IMPLEMENTATION.


  METHOD class_constructor.
    gi_persist = zcl_abappm_persist_apm=>get_instance( ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    register_stylesheet( ).
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_page_db.

    CREATE OBJECT lo_component.

    ri_page = zcl_abappm_gui_page_hoc=>create(
      iv_page_title         = 'Database Utility'
      iv_extra_css_url      = c_css_url
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD do_backup_db.

    DATA:
      lt_data     TYPE zif_abappm_persist_apm=>ty_list,
      lv_type     TYPE string,
      lv_text     TYPE string,
      lt_toc      TYPE string_table,
      lo_zip      TYPE REF TO cl_abap_zip,
      lv_zip      TYPE xstring,
      lv_path     TYPE string,
      lv_filename TYPE string,
      li_fe_serv  TYPE REF TO zif_abapgit_frontend_services.

    FIELD-SYMBOLS:
      <ls_data> LIKE LINE OF lt_data.

    lt_data = gi_persist->list( ).

    lv_text = |Table of Content\n|.
    INSERT lv_text INTO TABLE lt_toc.
    lv_text = |================\n|.
    INSERT lv_text INTO TABLE lt_toc.
    lv_text = |\n|.
    INSERT lv_text INTO TABLE lt_toc.

    CREATE OBJECT lo_zip.

    LOOP AT lt_data ASSIGNING <ls_data>.
      lv_filename = to_lower( <ls_data>-keys ) && '.json'.

      lo_zip->add(
        name    = lv_filename
        content = zcl_abapgit_convert=>string_to_xstring_utf8( <ls_data>-value ) ).

      lv_text = explain_content( <ls_data> ).
      REPLACE '<strong>' IN lv_text WITH ''.
      REPLACE '</strong>' IN lv_text WITH ''.
      lv_text = |{ <ls_data>-keys },{ lv_text }\n|.
      INSERT lv_text INTO TABLE lt_toc.
    ENDLOOP.

    lo_zip->add(
      name    = c_toc_filename
      content = zcl_abapgit_convert=>string_to_xstring_utf8( concat_lines_of( lt_toc ) ) ).

    lv_zip = lo_zip->save( ).

    CONCATENATE 'apm_Backup_' sy-datlo '_' sy-timlo '.zip' INTO lv_filename.

    li_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_save_dialog(
      iv_title            = 'apm Backup'
      iv_extension        = 'zip'
      iv_default_filename = lv_filename ).

    li_fe_serv->file_download(
      iv_path = lv_path
      iv_xstr = lv_zip ).

    MESSAGE 'apm Backup successfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD do_delete_entry.

    DATA:
      lv_answer TYPE c LENGTH 1,
      lx_error  TYPE REF TO zcx_abappm_persist_apm.

    ASSERT iv_key IS NOT INITIAL.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = |Are you sure you want to delete entry { iv_key }?|
      iv_text_button_1         = 'Yes'
      iv_icon_button_1         = 'ICON_DELETE'
      iv_text_button_2         = 'No'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    TRY.
        gi_persist->delete( iv_key ).
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    COMMIT WORK.

    MESSAGE 'Entry successfully deleted' TYPE 'S'.

  ENDMETHOD.


  METHOD do_restore_db.

    DATA:
      lv_answer   TYPE c LENGTH 1,
      lo_zip      TYPE REF TO cl_abap_zip,
      lv_zip      TYPE xstring,
      lv_path     TYPE string,
      lv_key      TYPE zif_abappm_persist_apm=>ty_key,
      lv_data     TYPE xstring,
      lt_data     TYPE zif_abappm_persist_apm=>ty_list,
      lt_data_old TYPE zif_abappm_persist_apm=>ty_list,
      ls_data     TYPE zif_abappm_persist_apm=>ty_zabappm,
      li_fe_serv  TYPE REF TO zif_abapgit_frontend_services,
      lx_error    TYPE REF TO zcx_abappm_persist_apm.

    FIELD-SYMBOLS:
      <ls_zipfile> LIKE LINE OF lo_zip->files.

    li_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_open_dialog(
      iv_title            = 'Restore apm Backup'
      iv_extension        = 'zip'
      iv_default_filename = 'apm_Backup_*.zip' ).

    lv_zip = li_fe_serv->file_upload( lv_path ).

    CREATE OBJECT lo_zip.

    lo_zip->load(
      EXPORTING
        zip             = lv_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error loading ZIP file' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile> WHERE name <> c_toc_filename.
      CLEAR ls_data.
      lv_key = replace(
        val  = <ls_zipfile>-name
        sub  = '.json'
        with = '' ).

      " Validate DB key
      IF zcl_abappm_persist_apm=>validate_key( lv_key ) = abap_false.
        zcx_abapgit_exception=>raise( |Invalid DB entry type. This is not an apm Backup| ).
      ENDIF.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error getting file { <ls_zipfile>-name } from ZIP| ).
      ENDIF.

      ls_data-value = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).
      INSERT ls_data INTO TABLE lt_data.
    ENDLOOP.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = 'All existing packages and settings will be deleted and overwritten! Continue?'
      iv_text_button_1         = 'Restore'
      iv_icon_button_1         = 'ICON_IMPORT'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer <> '1'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    TRY.
        gi_persist->lock( ls_data-keys ).

        lt_data_old = gi_persist->list( ).
        LOOP AT lt_data_old INTO ls_data.
          gi_persist->delete( ls_data-keys ).
        ENDLOOP.

        COMMIT WORK.

        LOOP AT lt_data INTO ls_data.
          gi_persist->save(
            iv_key   = ls_data-keys
            iv_value = ls_data-value ).
        ENDLOOP.

        COMMIT WORK.
      CATCH zcx_abappm_persist_apm INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    MESSAGE 'apm Backup successfully restored' TYPE 'S'.

  ENDMETHOD.


  METHOD explain_content.

    DATA:
      lv_key_type    TYPE string,
      lv_name        TYPE string,
      lv_rest        TYPE string,
      lv_package     TYPE devclass,
      lv_user        TYPE uname,
      lv_descr       TYPE string,
      ls_explanation TYPE ty_explanation.

    SPLIT is_data-keys AT ':' INTO lv_key_type lv_name lv_rest.

    CASE lv_key_type.
      WHEN zif_persist_apm=>c_key_type-package.
        lv_descr   = 'Package'.
        lv_package = lv_name.
        ls_explanation-value = zcl_abapgit_factory=>get_sap_package( lv_package )->read_description( ).
        IF lv_rest = 'PACKAGE_JSON'.
          ls_explanation-extra = 'Package JSON'.
        ELSEIF lv_rest = 'README'.
          ls_explanation-extra = 'Readme'.
        ENDIF.

      WHEN zif_persist_apm=>c_key_type-settings.
        IF lv_name = zif_abappm_settings=>c_global.
          lv_descr = 'Global Settings'.
        ELSE.
          lv_descr = 'Personal Settings'.
          lv_user  = lv_name.
          ls_explanation-value = zcl_abapgit_user_record=>get_instance( lv_user )->get_name( ).
        ENDIF.

      WHEN OTHERS.
        IF strlen( is_data-value ) >= 250.
          ls_explanation-value = is_data-value(250).
        ELSE.
          ls_explanation-value = is_data-value.
        ENDIF.

        ls_explanation-value = escape(
          val    = ls_explanation-value
          format = cl_abap_format=>e_html_attr ).
        ls_explanation-value = |<pre>{ ls_explanation-value }</pre>|.

    ENDCASE.

    IF ls_explanation-value IS NOT INITIAL.
      lv_descr = |{ lv_descr }: |.
    ENDIF.

    IF ls_explanation-extra IS NOT INITIAL.
      ls_explanation-extra = | ({ ls_explanation-extra })|.
    ENDIF.

    rv_text = |{ lv_descr }<strong>{ ls_explanation-value }</strong>{ ls_explanation-extra }|.

    IF strlen( rv_text ) >= 250.
      rv_text = rv_text(250) && '...'.
    ENDIF.

  ENDMETHOD.


  METHOD register_stylesheet.

    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.

    CREATE OBJECT lo_buf.

    " @@abapmerge include zabapgit_css_page_db.w3mi.data.css > lo_buf->add( '$$' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.


  METHOD render_stats.

    DATA:
      lv_key_type TYPE string,
      lv_name     TYPE string,
      lv_suffix   TYPE string,
      lv_packages TYPE i,
      lv_users    TYPE i.

    FIELD-SYMBOLS <ls_db_entry> LIKE LINE OF it_db_entries.

    LOOP AT it_db_entries ASSIGNING <ls_db_entry>.
      SPLIT <ls_db_entry>-keys AT ':' INTO lv_key_type lv_name lv_suffix.

      CASE lv_key_type.
        WHEN zif_persist_apm=>c_key_type-package.
          IF lv_suffix = zcl_abappm_package_json=>c_package_json.
            lv_packages = lv_packages + 1.
          ENDIF.
        WHEN zif_persist_apm=>c_key_type-settings.
          IF lv_name <> zif_abappm_settings=>c_global.
            lv_users = lv_users + 1.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( |Packages: { lv_packages }, Users: { lv_users }| ).

  ENDMETHOD.


  METHOD render_table.

    ri_html = zcl_abapgit_html_table=>create( me
      )->define_column(
        iv_column_id = 'keys'
        iv_column_title = 'Key'
      )->define_column(
        iv_column_id = 'value'
        iv_column_title = 'Description'
      )->define_column(
        iv_column_id = 'luser'
        iv_column_title = 'Last Changed By'
      )->define_column(
        iv_column_id = 'timestamp'
        iv_column_title = 'Last Changed At'
      )->define_column( 'cmd'
      )->render( it_db_entries ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_key TYPE zif_abappm_persist_apm=>ty_key.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN c_action-db_display.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN c_action-db_edit.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create(
          iv_key       = lv_key
          iv_edit_mode = abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN c_action-delete.
        do_delete_entry( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-backup.
        do_backup_db( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-restore.
        do_restore_db( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Backup'
      iv_act = c_action-backup ).
    ro_toolbar->add(
      iv_txt = 'Restore'
      iv_act = c_action-restore ).
    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lt_db_entries TYPE zif_abappm_persist_apm=>ty_list.

    register_handlers( ).

    lt_db_entries = gi_persist->list( ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="db-list">' ).
    ri_html->add( render_stats( lt_db_entries ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div class="db-list">' ).
    ri_html->add( render_table( lt_db_entries ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.
  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.

    DATA:
      lv_action    TYPE string,
      lo_toolbar   TYPE REF TO zcl_abapgit_html_toolbar,
      lv_user      TYPE syuname,
      lv_timestamp TYPE timestampl.

    FIELD-SYMBOLS <lv_key> TYPE zif_abappm_persist_apm=>ty_key.

    CASE iv_column_id.
      WHEN 'keys'.
        rs_render-content = |{ iv_value }|.
      WHEN 'value'.
        rs_render-content   = explain_content( is_row ).
        rs_render-css_class = 'data'.
      WHEN 'luser'.
        lv_user             = iv_value.
        rs_render-content   = zcl_abapgit_gui_chunk_lib=>render_user_name( lv_user )->render( ).
      WHEN 'timestamp'.
        lv_timestamp        = iv_value.
        rs_render-content   = zcl_abapgit_gui_chunk_lib=>render_timestamp( lv_timestamp ).
        rs_render-css_class = 'data'.
      WHEN 'cmd'.
        ASSIGN COMPONENT 'KEYS' OF STRUCTURE is_row TO <lv_key>.
        lv_action  = |key={ cl_http_utility=>escape_url( |{ <lv_key> }| ) }|.
        lo_toolbar = zcl_abapgit_html_toolbar=>create(
          )->add(
            iv_txt = 'Display'
            iv_act = |{ c_action-db_display }?{ lv_action }|
          )->add(
            iv_txt = 'Edit'
            iv_act = |{ c_action-db_edit }?{ lv_action }|
          )->add(
            iv_txt = 'Delete'
            iv_act = |{ c_action-delete }?{ lv_action }| ).
        rs_render-html = lo_toolbar->render( ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
