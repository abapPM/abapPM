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

    INTERFACES:
      zif_abappm_gui_event_handler,
      zif_abappm_gui_menu_provider,
      zif_abappm_gui_renderable,
      zif_abappm_html_table.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_gui_renderable
      RAISING
        zcx_abappm_error.

    METHODS constructor
      RAISING
        zcx_abappm_error.

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

    CONSTANTS:
      c_css_url      TYPE string VALUE 'css/page_db.css',
      c_toc_filename TYPE string VALUE '#_Table_of_Content_#.txt'.

    TYPES:
      BEGIN OF ty_list_item,
        key_type  TYPE string,
        key_name  TYPE string,
        key_extra TYPE string,
        show_key  TYPE string,
        keys      TYPE zif_abappm_persist_apm=>ty_key,
        value     TYPE zif_abappm_persist_apm=>ty_value,
        user      TYPE as4user,
        timestamp TYPE timestampl,
      END OF ty_list_item,
      ty_list TYPE SORTED TABLE OF ty_list_item WITH UNIQUE KEY key_type key_name key_extra.

    CLASS-DATA db_persist TYPE REF TO zif_abappm_persist_apm.

    DATA db_entries TYPE zif_abappm_persist_apm=>ty_list.

    DATA list TYPE ty_list.

    METHODS prepare_list.

    METHODS register_stylesheet
      RAISING
        zcx_abappm_error.

    METHODS render_stats
      IMPORTING
        html TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS render_table
      IMPORTING
        !html TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS do_backup_db
      RAISING
        zcx_abappm_error.

    CLASS-METHODS do_delete_entry
      IMPORTING
        !key TYPE zif_abappm_persist_apm=>ty_key
      RAISING
        zcx_abappm_error.

    CLASS-METHODS do_restore_db
      RAISING
        zcx_abappm_error.

    CLASS-METHODS explain_key
      IMPORTING
        !key          TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_abappm_gui_page_db IMPLEMENTATION.


  METHOD class_constructor.

    db_persist = zcl_abappm_persist_apm=>get_instance( ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    register_stylesheet( ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW zcl_abappm_gui_page_db( ).

    result = zcl_abappm_gui_page_hoc=>create(
      page_title         = 'Database Utility'
      extra_css_url      = c_css_url
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD do_backup_db.

    DATA table_of_contents TYPE string_table.

    DATA(db_entries) = db_persist->list( ).

    INSERT |Table of Content\n| INTO TABLE table_of_contents.
    INSERT |================\n| INTO TABLE table_of_contents.
    INSERT |\n| INTO TABLE table_of_contents.

    DATA(zip) = NEW cl_abap_zip( ).

    LOOP AT db_entries ASSIGNING FIELD-SYMBOL(<data>).
      DATA(filename) = to_lower( <data>-keys ) && '.json'.

      zip->add(
        name    = filename
        content = zcl_abappm_convert=>string_to_xstring_utf8( <data>-value ) ).

      INSERT explain_key( <data>-keys ) INTO TABLE table_of_contents.
    ENDLOOP.

    zip->add(
      name    = c_toc_filename
      content = zcl_abappm_convert=>string_to_xstring_utf8( concat_lines_of( table_of_contents ) ) ).

    DATA(zip_content) = zip->save( ).

    CONCATENATE 'apm_Backup_' sy-datlo '_' sy-timlo '.zip' INTO filename.

    DATA(frontend_service) = zcl_abappm_gui_factory=>get_frontend_services( ).

    DATA(path) = frontend_service->show_file_save_dialog(
      iv_title            = 'apm Backup'
      iv_extension        = 'zip'
      iv_default_filename = filename ).

    frontend_service->file_download(
      iv_path = path
      iv_xstr = zip_content ).

    MESSAGE 'apm Backup successfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD do_delete_entry.

    ASSERT key IS NOT INITIAL.

    DATA(answer) = zcl_abappm_gui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = |Are you sure you want to delete entry { key }?|
      iv_text_button_1         = 'Yes'
      iv_icon_button_1         = 'ICON_DELETE'
      iv_text_button_2         = 'No'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF answer = '2'.
      RAISE EXCEPTION TYPE zcx_abappm_cancel.
    ENDIF.

    db_persist->delete( key ).

    COMMIT WORK.

    MESSAGE 'Entry successfully deleted' TYPE 'S'.

  ENDMETHOD.


  METHOD do_restore_db.

    DATA:
      file_data      TYPE xstring,
      db_entries     TYPE zif_abappm_persist_apm=>ty_list,
      db_entries_old TYPE zif_abappm_persist_apm=>ty_list,
      db_entry       TYPE zif_abappm_persist_apm=>ty_list_item.

    DATA(frontend_service) = zcl_abappm_gui_factory=>get_frontend_services( ).

    DATA(path) = frontend_service->show_file_open_dialog(
      iv_title            = 'Restore apm Backup'
      iv_extension        = 'zip'
      iv_default_filename = 'apm_Backup_*.zip' ).

    DATA(zip_data) = frontend_service->file_upload( path ).

    DATA(zip) = NEW cl_abap_zip( ).

    zip->load(
      EXPORTING
        zip             = zip_data
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error loading ZIP file' ).
    ENDIF.

    LOOP AT zip->files ASSIGNING FIELD-SYMBOL(<file>) WHERE name <> c_toc_filename.
      CLEAR db_entry.
      DATA(key) = replace(
        val  = <file>-name
        sub  = '.json'
        with = '' ).

      " Validate DB key
      IF zcl_abappm_persist_apm=>validate_key( key ) = abap_false.
        zcx_abappm_error=>raise( |Invalid DB entry type. This is not an apm Backup| ).
      ENDIF.

      zip->get(
        EXPORTING
          name                    = <file>-name
        IMPORTING
          content                 = file_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( |Error getting file { <file>-name } from ZIP| ).
      ENDIF.

      db_entry-value = zcl_abappm_convert=>xstring_to_string_utf8( file_data ).
      INSERT db_entry INTO TABLE db_entries.
    ENDLOOP.

    DATA(answer) = zcl_abappm_gui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = 'All existing packages and settings will be deleted and overwritten! Continue?'
      iv_text_button_1         = 'Restore'
      iv_icon_button_1         = 'ICON_IMPORT'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF answer <> '1'.
      RAISE EXCEPTION TYPE zcx_abappm_cancel.
    ENDIF.

    db_persist->lock( db_entry-keys ).

    db_entries_old = db_persist->list( ).
    LOOP AT db_entries_old INTO db_entry.
      db_persist->delete( db_entry-keys ).
    ENDLOOP.

    COMMIT WORK.

    LOOP AT db_entries INTO db_entry.
      db_persist->save(
        key   = db_entry-keys
        value = db_entry-value ).
    ENDLOOP.

    COMMIT WORK.

    MESSAGE 'apm Backup successfully restored' TYPE 'S'.

  ENDMETHOD.


  METHOD explain_key.

    DATA(explained) = zcl_abappm_persist_apm=>explain_key( key ).

    result = |{ key } - { explained-key_type }: { explained-description } ({ explained-extra })\n|.

  ENDMETHOD.


  METHOD prepare_list.

    DATA sorted_list TYPE ty_list.

    CLEAR list.

    " Different field order for AT processing
    LOOP AT db_entries ASSIGNING FIELD-SYMBOL(<data>).
      DATA(list_entry) = CORRESPONDING ty_list_item( <data> ).
      INSERT list_entry INTO TABLE sorted_list.
    ENDLOOP.

    DATA(icon) = zcl_abappm_html=>icon( 'folder' ).

    LOOP AT sorted_list INTO list_entry.
      AT NEW key_type.
        list_entry-show_key = |{ icon } { list_entry-key_type }|.
        INSERT list_entry INTO TABLE list.
      ENDAT.
      AT NEW key_name.
        list_entry-show_key = |&nbsp;&nbsp;&nbsp;{ icon } { list_entry-key_name }|.
        INSERT list_entry INTO TABLE list.
      ENDAT.
      list_entry-show_key = |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{ to_lower( list_entry-key_extra ) }|.
      INSERT list_entry INTO TABLE list.
    ENDLOOP.

  ENDMETHOD.


  METHOD register_stylesheet.

    DATA(buffer) = NEW zcl_abapgit_string_buffer( ).

    " @@abapmerge include zabapgit_css_page_db.w3mi.data.css > buffer->add( '$$' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB'
      iv_inline    = buffer->join_w_newline_and_flush( ) ).

  ENDMETHOD.


  METHOD render_stats.

    DATA(package_count) = 0.
    DATA(user_count) = 0.

    LOOP AT db_entries ASSIGNING FIELD-SYMBOL(<db_entry>).
      CASE <db_entry>-key_type.
        WHEN zif_abappm_persist_apm=>c_key_type-package.
          IF <db_entry>-key_extra = zif_abappm_persist_apm=>c_key_extra-package_json.
            package_count = package_count + 1.
          ENDIF.
        WHEN zif_abappm_persist_apm=>c_key_type-settings.
          IF <db_entry>-key_name <> zif_abappm_persist_apm=>c_key_name-global_settings.
            user_count = user_count + 1.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    html->add( |Packages: { package_count }, Users: { user_count }| ).

  ENDMETHOD.


  METHOD render_table.

    html->add( zcl_abappm_html_table=>create( me
      )->define_column(
        iv_column_id    = 'show_key'
        iv_column_title = 'Key'
      )->define_column(
        iv_column_id    = 'value'
        iv_column_title = 'Description'
      )->define_column(
        iv_column_id    = 'user'
        iv_column_title = 'Last Changed By'
      )->define_column(
        iv_column_id    = 'timestamp'
        iv_column_title = 'Last Changed At'
      )->define_column(
        iv_column_id    = 'cmd'
        iv_column_title = 'Commands'
      )->render( list ) ).

  ENDMETHOD.


  METHOD zif_abappm_gui_event_handler~on_event.

    DATA(key) = CONV zif_abappm_persist_apm=>ty_key( ii_event->query( )->get( 'KEY' ) ).

    CASE ii_event->mv_action.
      WHEN c_action-db_display.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create(
          key          = key
          edit_mode    = abap_false ).
        rs_handled-state = zcl_abappm_gui=>c_event_state-new_page.
      WHEN c_action-db_edit.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create(
          key          = key
          edit_mode    = abap_true ).
        rs_handled-state = zcl_abappm_gui=>c_event_state-new_page.
      WHEN c_action-delete.
        do_delete_entry( key ).
        rs_handled-state = zcl_abappm_gui=>c_event_state-re_render.
      WHEN c_action-backup.
        do_backup_db( ).
        rs_handled-state = zcl_abappm_gui=>c_event_state-re_render.
      WHEN c_action-restore.
        do_restore_db( ).
        rs_handled-state = zcl_abappm_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abappm_gui_menu_provider~get_menu.

    DATA(toolbar) = zcl_abappm_html_toolbar=>create( 'apm-database-utility' ).

    toolbar->add(
      iv_txt = 'Backup'
      iv_act = c_action-backup
    )->add(
      iv_txt = 'Restore'
      iv_act = c_action-restore
    )->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD zif_abappm_gui_renderable~render.

    register_handlers( ).

    db_entries = db_persist->list( ).

    prepare_list( ).

    DATA(html) = zcl_abappm_html=>create( ).

    html->add( '<div class="db-list">' ).
    render_stats( html ).
    html->add( '</div>' ).

    html->add( '<div class="db-list">' ).
    render_table( html ).
    html->add( '</div>' ).

    ri_html = html.

  ENDMETHOD.


  METHOD zif_abappm_html_table~get_row_attrs.
  ENDMETHOD.


  METHOD zif_abappm_html_table~render_cell.

    ASSIGN COMPONENT 'KEYS' OF STRUCTURE is_row TO FIELD-SYMBOL(<key>).
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'KEY_TYPE' OF STRUCTURE is_row TO FIELD-SYMBOL(<key_type>).
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'KEY_NAME' OF STRUCTURE is_row TO FIELD-SYMBOL(<key_name>).
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'KEY_EXTRA' OF STRUCTURE is_row TO FIELD-SYMBOL(<key_extra>).
    ASSERT sy-subrc = 0.

    CASE iv_column_id.
      WHEN 'show_key'.
        rs_render-content = |{ iv_value }|.
      WHEN 'value'.
        IF <key_name> IS INITIAL AND <key_extra> IS INITIAL.
          rs_render-content =
            '<strong><i>' &&
            zcl_abappm_persist_apm=>explain_key( <key_type> )-key_type &&
            '</i></strong>'.
        ELSEIF <key_extra> IS INITIAL.
          rs_render-content =
            '<strong>' &&
            zcl_abappm_persist_apm=>explain_key( |{ <key_type> }:{ <key_name> }| )-description &&
            '</strong>'.
        ELSE.
          rs_render-content = zcl_abappm_persist_apm=>explain_key( <key> )-extra.
        ENDIF.
        rs_render-css_class = 'data'.
      WHEN 'user'.
        IF <key_extra> IS NOT INITIAL.
          DATA(user)          = CONV syuname( iv_value ).
          rs_render-content   = zcl_abappm_gui_chunk_lib=>render_user_name( user )->render( ).
        ENDIF.
      WHEN 'timestamp'.
        IF <key_extra> IS NOT INITIAL.
          DATA(timestamp)     = CONV timestampl( iv_value ).
          rs_render-content   = zcl_abappm_gui_chunk_lib=>render_timestamp( timestamp ).
          rs_render-css_class = 'data'.
        ENDIF.
      WHEN 'cmd'.
        IF <key_extra> IS NOT INITIAL.
          DATA(action)  = |key={ cl_http_utility=>escape_url( |{ <key> }| ) }|.
          DATA(toolbar) = zcl_abappm_html_toolbar=>create(
            )->add(
              iv_txt = 'Display'
              iv_act = |{ c_action-db_display }?{ action }|
            )->add(
              iv_txt = 'Edit'
              iv_act = |{ c_action-db_edit }?{ action }|
            )->add(
              iv_txt = 'Delete'
              iv_act = |{ c_action-delete }?{ action }| ).
          rs_render-html = toolbar->render( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
