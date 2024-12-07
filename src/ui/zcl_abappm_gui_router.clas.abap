CLASS zcl_abappm_gui_router DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Router
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abappm_gui_router.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS general_page_routing
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS command_dialogs
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS utility_actions
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS sap_gui_actions
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS browser_actions
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS other_utilities
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS jump_object
      IMPORTING
        !iv_obj_type   TYPE string
        !iv_obj_name   TYPE string
        !iv_filename   TYPE string
        !iv_sub_type   TYPE string OPTIONAL
        !iv_sub_name   TYPE string OPTIONAL
        !iv_line       TYPE string OPTIONAL
        !iv_new_window TYPE string DEFAULT 'X'
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS jump_display_transport
      IMPORTING
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS jump_display_user
      IMPORTING
        !iv_username TYPE syuname
      RAISING
        zcx_abapgit_exception.

    METHODS call_browser
      IMPORTING
        !iv_url TYPE csequence
      RAISING
        zcx_abapgit_exception.

    METHODS get_state_settings
      IMPORTING
        !ii_event       TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rv_state) TYPE i.

    METHODS main_page
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS toggle_favorite
      IMPORTING
        !iv_package TYPE csequence
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_router IMPLEMENTATION.


  METHOD browser_actions.

    CASE ii_event->mv_action.
      WHEN zif_abappm_gui_router=>c_action-homepage.
        call_browser( zif_abappm_constants=>c_website ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abappm_gui_router=>c_action-registry.
        call_browser( zif_abappm_constants=>c_registry ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abappm_gui_router=>c_action-documentation.
        call_browser( zif_abappm_constants=>c_documentation ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abappm_gui_router=>c_action-changelog.
        call_browser( zif_abappm_constants=>c_changelog ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abappm_gui_router=>c_action-sponsor.
        call_browser( zif_abappm_constants=>c_sponsor ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.


  METHOD call_browser.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = |{ iv_url }| ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapgit_exception=>raise(
          iv_text     = 'Error opening page in external browser'
          ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD command_dialogs.

    CASE ii_event->mv_action.
      WHEN zif_abappm_gui_router=>c_action-apm_init.
        rs_handled-page  = zcl_abappm_gui_dlg_init=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abappm_gui_router=>c_action-apm_install.
        rs_handled-page  = zcl_abappm_gui_dlg_install=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abappm_gui_router=>c_action-apm_publish.
        rs_handled-page  = zcl_abappm_gui_dlg_publish=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abappm_gui_router=>c_action-apm_uninstall.
        rs_handled-page  = zcl_abappm_gui_dlg_uninstall=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDCASE.

  ENDMETHOD.


  METHOD general_page_routing.

    DATA lv_key TYPE zif_abappm_persist_apm=>ty_key.

    CASE ii_event->mv_action.
      WHEN zif_abappm_gui_router=>c_action-go_home.
        rs_handled-page  = zcl_abappm_gui_page_list=>create( ). "TODO main_page( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN zif_abappm_gui_router=>c_action-go_back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN zif_abappm_gui_router=>c_action-apm_home.
        rs_handled-page  = zcl_abappm_gui_page_list=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_replacing.

      WHEN zif_abappm_gui_router=>c_action-go_settings.
        lv_key = |{ zif_abappm_persist_apm=>c_key_type-settings }:{ zif_abappm_settings=>c_global }|.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abappm_gui_router=>c_action-go_settings_personal.
        lv_key = |{ zif_abappm_persist_apm=>c_key_type-settings }:{ sy-uname }|.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

* FUTURE
*      WHEN zif_abappm_gui_router=>c_action-go_tutorial.
*        rs_handled-page  = zcl_abapgit_gui_page_tutorial=>create( ).
*        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN zif_abappm_gui_router=>c_action-favorite_package.
        toggle_favorite( ii_event->query( )->get( 'PACKAGE' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abappm_gui_router=>c_action-show_hotkeys.
        zcl_abappm_gui_factory=>get_gui_services( )->get_hotkeys_ctl( )->set_visible( abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD get_state_settings.

    " Bookmark current page before jumping to any settings page
    IF ii_event->mv_current_page_name CP 'ZCL_ABAPPM_GUI_PAGE_SETT_*'.
      rv_state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
    ELSE.
      rv_state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
    ENDIF.

  ENDMETHOD.


  METHOD jump_display_transport.

    DATA:
      lv_adt_link         TYPE string,
      lv_adt_jump_enabled TYPE abap_bool.

    TRY.
        lv_adt_jump_enabled = zcl_abappm_settings=>factory( )->load( )->get( )-gui_settings-adt_jump_enabled.
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

    IF lv_adt_jump_enabled = abap_true.
      TRY.
          lv_adt_link = zcl_abapgit_adt_link=>link_transport( iv_transport ).
          zcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = lv_adt_link ).
        CATCH zcx_abapgit_exception.
          " Fallback if ADT link execution failed or was cancelled
          CALL FUNCTION 'TR_DISPLAY_REQUEST'
            EXPORTING
              i_trkorr = iv_transport.
      ENDTRY.
    ELSE.
      CALL FUNCTION 'TR_DISPLAY_REQUEST'
        EXPORTING
          i_trkorr = iv_transport.
    ENDIF.

  ENDMETHOD.


  METHOD jump_display_user.

    " todo, user display in ADT

    CALL FUNCTION 'BAPI_USER_DISPLAY'
      EXPORTING
        username = iv_username.

  ENDMETHOD.


  METHOD jump_object.

    DATA:
      ls_item        TYPE zif_abapgit_definitions=>ty_item,
      ls_sub_item    TYPE zif_abapgit_definitions=>ty_item,
      lx_error       TYPE REF TO zcx_abapgit_exception,
      lv_line_number TYPE i,
      lv_new_window  TYPE abap_bool,
      li_html_viewer TYPE REF TO zif_abapgit_html_viewer.

    ls_item-obj_type = cl_http_utility=>unescape_url( |{ iv_obj_type }| ).
    ls_item-obj_name = cl_http_utility=>unescape_url( |{ iv_obj_name }| ).
    ls_sub_item-obj_type = cl_http_utility=>unescape_url( |{ iv_sub_type }| ).
    ls_sub_item-obj_name = cl_http_utility=>unescape_url( |{ iv_sub_name }| ).

    IF iv_line CO '0123456789'.
      lv_line_number = iv_line.
    ENDIF.
    lv_new_window = boolc( iv_new_window IS NOT INITIAL ).

    TRY.
        li_html_viewer = zcl_abapgit_ui_factory=>get_html_viewer( ).

        " Hide HTML Viewer in dummy screen0 for direct CALL SCREEN to work
        li_html_viewer->set_visiblity( abap_false ).

        IF ls_item-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
          zcl_abapgit_data_utils=>jump( ls_item ).
        ELSEIF lv_line_number IS INITIAL OR ls_sub_item IS INITIAL.
          zcl_abapgit_objects=>jump(
            is_item       = ls_item
            iv_filename   = iv_filename
            iv_new_window = lv_new_window ).
        ELSE.
          zcl_abapgit_objects=>jump(
            is_item        = ls_item
            is_sub_item    = ls_sub_item
            iv_filename    = iv_filename
            iv_line_number = lv_line_number
            iv_new_window  = lv_new_window ).
        ENDIF.

        li_html_viewer->set_visiblity( abap_true ).
      CATCH zcx_abapgit_exception INTO lx_error.
        li_html_viewer->set_visiblity( abap_true ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD main_page.

    DATA:
      lx_error    TYPE REF TO zcx_abappm_error,
      lt_list     TYPE zif_abappm_package_json=>ty_packages,
      ls_settings TYPE zif_abappm_settings=>ty_settings.

    TRY.
        " Prio 1: Show last viewed package (if it exists)
        ls_settings = zcl_abappm_settings=>factory( )->get( ).

        IF ls_settings-show_last_package = abap_true AND ls_settings-last_package IS NOT INITIAL.
          TRY.
              zcl_abappm_package_json=>factory( ls_settings-last_package )->load(  ).

              ri_page = zcl_abappm_gui_page_package=>create( ls_settings-last_package ).
              RETURN.
            CATCH zcx_abappm_error.
              " Remove inconsistent value from settings
              CLEAR ls_settings-last_package.
              zcl_abappm_settings=>factory( )->set( ls_settings )->save( ).
          ENDTRY.
        ENDIF.

        " Prio 2: Show list of packages
        lt_list = zcl_abappm_package_json=>list( ).
        IF lt_list IS NOT INITIAL.
          ri_page = zcl_abappm_gui_page_list=>create( ).
        ELSE.
          " Prio 3: Show tutorial
          " ri_page = zcl_abapgit_gui_page_tutorial=>create( ). " TODO
          zcl_abappm_roadmap=>planned( ).
        ENDIF.

      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD other_utilities.

    TYPES ty_char600 TYPE c LENGTH 600.

    DATA lv_clip_content TYPE string.

    DATA lt_clipboard TYPE STANDARD TABLE OF ty_char600.

    CASE ii_event->mv_action.
      WHEN zif_abappm_gui_router=>c_action-ie_devtools.
        zcl_abapgit_ui_factory=>get_frontend_services( )->open_ie_devtools( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abappm_gui_router=>c_action-clipboard.
        lv_clip_content = ii_event->query( )->get( 'CLIPBOARD' ).
        APPEND lv_clip_content TO lt_clipboard.
        zcl_abapgit_ui_factory=>get_frontend_services( )->clipboard_export( lt_clipboard ).
        MESSAGE 'Successfully exported to clipboard' TYPE 'S'.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD sap_gui_actions.

    CASE ii_event->mv_action.
      WHEN zif_abappm_gui_router=>c_action-jump.                          " Open object editor
        jump_object(
          iv_obj_type   = ii_event->query( )->get( 'TYPE' )
          iv_obj_name   = ii_event->query( )->get( 'NAME' )
          iv_filename   = ii_event->query( )->get( 'FILE' )
          iv_sub_type   = ii_event->query( )->get( 'SUBTYPE' )
          iv_sub_name   = ii_event->query( )->get( 'SUBNAME' )
          iv_line       = ii_event->query( )->get( 'LINE' )
          iv_new_window = ii_event->query( )->get( 'NEW_WINDOW' ) ).

        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abappm_gui_router=>c_action-jump_transport.
        jump_display_transport( |{ ii_event->query( )->get( 'TRANSPORT' ) }| ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abappm_gui_router=>c_action-jump_user.
        jump_display_user( |{ ii_event->query( )->get( 'USER' ) }| ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abappm_gui_router=>c_action-url.
        call_browser( ii_event->query( )->get( 'URL' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD toggle_favorite.

    DATA:
      lx_error    TYPE REF TO zcx_abappm_error,
      ls_settings TYPE zif_abappm_settings=>ty_settings,
      ls_package  TYPE zif_abappm_settings=>ty_package_settings.

    FIELD-SYMBOLS <ls_package> TYPE zif_abappm_settings=>ty_package_settings.

    TRY.
        ls_settings = zcl_abappm_settings=>factory( )->load( )->get( ).

        READ TABLE ls_settings-package_settings ASSIGNING <ls_package> WITH KEY package = iv_package.
        IF sy-subrc = 0.
          <ls_package>-favorite = boolc( <ls_package>-favorite = abap_false ).
        ELSE.
          ls_package-package  = iv_package.
          ls_package-favorite = abap_true.
          INSERT ls_package INTO TABLE ls_settings-package_settings.
        ENDIF.

        zcl_abappm_settings=>factory( )->set( ls_settings )->save( ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD utility_actions.

    CASE ii_event->mv_action.
      WHEN zif_abappm_gui_router=>c_action-go_db.
        rs_handled-page  = zcl_abappm_gui_page_db=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abappm_gui_router=>c_action-go_debuginfo.
        rs_handled-page  = zcl_abappm_gui_page_debuginfo=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    IF rs_handled-state IS INITIAL.
      rs_handled = general_page_routing( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = command_dialogs( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = utility_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = sap_gui_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = browser_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = other_utilities( ii_event ).
    ENDIF.

    IF rs_handled-state IS INITIAL.
      rs_handled-state = zcl_abapgit_gui=>c_event_state-not_handled.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
