CLASS /apmg/cl_apm_gui_router DEFINITION
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

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_router.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS general_page_routing
      IMPORTING
        !event        TYPE REF TO /apmg/if_apm_gui_event
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_gui_event_handler=>ty_handling_result
      RAISING
        /apmg/cx_apm_error.

    METHODS command_dialogs
      IMPORTING
        !event        TYPE REF TO /apmg/if_apm_gui_event
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_gui_event_handler=>ty_handling_result
      RAISING
        /apmg/cx_apm_error.

    METHODS utility_actions
      IMPORTING
        !event        TYPE REF TO /apmg/if_apm_gui_event
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_gui_event_handler=>ty_handling_result
      RAISING
        /apmg/cx_apm_error.

    METHODS sap_gui_actions
      IMPORTING
        !event        TYPE REF TO /apmg/if_apm_gui_event
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_gui_event_handler=>ty_handling_result
      RAISING
        /apmg/cx_apm_error.

    METHODS browser_actions
      IMPORTING
        !event        TYPE REF TO /apmg/if_apm_gui_event
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_gui_event_handler=>ty_handling_result
      RAISING
        /apmg/cx_apm_error.

    METHODS other_utilities
      IMPORTING
        !event        TYPE REF TO /apmg/if_apm_gui_event
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_gui_event_handler=>ty_handling_result
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS jump_object
      IMPORTING
        !obj_type   TYPE string
        !obj_name   TYPE string
        !filename   TYPE string
        !sub_type   TYPE string OPTIONAL
        !sub_name   TYPE string OPTIONAL
        !line       TYPE string OPTIONAL
        !new_window TYPE string DEFAULT 'X'
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS jump_display_transport
      IMPORTING
        !transport TYPE trkorr
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS jump_display_user
      IMPORTING
        !username TYPE syuname
      RAISING
        /apmg/cx_apm_error.

    METHODS call_browser
      IMPORTING
        !url TYPE csequence
      RAISING
        /apmg/cx_apm_error.

    METHODS main_page
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error ##CALLED.

    METHODS toggle_favorite
      IMPORTING
        !package TYPE csequence
      RAISING
        /apmg/cx_apm_error.
ENDCLASS.



CLASS /apmg/cl_apm_gui_router IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    DATA result TYPE /apmg/if_apm_gui_event_handler=>ty_handling_result.

    IF result-state IS INITIAL.
      result = general_page_routing( ii_event ).
    ENDIF.
    IF result-state IS INITIAL.
      result = command_dialogs( ii_event ).
    ENDIF.
    IF result-state IS INITIAL.
      result = utility_actions( ii_event ).
    ENDIF.
    IF result-state IS INITIAL.
      result = sap_gui_actions( ii_event ).
    ENDIF.
    IF result-state IS INITIAL.
      result = browser_actions( ii_event ).
    ENDIF.
    IF result-state IS INITIAL.
      result = other_utilities( ii_event ).
    ENDIF.

    IF result-state IS INITIAL.
      result-state = /apmg/cl_apm_gui=>c_event_state-not_handled.
    ENDIF.

    rs_handled = result.

  ENDMETHOD.


  METHOD browser_actions.

    CASE event->mv_action.
      WHEN /apmg/if_apm_gui_router=>c_action-homepage.

        call_browser( /apmg/if_apm_constants=>c_website ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-feedback.

        call_browser( /apmg/if_apm_constants=>c_new_issue ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-registry.
        TRY.
            DATA(registry) = /apmg/cl_apm_settings=>factory( )->get( )-registry.
            call_browser( registry ).
          CATCH /apmg/cx_apm_error ##NO_HANDLER.
        ENDTRY.
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-documentation.

        call_browser( /apmg/if_apm_constants=>c_documentation ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-changelog.

        call_browser( /apmg/if_apm_constants=>c_changelog ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-sponsor.

        call_browser( /apmg/if_apm_constants=>c_sponsor ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD call_browser.

    /apmg/cl_apm_gui_factory=>get_frontend_services( )->execute( iv_document = |{ url }| ).

  ENDMETHOD.


  METHOD command_dialogs.

    DATA(package) = CONV devclass( event->query( )->get( 'KEY' ) ).

    CASE event->mv_action.
      WHEN /apmg/if_apm_gui_router=>c_action-apm_init.

        result-page  = /apmg/cl_apm_gui_dlg_init=>create( ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN /apmg/if_apm_gui_router=>c_action-apm_install.

        result-page  = /apmg/cl_apm_gui_dlg_install=>create( ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN /apmg/if_apm_gui_router=>c_action-apm_uninstall.

        result-page  = /apmg/cl_apm_gui_dlg_uninstall=>create( package ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN /apmg/if_apm_gui_router=>c_action-apm_publish.

        result-page  = /apmg/cl_apm_gui_dlg_publish=>create( package ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN /apmg/if_apm_gui_router=>c_action-apm_unpublish.

        result-page  = /apmg/cl_apm_gui_dlg_unpublish=>create( package ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.


  METHOD general_page_routing.

    CASE event->mv_action.
      WHEN /apmg/if_apm_gui_router=>c_action-go_home.

        result-page  = /apmg/cl_apm_gui_page_list=>create( ). "TODO main_page( ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN /apmg/if_apm_gui_router=>c_action-go_back.

        result-state = /apmg/cl_apm_gui=>c_event_state-go_back.

      WHEN /apmg/if_apm_gui_router=>c_action-apm_home.

        result-page  = /apmg/cl_apm_gui_page_list=>create( ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page_replacing.

      WHEN /apmg/if_apm_gui_router=>c_action-go_settings.

        DATA(key) = /apmg/cl_apm_settings=>get_setting_key( /apmg/if_apm_settings=>c_global ).
        result-page  = /apmg/cl_apm_gui_page_db_entry=>create( key ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN /apmg/if_apm_gui_router=>c_action-go_settings_personal.

        key = /apmg/cl_apm_settings=>get_setting_key( sy-uname ).
        result-page  = /apmg/cl_apm_gui_page_db_entry=>create( key ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

* FUTURE
*      WHEN /apmg/if_apm_gui_router=>c_action-go_tutorial
*        result-page  = /apmg/cl_apm_gui_page_tutorial=>create( )
*        result-state = /apmg/cl_apm_gui=>c_event_state-new_page

      WHEN /apmg/if_apm_gui_router=>c_action-favorite_package.

        toggle_favorite( event->query( )->get( 'PACKAGE' ) ).
        result-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN /apmg/if_apm_gui_router=>c_action-show_hotkeys.

        /apmg/cl_apm_gui_factory=>get_gui_services( )->get_hotkeys_ctl( )->set_visible( abap_true ).
        result-state = /apmg/cl_apm_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD jump_display_transport.

    TRY.
        DATA(is_adt_jump_enabled) = /apmg/cl_apm_settings=>factory( )->get( )-gui_settings-adt_jump_enabled.
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
    ENDTRY.

    IF is_adt_jump_enabled = abap_true.
      TRY.
          DATA(adt_link) = zcl_abapgit_adt_link=>link_transport( transport ).
          /apmg/cl_apm_gui_factory=>get_frontend_services( )->execute( iv_document = adt_link ).
        CATCH /apmg/cx_apm_error.
          " Fallback if ADT link execution failed or was cancelled
          CALL FUNCTION 'TR_DISPLAY_REQUEST'
            EXPORTING
              i_trkorr = transport.
      ENDTRY.
    ELSE.
      CALL FUNCTION 'TR_DISPLAY_REQUEST'
        EXPORTING
          i_trkorr = transport.
    ENDIF.

  ENDMETHOD.


  METHOD jump_display_user.

    " TODO: user display in ADT

    CALL FUNCTION 'BAPI_USER_DISPLAY'
      EXPORTING
        username = username.

  ENDMETHOD.


  METHOD jump_object.

    DATA(item) = VALUE zif_abapgit_definitions=>ty_item(
      obj_type = cl_http_utility=>unescape_url( |{ obj_type }| )
      obj_name = cl_http_utility=>unescape_url( |{ obj_name }| ) ).

    DATA(sub_item) = VALUE zif_abapgit_definitions=>ty_item(
      obj_type = cl_http_utility=>unescape_url( |{ sub_type }| )
      obj_name = cl_http_utility=>unescape_url( |{ sub_name }| ) ).

    IF line CO '0123456789'.
      DATA(line_number) = CONV i( line ).
    ENDIF.
    DATA(is_new_window) = xsdbool( new_window IS NOT INITIAL ).

    TRY.
        DATA(html_viewer) = /apmg/cl_apm_gui_factory=>get_html_viewer( ).

        " Hide HTML Viewer in dummy screen0 for direct CALL SCREEN to work
        html_viewer->set_visiblity( abap_false ).

        IF line_number IS INITIAL OR sub_item IS INITIAL.
          /apmg/cl_apm_abapgit_objects=>jump(
            is_item       = item
            iv_filename   = filename
            iv_new_window = is_new_window ).
        ELSE.
          /apmg/cl_apm_abapgit_objects=>jump(
            is_item        = item
            is_sub_item    = sub_item
            iv_filename    = filename
            iv_line_number = line_number
            iv_new_window  = is_new_window ).
        ENDIF.

        html_viewer->set_visiblity( abap_true ).
      CATCH cx_root INTO DATA(error).
        html_viewer->set_visiblity( abap_true ).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD main_page ##CALLED.

    " Prio 1: Show last viewed package (if it exists)
    DATA(settings) = /apmg/cl_apm_settings=>factory( )->get( ).

    IF settings-show_last_package = abap_true AND settings-last_package IS NOT INITIAL.
      TRY.
          /apmg/cl_apm_package_json=>factory( settings-last_package )->load( ).

          result = /apmg/cl_apm_gui_page_package=>create( settings-last_package ).
          RETURN.
        CATCH /apmg/cx_apm_error.
          " Remove inconsistent value from settings
          CLEAR settings-last_package.
          /apmg/cl_apm_settings=>factory( )->set( settings )->save( ).
      ENDTRY.
    ENDIF.

    " Prio 2: Show list of packages
    DATA(package_list) = /apmg/cl_apm_package_json=>list( ).
    IF package_list IS NOT INITIAL.
      result = /apmg/cl_apm_gui_page_list=>create( ).
    ELSE.
      " TODO: Prio 3: Show tutorial
      " ri_page = /apmg/cl_apm_gui_page_tutorial=>create( )
      /apmg/cl_apm_roadmap=>planned( ).
    ENDIF.

  ENDMETHOD.


  METHOD other_utilities.

    TYPES ty_char TYPE c LENGTH 1024.

    DATA clipboard TYPE STANDARD TABLE OF ty_char WITH KEY table_line.

    CASE event->mv_action.
      WHEN /apmg/if_apm_gui_router=>c_action-ie_devtools.

        /apmg/cl_apm_gui_factory=>get_frontend_services( )->open_ie_devtools( ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-clipboard.

        DATA(clip_content) = event->query( )->get( 'CLIPBOARD' ).
        APPEND clip_content TO clipboard.

        /apmg/cl_apm_gui_factory=>get_frontend_services( )->clipboard_export( clipboard ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

        MESSAGE 'Successfully exported to clipboard' TYPE 'S'.

    ENDCASE.

  ENDMETHOD.


  METHOD sap_gui_actions.

    CASE event->mv_action.
      WHEN /apmg/if_apm_gui_router=>c_action-jump.                          " Open object editor

        jump_object(
          obj_type   = event->query( )->get( 'TYPE' )
          obj_name   = event->query( )->get( 'NAME' )
          filename   = event->query( )->get( 'FILE' )
          sub_type   = event->query( )->get( 'SUBTYPE' )
          sub_name   = event->query( )->get( 'SUBNAME' )
          line       = event->query( )->get( 'LINE' )
          new_window = event->query( )->get( 'NEW_WINDOW' ) ).

        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-jump_transport.

        jump_display_transport( |{ event->query( )->get( 'TRANSPORT' ) }| ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-jump_user.

        jump_display_user( |{ event->query( )->get( 'USER' ) }| ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

      WHEN /apmg/if_apm_gui_router=>c_action-url.

        call_browser( event->query( )->get( 'URL' ) ).
        result-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD toggle_favorite.

    TRY.
        DATA(settings) = /apmg/cl_apm_settings=>factory( )->get( ).

        READ TABLE settings-package_settings ASSIGNING FIELD-SYMBOL(<package>)
          WITH KEY package = package.
        IF sy-subrc = 0.
          <package>-favorite = xsdbool( <package>-favorite = abap_false ).
        ELSE.
          DATA(package_setting) = VALUE /apmg/if_apm_settings=>ty_package_settings(
            package  = package
            favorite = abap_true ).
          INSERT package_setting INTO TABLE settings-package_settings.
        ENDIF.

        /apmg/cl_apm_settings=>factory( )->set( settings )->save( ).
      CATCH /apmg/cx_apm_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD utility_actions.

    CASE event->mv_action.
      WHEN /apmg/if_apm_gui_router=>c_action-go_db.

        result-page  = /apmg/cl_apm_gui_page_db=>create( ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN /apmg/if_apm_gui_router=>c_action-go_debuginfo.

        result-page  = /apmg/cl_apm_gui_page_debuginf=>create( ).
        result-state = /apmg/cl_apm_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
