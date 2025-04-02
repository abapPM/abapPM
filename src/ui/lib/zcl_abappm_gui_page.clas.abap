CLASS zcl_abappm_gui_page DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  ABSTRACT
  CREATE PUBLIC.

************************************************************************
* apm GUI Page
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
* adapted: gui_component, settings, title, and footer
  PUBLIC SECTION.

    INTERFACES:
      zif_abappm_gui_modal,
      zif_abappm_gui_renderable,
      zif_abappm_gui_event_handler,
      zif_abappm_gui_error_handler.

    TYPES:
      BEGIN OF ty_control,
        page_layout         TYPE string,
        page_title          TYPE string,
        page_menu           TYPE REF TO zcl_abappm_html_toolbar,
        page_menu_provider  TYPE REF TO zif_abappm_gui_menu_provider,
        page_title_provider TYPE REF TO zif_abappm_gui_page_title,
        extra_css_url       TYPE string,
        extra_js_url        TYPE string,
        show_as_modal       TYPE abap_bool,
      END OF  ty_control.

    CONSTANTS:
      BEGIN OF c_page_layout,
        centered   TYPE string VALUE `centered`,
        full_width TYPE string VALUE `full_width`,
      END OF c_page_layout.

    METHODS constructor
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.

    DATA page_control TYPE ty_control.

    METHODS render_content " TODO refactor, render child directly
      ABSTRACT
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

  PRIVATE SECTION.

    DATA settings TYPE zif_abappm_settings=>ty_settings. " apm
    DATA error TYPE REF TO zcx_abappm_error.
    DATA exception_viewer TYPE REF TO zcl_abappm_exception_viewer.

    METHODS render_deferred_parts
      IMPORTING
        !part_category TYPE string
      RETURNING
        VALUE(result)  TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS html_head
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html.

    METHODS header_stylesheet_links
      IMPORTING
        html TYPE REF TO zif_abappm_html.

    METHODS header_script_links
      IMPORTING
        html TYPE REF TO zif_abappm_html.

    METHODS title
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS footer
      IMPORTING
        !time         TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS render_link_hints
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS render_browser_control_warning
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS render_command_palettes
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS render_hotkey_overview
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS render_error_message_box
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS scripts
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_html
      RAISING
        zcx_abappm_error.

    METHODS get_version_details
      RETURNING
        VALUE(result) TYPE string.

    METHODS is_edge_control_warning_needed
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS zcl_abappm_gui_page IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    page_control-page_layout = c_page_layout-centered.
  ENDMETHOD.


  METHOD footer.
    DATA(html) = zcl_abappm_html=>create( ).

    html->add( '<div id="footer">' ).
    html->add( '<table class="w100"><tr>' ).

    html->add( '<td class="w40 sponsor">' ).
    html->add_a( iv_act = zif_abapgit_definitions=>c_action-sponsor
                 iv_txt = html->icon( iv_name = 'heart-regular/pink'
                                      iv_hint = 'Sponsor us' ) ).
    html->add_a( iv_act = zif_abapgit_definitions=>c_action-sponsor
                 iv_txt = 'Sponsor us' ).
    html->add( '</td>' ).

    html->add( '<td class="center">' ).
    html->add( '<div class="logo" style="text-decoration:none">' ).
    html->add_a( iv_act   = zif_abapgit_definitions=>c_action-homepage
                 iv_title = time
                 iv_txt   = zcl_abappm_logo=>svg_logo_with_text( 20 ) ). " apm
    html->add( '</div>' ).
    html->add( |<div id="footer-version" class="version">{ get_version_details( ) }</div>| ).
    html->add( '</td>' ).

    html->add( '<td id="debug-output" class="w40"></td>' ).

    html->add( '</tr></table>' ).
    html->add( '</div>' ).

    result = html.
  ENDMETHOD.


  METHOD get_version_details.

    result = zif_abappm_version=>c_version. " apm

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      result = result && ` - Standalone Version`.
    ELSE.
      result = result && ` - Developer Version`.
    ENDIF.

    DATA(frontend_services) = zcl_abappm_gui_factory=>get_frontend_services( ).

    CASE abap_true.
      WHEN frontend_services->is_webgui( ).
        result = result && ` - Web`.
      WHEN frontend_services->is_sapgui_for_windows( ).
        result = result && ` - Win`.
      WHEN frontend_services->is_sapgui_for_java( ).
        result = result && ` - Java`.
      WHEN OTHERS.
        " eg. open-abap?
        result = result && ` - Unknown`.
    ENDCASE.

    " Will be filled by JS method displayBrowserControlFooter
    result = result && '<span id="browser-control-footer"></span>'.

  ENDMETHOD.


  METHOD header_script_links.

    html->add( '<script src="js/common.js"></script>' ).

    IF page_control-extra_js_url IS NOT INITIAL.
      html->add( |<script src="{ page_control-extra_js_url }"></script>| ).
    ENDIF.

  ENDMETHOD.


  METHOD header_stylesheet_links.

    html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    html->add( '<link rel="stylesheet" type="text/css" href="css/ag-icons.css">' ).

    " Themes
    html->add( '<link rel="stylesheet" type="text/css" href="css/theme-default.css">' ). " Theme basis
    CASE settings-gui_settings-ui_theme.
      WHEN zcl_abapgit_settings=>c_ui_theme-dark.
        html->add( '<link rel="stylesheet" type="text/css" href="css/theme-dark.css">' ).
      WHEN zcl_abapgit_settings=>c_ui_theme-belize.
        html->add( '<link rel="stylesheet" type="text/css" href="css/theme-belize-blue.css">' ).
    ENDCASE.

    " Page stylesheets
    IF page_control-extra_css_url IS NOT INITIAL.
      html->add( |<link rel="stylesheet" type="text/css" href="{ page_control-extra_css_url }">| ).
    ENDIF.

  ENDMETHOD.


  METHOD html_head.

    DATA(html) = zcl_abappm_html=>create( ).

    html->add( '<head>' ).

    html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ).
    html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ).

    html->add( '<title>abapGit</title>' ).

    header_stylesheet_links( html ).
    header_script_links( html ).

    " Overwrite the automatic icon scaling done in zcl_abappm_html=>icon
    CASE settings-gui_settings-icon_scaling.
      WHEN 'large'. "mo_settings->c_icon_scaling-large.
        html->add( '<style>.icon { font-size: 200% }</style>' ).
      WHEN 'small'. "mo_settings->c_icon_scaling-small.
        html->add( '<style>.icon.large { font-size: inherit }</style>' ).
    ENDCASE.

    html->add( '</head>' ).

    result = html.

  ENDMETHOD.


  METHOD is_edge_control_warning_needed.

    DATA:
      gui_release TYPE zif_abappm_frontend_services=>ty_gui_release,
      gui_sp      TYPE zif_abappm_frontend_services=>ty_gui_sp,
      gui_patch   TYPE zif_abappm_frontend_services=>ty_gui_patch.

    " With SAP GUI 8.00 PL3 and 7.70 PL13 Edge browser control is basically working.
    " For lower releases we render the browser control warning
    " and toggle it via JS function toggleBrowserControlWarning.

    result = abap_true.

    TRY.
        DATA(frontend_services) = zcl_abappm_gui_factory=>get_frontend_services( ).

        frontend_services->get_gui_version(
          IMPORTING
            ev_gui_release        = gui_release
            ev_gui_sp             = gui_sp
            ev_gui_patch          = gui_patch ).
      CATCH zcx_abappm_error.
        RETURN.
    ENDTRY.

    IF gui_release >= '7700' AND gui_sp >= '1' AND gui_patch >= '13'
    OR gui_release >= '8000' AND gui_sp >= '1' AND gui_patch >= '3'.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD render_browser_control_warning.
    DATA(html) = zcl_abappm_html=>create( ).

    DATA(link) = zcl_abappm_html=>create( )->add_a(
      iv_txt = 'Documentation'
      iv_typ = zif_abappm_html=>c_action_type-url
      iv_act = 'https://docs.abapgit.org/guide-sapgui.html#sap-gui-for-windows' ).

    html->add( '<div id="browser-control-warning" class="browser-control-warning">' ).
    html->add( zcl_abappm_gui_chunk_lib=>render_warning_banner(
                    |Attention: You use Edge browser control. |
                 && |There are several known malfunctions. See |
                 && link->render( ) ) ).
    html->add( '</div>' ).

    result = html.
  ENDMETHOD.


  METHOD render_command_palettes.
    DATA(html) = zcl_abappm_html=>create( ).

    html->add( 'var gCommandPalette = new CommandPalette(enumerateUiActions, {' ).
    html->add( '  toggleKey: "F1",' ).
    html->add( '  hotkeyDescription: "Command Palette"' ).
    html->add( '});' ).

    result = html.
  ENDMETHOD.


  METHOD render_deferred_parts.
    DATA(html) = zcl_abappm_html=>create( ).

    DATA(parts) = gui_services( )->get_html_parts( )->get_parts( part_category ).
    LOOP AT parts INTO DATA(part).
      html->add( part ).
    ENDLOOP.

    result = html.
  ENDMETHOD.


  METHOD render_error_message_box.

    " You should remember that the we have to instantiate ro_html even
    " it's overwritten further down. Because ADD checks whether it's
    " bound.
    result = zcl_abappm_html=>create( ).

    " You should remember that we render the message panel only
    " if we have an error.
    IF error IS NOT BOUND.
      RETURN.
    ENDIF.

    result = zcl_abappm_gui_chunk_lib=>render_error_message_box( error ).

    " You should remember that the exception viewer dispatches the events of
    " error message panel
    exception_viewer = NEW #( error ).

    " You should remember that we render the message panel just once
    " for each exception/error text.
    CLEAR error.

  ENDMETHOD.


  METHOD render_hotkey_overview.
    DATA hotkeys_component TYPE REF TO zif_abappm_gui_renderable.

    hotkeys_component ?= gui_services( )->get_hotkeys_ctl( ). " Mmmm...
    result = hotkeys_component->render( ).
  ENDMETHOD.


  METHOD render_link_hints.
    DATA(html) = zcl_abappm_html=>create( ).
    DATA(link_hint_key) = settings-keyboard_settings-link_hint_key.

    IF settings-keyboard_settings-link_hints_enabled = abap_true AND link_hint_key IS NOT INITIAL.
      html->add( |activateLinkHints("{ link_hint_key }");| ).
      html->add( |setInitialFocusWithQuerySelector('#header', false);| ).
      html->add( |enableArrowListNavigation();| ).
    ENDIF.

    result = html.
  ENDMETHOD.


  METHOD scripts.
    DATA(html) = zcl_abappm_html=>create( ).

    html->add( render_deferred_parts( c_html_parts-scripts ) ).
    html->add( render_link_hints( ) ).
    html->add( render_command_palettes( ) ).

    html->add( |toggleBrowserControlWarning();| ).
    html->add( |displayBrowserControlFooter();| ).

    result = html.
  ENDMETHOD.


  METHOD title.
    DATA(page_menu) = page_control-page_menu.
    IF page_menu IS NOT BOUND AND page_control-page_menu_provider IS BOUND.
      page_menu = page_control-page_menu_provider->get_menu( ).
    ENDIF.

    DATA(page_title) = page_control-page_title.
    IF page_control-page_title_provider IS BOUND.
      page_title = page_control-page_title_provider->get_page_title( ).
    ENDIF.

    DATA(html) = zcl_abappm_html=>create( ).

    html->add( '<div id="header">' ).

    html->add( '<div class="logo">' ).
    html->add_a(
      iv_act = zif_abappm_gui_router=>c_action-apm_home
      iv_txt = zcl_abappm_logo=>svg_logo_with_text( 28 ) ).
    html->add( '</div>' ).

    " TODO: add inline-style to page-title
    html->add( '<div class="page-title" style="vertical-align:top">' ).
    html->add( |<span class="spacer">&#x25BA;</span>{ page_title }| ).
    html->add( '</div>' ).

    IF page_menu IS BOUND.
      html->add( '<div class="float-right">' ).
      html->add( page_menu->render( iv_right = abap_true ) ).
      html->add( '</div>' ).
    ENDIF.

    IF is_edge_control_warning_needed( ) = abap_true.
      html->add( render_browser_control_warning( ) ).
    ENDIF.

    html->add( '</div>' ).

    result = html.
  ENDMETHOD.


  METHOD zif_abappm_gui_error_handler~handle_error.
    error = ix_error.
    rv_handled = abap_true.
  ENDMETHOD.


  METHOD zif_abappm_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-goto_source.

        IF exception_viewer IS BOUND.
          exception_viewer->goto_source( ).
        ENDIF.
        rs_handled-state = zcl_abappm_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-show_callstack.

        IF exception_viewer IS BOUND.
          exception_viewer->show_callstack( ).
        ENDIF.
        rs_handled-state = zcl_abappm_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-goto_message.

        IF exception_viewer IS BOUND.
          exception_viewer->goto_message( ).
        ENDIF.
        rs_handled-state = zcl_abappm_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abappm_gui_modal~is_modal.

    rv_yes = xsdbool( page_control-show_as_modal = abap_true ).

  ENDMETHOD.


  METHOD zif_abappm_gui_renderable~render.
    register_handlers( ).

    " Real page
    DATA(html) = zcl_abappm_html=>create( ).

    html->add( '<!DOCTYPE html>' ).
    html->add( '<html lang="en">' ).
    html->add( html_head( ) ).
    html->add( |<body class="{ page_control-page_layout }">| ).

    html->add( title( ) ).

    html->add( '<div class="not_sticky">' ).

    DATA(timer) = zcl_abapgit_timer=>create( )->start( ).

    html->add( render_content( ) ). " TODO -> render child

    DATA(render_content_time) = timer->end( ).

    html->add( render_hotkey_overview( ) ).
    html->add( render_error_message_box( ) ).
    html->add( render_deferred_parts( c_html_parts-hidden_forms ) ).

    html->add( footer( render_content_time ) ).

    html->add( '</div>' ).

    DATA(scripts) = scripts( ).

    IF scripts IS BOUND AND scripts->is_empty( ) = abap_false.
      html->add( '<script>' ).
      html->add( scripts ).
      html->add( 'confirmInitialized();' ).
      html->add( '</script>' ).
    ENDIF.

    html->add( '</body>' ).
    html->add( '</html>' ).

    ri_html = html.
  ENDMETHOD.
ENDCLASS.
