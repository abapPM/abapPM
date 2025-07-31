CLASS /apmg/cl_apm_gui_page_debuginf DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Debug Info
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_menu_provider,
      /apmg/if_apm_gui_renderable.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        save TYPE string VALUE 'save',
      END OF c_action.

    DATA html_for_download TYPE string.

    METHODS render_debug_info
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS get_scripts
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_page_debuginf IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-save.

        DATA(filename) = |apm_Debug_Info_{ sy-datlo }_{ sy-timlo }.html|.

        DATA(frontend_services) = /apmg/cl_apm_gui_factory=>get_frontend_services( ).

        DATA(path) = frontend_services->show_file_save_dialog(
          iv_title            = 'apm - Debug Info'
          iv_extension        = 'html'
          iv_default_filename = filename ).

        TRY.
            DATA(content) = zcl_abapgit_convert=>string_to_xstring_utf8( html_for_download ).
          CATCH zcx_abapgit_exception INTO DATA(error).
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
        ENDTRY.

        frontend_services->file_download(
          iv_path = path
          iv_xstr = content ).

        MESSAGE 'apm debug info successfully saved' TYPE 'S'.

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_menu_provider~get_menu.

    DATA(toolbar) = /apmg/cl_apm_html_toolbar=>create( 'apm-debug-info' ).

    toolbar->add(
      iv_txt = 'Save'
      iv_act = c_action-save ).
    toolbar->add(
      iv_txt = 'Back'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_back ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->add( '<div id="debug_info" class="debug_container">' ).
    render_debug_info( html ).
    html->add( '</div>' ).

    html_for_download =
      |<!DOCTYPE html>\n| &&
      |<html lang="en">\n| &&
      |  <title>apm - Debug Info</title>\n| &&
      |</head>\n| &&
      |<body>\n| &&
      |  { html->render( ) }\n| &&
      |</body>\n| &&
      |</html>|.

    register_deferred_script( get_scripts( ) ).

    ri_html = html.

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_page_debuginf( ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title         = 'Debug Info'
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD get_scripts.

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    html->add( 'debugOutput("<table><tr><td>Browser:</td><td>" + navigator.userAgent + ' &&
      '"</td></tr><tr><td>Frontend time:</td><td>" + new Date() + "</td></tr></table>", "debug_info");' ).

    result = html.

  ENDMETHOD.


  METHOD render_debug_info.

    DATA gui_version TYPE string.

    DATA(frontend_service) = /apmg/cl_apm_gui_factory=>get_frontend_services( ).

    TRY.
        frontend_service->get_gui_version( IMPORTING ev_gui_version_string = gui_version ).
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
        " Continue rendering even if this fails
    ENDTRY.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      html->add( '<h2>apm - Standalone Version</h2>' ).
      html->add( '<div>To keep apm up-to-date (or also to contribute) you need to' ).
      html->add( |install it as a repository ({ html->a(
        iv_txt = 'Developer Version'
        iv_act = /apmg/if_apm_constants=>c_repository
        iv_typ = /apmg/if_apm_html=>c_action_type-url ) }).</div>| ).
    ELSE.
      TRY.
          DATA(package) = zcl_abapgit_factory=>get_tadir( )->get_object_package(
            iv_object   = 'PROG'
            iv_obj_name = '/APMG/APM' ).
        CATCH cx_root ##NO_HANDLER.
          package = 'UNKNOWN'.
      ENDTRY.
      html->add( '<h2>apm - Developer Version</h2>' ).
      html->add( |<div>apm is installed in package { package }</div>| ).
    ENDIF.

    data(action) = |{ /apmg/if_apm_gui_router=>c_action-url }?url={
                      /apmg/if_apm_constants=>c_repository }/blob/main/CONTRIBUTING.md|.

    html->add( '<br><div>' ).
    html->add_a(
      iv_txt = 'Contribution guidelines for apm'
      iv_act = action
      iv_class = |url| ).
    html->add( '</div>' ).

    DATA(release) = zcl_abapgit_factory=>get_environment( )->get_basis_release( ).

    html->add( '<h2>Environment</h2>' ).

    html->add( |<table>| ).
    html->add( |<tr><td>apm version:    </td><td>{ /apmg/if_apm_version=>c_version }</td></tr>| ).
    html->add( |<tr><td>GUI version:    </td><td>{ gui_version }</td></tr>| ).
    html->add( |<tr><td>SY time:        </td><td>{ sy-datum } { sy-uzeit } { sy-tzone }</td></tr>| ).
    html->add( |<tr><td>SY release:     </td><td>{ release-release } SP { release-sp }</td></tr>| ).
    html->add( |</table>| ).
    html->add( |<br>| ).

    " TODO: List support objects for bundling and IMPORT (see package /APMG/APM_OBJECTS)

  ENDMETHOD.
ENDCLASS.
