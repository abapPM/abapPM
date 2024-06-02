CLASS zcl_abappm_gui_page_debuginfo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Debug Info
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        save TYPE string VALUE 'save',
      END OF c_action.

    DATA mv_html TYPE string.

    CLASS-METHODS build_toolbar
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

    METHODS render_debug_info
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_page_debuginfo IMPLEMENTATION.


  METHOD build_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-debug'.

    ro_menu->add(
      iv_txt = 'Save'
      iv_act = c_action-save ).
    ro_menu->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_page_debuginfo.

    CREATE OBJECT lo_component.

    ri_page = zcl_abappm_gui_page_hoc=>create(
      iv_page_title      = 'Debug Info'
      io_page_menu       = build_toolbar( )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD render_debug_info.

    DATA: ls_release       TYPE zif_abapgit_environment=>ty_release_sp,
          lv_gui_version   TYPE string,
          lv_devclass      TYPE devclass,
          lo_frontend_serv TYPE REF TO zif_abapgit_frontend_services.

    lo_frontend_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).
    TRY.
        lo_frontend_serv->get_gui_version( IMPORTING ev_gui_version_string = lv_gui_version ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
        " Continue rendering even if this fails
    ENDTRY.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      ri_html->add( '<h2>apm - Standalone Version</h2>' ).
      ri_html->add( '<div>To keep apm up-to-date (or also to contribute) you need to' ).
      ri_html->add( |install it as a repository ({ ri_html->a(
        iv_txt = 'Developer Version'
        iv_act = zif_abappm_constants=>c_repository
        iv_typ = zif_abapgit_html=>c_action_type-url ) }).</div>| ).
    ELSE.
      lv_devclass = zcl_abapgit_factory=>get_tadir( )->get_object_package(
        iv_object   = 'CLAS'
        iv_obj_name = 'ZCX_ABAPPM_EXCEPTION' ).
      ri_html->add( '<h2>apm - Developer Version</h2>' ).
      ri_html->add( |<div>apm is installed in package { lv_devclass }</div>| ).
    ENDIF.

    ri_html->add( '<br><div>' ).
    ri_html->add_a(
      iv_txt = 'Contribution guidelines for apm'
      iv_act = |{ zif_abapgit_definitions=>c_action-url
        }?url={ zif_abappm_constants=>c_repository }/blob/main/CONTRIBUTING.md|
        iv_class = |url| ).
    ri_html->add( '</div>' ).

    ls_release = zcl_abapgit_factory=>get_environment( )->get_basis_release( ).

    ri_html->add( '<h2>Environment</h2>' ).

    ri_html->add( |<table>| ).
    ri_html->add( |<tr><td>apm version:    </td><td>{ zif_abappm_constants=>c_version }</td></tr>| ).
    ri_html->add( |<tr><td>GUI version:    </td><td>{ lv_gui_version }</td></tr>| ).
    ri_html->add( |<tr><td>SY time:        </td><td>{ sy-datum } { sy-uzeit } { sy-tzone }</td></tr>| ).
    ri_html->add( |<tr><td>SY release:     </td><td>{ ls_release-release } SP { ls_release-sp }</td></tr>| ).
    ri_html->add( |</table>| ).
    ri_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'debugOutput("<table><tr><td>Browser:</td><td>" + navigator.userAgent + ' &&
      '"</td></tr><tr><td>Frontend time:</td><td>" + new Date() + "</td></tr></table>", "debug_info");' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_path     TYPE string,
      lv_filename TYPE string,
      li_fe_serv  TYPE REF TO zif_abapgit_frontend_services.

    CASE ii_event->mv_action.
      WHEN c_action-save.

        CONCATENATE 'apm_Debug_Info_' sy-datlo '_' sy-timlo '.html' INTO lv_filename.

        li_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

        lv_path = li_fe_serv->show_file_save_dialog(
          iv_title            = 'apm - Debug Info'
          iv_extension        = 'html'
          iv_default_filename = lv_filename ).

        li_fe_serv->file_download(
          iv_path = lv_path
          iv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( mv_html ) ).

        MESSAGE 'apm debug info successfully saved' TYPE 'S'.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="debug_info" class="debug_container">' ).
    ri_html->add( render_debug_info( ) ).
    ri_html->add( '</div>' ).

    mv_html = '<!DOCTYPE html><html lang="en"><title>apm - Debug Info</title></head>'.
    mv_html = |<body>{ ri_html->render( ) }</body></html>|.

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
ENDCLASS.
