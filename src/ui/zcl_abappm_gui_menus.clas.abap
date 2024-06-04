CLASS zcl_abappm_gui_menus DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Menus
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS advanced
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS help
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS back
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS settings
      IMPORTING
        !iv_act        TYPE string
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS experimental
      IMPORTING
        !io_menu TYPE REF TO zcl_abapgit_html_toolbar.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_gui_menus IMPLEMENTATION.


  METHOD advanced.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-advanced'.

    ro_menu->add(
      iv_txt = 'Database Utility'
      iv_act = zif_abapgit_definitions=>c_action-go_db
    )->add(
      iv_txt = 'Debug Info'
      iv_act = zif_abapgit_definitions=>c_action-go_debuginfo ).

    IF zcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_windows( ) = abap_true.
      ro_menu->add(
        iv_txt = 'Open IE DevTools'
        iv_act = zif_abapgit_definitions=>c_action-ie_devtools ).
    ENDIF.

  ENDMETHOD.


  METHOD back.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-back'.

    ro_menu->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD experimental.

    IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) IS NOT INITIAL.
      io_menu->add(
        iv_txt = zcl_abappm_gui_buttons=>experimental( )
        iv_act = zif_abappm_gui_router=>c_action-go_settings ).
    ENDIF.

  ENDMETHOD.


  METHOD help.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-help'.

    ro_menu->add(
      iv_txt = 'Registry'
      iv_act = zif_abappm_gui_router=>c_action-registry
    )->add(
      iv_txt = 'Tutorial'
      iv_act = zif_abappm_gui_router=>c_action-go_tutorial
    )->add(
      iv_txt = 'Documentation'
      iv_act = zif_abappm_gui_router=>c_action-documentation
    )->add(
      iv_txt = 'Changelog'
      iv_act = zif_abappm_gui_router=>c_action-changelog
    )->add(
      iv_txt = 'Hotkeys'
      iv_act = zif_abappm_gui_router=>c_action-show_hotkeys ).

  ENDMETHOD.


  METHOD settings.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-settings'.

    ro_menu->add(
      iv_txt = 'Global'
      iv_act = zif_abappm_gui_router=>c_action-go_settings
      iv_cur = boolc( iv_act = zif_abappm_gui_router=>c_action-go_settings )
    )->add(
      iv_txt = 'Personal'
      iv_act = zif_abappm_gui_router=>c_action-go_settings_personal
      iv_cur = boolc( iv_act = zif_abappm_gui_router=>c_action-go_settings_personal ) ).

  ENDMETHOD.
ENDCLASS.
