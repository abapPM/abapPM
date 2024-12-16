CLASS zcl_abappm_gui_menus DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Menus
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS advanced
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS help
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS back
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS settings
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS experimental
      IMPORTING
        !menu TYPE REF TO zcl_abapgit_html_toolbar.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_gui_menus IMPLEMENTATION.


  METHOD advanced.
    result = zcl_abapgit_html_toolbar=>create( 'toolbar-advanced' ).

    result->add(
      iv_txt = 'Database Utility'
      iv_act = zif_abapgit_definitions=>c_action-go_db
    )->add(
      iv_txt = 'Debug Info'
      iv_act = zif_abapgit_definitions=>c_action-go_debuginfo ).

    IF zcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_windows( ) = abap_true.
      result->add(
        iv_txt = 'Open IE DevTools'
        iv_act = zif_abapgit_definitions=>c_action-ie_devtools ).
    ENDIF.
  ENDMETHOD.


  METHOD back.
    result = zcl_abapgit_html_toolbar=>create( 'toolbar-back' ).

    result->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).
  ENDMETHOD.


  METHOD experimental.
    TRY.
        IF zcl_abappm_settings=>factory( )->get( )-experimental_features IS NOT INITIAL. "apm
          menu->add(
            iv_txt = zcl_abappm_gui_buttons=>experimental( )
            iv_act = zif_abappm_gui_router=>c_action-go_settings ).
        ENDIF.
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD help.
    result = zcl_abapgit_html_toolbar=>create( 'toolbar-help' ).

    result->add(
      iv_txt = 'Registry'
      iv_act = zif_abappm_gui_router=>c_action-registry
* FUTURE
*    )->add(
*      iv_txt = 'Tutorial'
*      iv_act = zif_abappm_gui_router=>c_action-go_tutorial
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
    result = zcl_abapgit_html_toolbar=>create( 'toolbar-settings' ).

    result->add(
      iv_txt = 'Global'
      iv_act = zif_abappm_gui_router=>c_action-go_settings
    )->add(
      iv_txt = 'Personal'
      iv_act = zif_abappm_gui_router=>c_action-go_settings_personal ).
  ENDMETHOD.
ENDCLASS.
