CLASS /apmg/cl_apm_gui_menus DEFINITION
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
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_toolbar.

    CLASS-METHODS help
      IMPORTING
        with_welcome  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_toolbar.

    CLASS-METHODS back
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_toolbar.

    CLASS-METHODS settings
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_toolbar.

    CLASS-METHODS registry
      IMPORTING
        registry      TYPE string
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_toolbar.

    CLASS-METHODS experimental
      IMPORTING
        !menu TYPE REF TO /apmg/cl_apm_html_toolbar.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_gui_menus IMPLEMENTATION.


  METHOD advanced.

    result = /apmg/cl_apm_html_toolbar=>create( 'apm-toolbar-advanced' ).

    result->add(
      iv_txt = 'Package Tree'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_tree
    )->add(
      iv_txt = 'Database Utility'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_db
    )->add(
      iv_txt = 'Whoami'
      iv_act = /apmg/if_apm_gui_router=>c_action-apm_whoami
    )->add(
      iv_txt = 'Debug Info'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_debuginfo ).

    IF /apmg/cl_apm_gui_factory=>get_frontend_services( )->is_sapgui_for_windows( ) = abap_true.
      result->add(
        iv_txt = 'Open IE DevTools'
        iv_act = /apmg/if_apm_gui_router=>c_action-ie_devtools ).
    ENDIF.

  ENDMETHOD.


  METHOD back.

    result = /apmg/cl_apm_html_toolbar=>create( 'apm-toolbar-back' ).

    result->add(
      iv_txt = 'Back'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_back ).

  ENDMETHOD.


  METHOD experimental.

    TRY.
        IF /apmg/cl_apm_settings=>factory( )->get( )-experimental_features IS NOT INITIAL. "apm
          menu->add(
            iv_txt = /apmg/cl_apm_gui_buttons=>experimental( )
            iv_act = /apmg/if_apm_gui_router=>c_action-go_settings_global ).
        ENDIF.
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD help.

    result = /apmg/cl_apm_html_toolbar=>create( 'apm-toolbar-help' ).

    IF with_welcome = abap_true.
      result->add(
        iv_txt = 'Welcome'
        iv_act = /apmg/if_apm_gui_router=>c_action-go_welcome ).
    ENDIF.

    result->add(
      iv_txt   = 'Registry'
      iv_title = 'Global apm registry'
      iv_act   = /apmg/if_apm_gui_router=>c_action-registry
    )->add(
      iv_txt   = 'Playground'
      iv_title = 'Playground registry'
      iv_act   = /apmg/if_apm_gui_router=>c_action-playground
    )->add(
      iv_txt   = 'Tutorial'
      iv_title = 'Instructions for trying apm features'
      iv_act   = /apmg/if_apm_gui_router=>c_action-tutorial
    )->add(
      iv_txt   = 'Documentation'
      iv_title = 'Full documentation of apm registry and client'
      iv_act   = /apmg/if_apm_gui_router=>c_action-documentation
    )->add(
      iv_txt   = 'Changelog'
      iv_title = 'Release notes for apm'
      iv_act   = /apmg/if_apm_gui_router=>c_action-changelog
    )->add(
      iv_txt   = 'Hotkeys'
      iv_title = 'Keyboard shortcuts'
      iv_act   = /apmg/if_apm_gui_router=>c_action-show_hotkeys
    )->add(
      iv_txt   = 'Feedback'
      iv_title = 'We are happy to hear from you, good or bad!'
      iv_class = 'red'
      iv_act   = /apmg/if_apm_gui_router=>c_action-feedback ).

  ENDMETHOD.


  METHOD registry.

    IF registry = /apmg/if_apm_constants=>c_registry.
      DATA(fav_class) = `transport-box`. " green
    ELSE.
      fav_class = `user-box`. " blue
    ENDIF.

    result = /apmg/cl_apm_html_toolbar=>create( 'apm-toolbar-registry' )->add(
      iv_title = 'Registry'
      iv_txt   = registry
      iv_class = fav_class
      iv_act   = |{ /apmg/if_apm_gui_router=>c_action-url }?url={ registry }| ).

  ENDMETHOD.


  METHOD settings.

    result = /apmg/cl_apm_html_toolbar=>create( 'apm-toolbar-settings' ).

    result->add(
      iv_txt = 'Global'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_settings_global
    )->add(
      iv_txt = 'Personal'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_settings_personal ).

  ENDMETHOD.
ENDCLASS.
