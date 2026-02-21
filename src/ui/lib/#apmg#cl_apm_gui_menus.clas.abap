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
            iv_act = /apmg/if_apm_gui_router=>c_action-go_settings ).
        ENDIF.
      CATCH /apmg/cx_apm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD help.

    result = /apmg/cl_apm_html_toolbar=>create( 'apm-toolbar-help' ).

    result->add(
      iv_txt = 'Feedback'
      iv_class = 'red'
      iv_act = /apmg/if_apm_gui_router=>c_action-feedback
    )->add(
      iv_txt = 'Registry'
      iv_act = /apmg/if_apm_gui_router=>c_action-registry
* FUTURE
*    )->add(
*      iv_txt = 'Tutorial'
*      iv_act = /apmg/if_apm_gui_router=>c_action-go_tutorial
    )->add(
      iv_txt = 'Documentation'
      iv_act = /apmg/if_apm_gui_router=>c_action-documentation
    )->add(
      iv_txt = 'Changelog'
      iv_act = /apmg/if_apm_gui_router=>c_action-changelog
    )->add(
      iv_txt = 'Hotkeys'
      iv_act = /apmg/if_apm_gui_router=>c_action-show_hotkeys ).

  ENDMETHOD.


  METHOD registry.

    IF registry = /apmg/if_apm_settings=>c_registry.
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
      iv_act = /apmg/if_apm_gui_router=>c_action-go_settings
    )->add(
      iv_txt = 'Personal'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_settings_personal ).

  ENDMETHOD.
ENDCLASS.
