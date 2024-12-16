CLASS zcl_abappm_gui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abappm_gui_injector.

************************************************************************
* apm GUI Factory
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
* adapted: router, hotkey_controller
  PUBLIC SECTION.

    CLASS-METHODS get_gui
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_gui
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_gui_services
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_services
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gui TYPE REF TO zcl_abapgit_gui.
    CLASS-DATA gui_service TYPE REF TO zif_abapgit_gui_services.

ENDCLASS.



CLASS zcl_abappm_gui_factory IMPLEMENTATION.


  METHOD get_gui.
    IF gui IS INITIAL.
      DATA(asset_mananager) = zcl_abapgit_ui_factory=>get_asset_manager( ).

      DATA(html_preprocessor) = NEW zcl_abapgit_gui_html_processor( ii_asset_man = asset_mananager ).

      html_preprocessor->preserve_css( 'css/ag-icons.css' ).
      html_preprocessor->preserve_css( 'css/common.css' ).

      DATA(router)            = NEW zcl_abappm_gui_router( ). " apm: routing
      DATA(hotkey_controller) = NEW zcl_abappm_gui_hotkey_ctl( ). " apm: settings

      CREATE OBJECT gui
        EXPORTING
          io_component      = router
          ii_hotkey_ctl     = hotkey_controller
          ii_html_processor = html_preprocessor
          ii_asset_man      = asset_mananager.
    ENDIF.

    result = gui.
  ENDMETHOD.


  METHOD get_gui_services.
    IF gui_service IS NOT BOUND.
      gui_service ?= get_gui( ).
    ENDIF.
    result = gui_service.
  ENDMETHOD.
ENDCLASS.
