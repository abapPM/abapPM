CLASS zcl_abappm_gui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abappm_gui_injector.

************************************************************************
* apm GUI Factory
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS get_gui
      RETURNING
        VALUE(ro_gui) TYPE REF TO zcl_abapgit_gui
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO zif_abapgit_gui_services
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_gui TYPE REF TO zcl_abapgit_gui.
    CLASS-DATA gi_gui_services TYPE REF TO zif_abapgit_gui_services.

ENDCLASS.



CLASS zcl_abappm_gui_factory IMPLEMENTATION.


  METHOD get_gui.

    DATA:
      li_hotkey_ctl TYPE REF TO zif_abapgit_gui_hotkey_ctl,
      li_router     TYPE REF TO zif_abapgit_gui_event_handler,
      li_asset_man  TYPE REF TO zif_abapgit_gui_asset_manager.

    DATA lo_html_preprocessor TYPE REF TO zcl_abapgit_gui_html_processor.

    IF go_gui IS INITIAL.
      li_asset_man = zcl_abapgit_ui_factory=>get_asset_manager( ). " apm

      CREATE OBJECT lo_html_preprocessor EXPORTING ii_asset_man = li_asset_man.
      lo_html_preprocessor->preserve_css( 'css/ag-icons.css' ).
      lo_html_preprocessor->preserve_css( 'css/common.css' ).

      CREATE OBJECT li_router TYPE zcl_abappm_gui_router. " apm
      CREATE OBJECT li_hotkey_ctl TYPE zcl_abapgit_gui_hotkey_ctl.

      CREATE OBJECT go_gui
        EXPORTING
          io_component      = li_router
          ii_hotkey_ctl     = li_hotkey_ctl
          ii_html_processor = lo_html_preprocessor
          ii_asset_man      = li_asset_man.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.


  METHOD get_gui_services.
    IF gi_gui_services IS NOT BOUND.
      gi_gui_services ?= get_gui( ).
    ENDIF.
    ri_gui_services = gi_gui_services.
  ENDMETHOD.
ENDCLASS.
