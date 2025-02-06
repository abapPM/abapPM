CLASS zcl_abappm_gui_factory DEFINITION
  PUBLIC
  CREATE PRIVATE.

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
        VALUE(result) TYPE REF TO zcl_abappm_gui
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_gui_services
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_gui_services
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_frontend_services
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_frontend_services.

    CLASS-METHODS get_html_viewer
      IMPORTING
        !container           TYPE REF TO cl_gui_container DEFAULT cl_gui_container=>screen0
        !disable_query_table TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result)        TYPE REF TO zif_abappm_html_viewer.

    CLASS-METHODS get_popups
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_popups.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      gui               TYPE REF TO zcl_abappm_gui,
      gui_service       TYPE REF TO zif_abappm_gui_services,
      frontend_services TYPE REF TO zif_abappm_frontend_services,
      html_viewer       TYPE REF TO zif_abappm_html_viewer,
      popups            TYPE REF TO zif_abappm_popups.

    CLASS-METHODS get_asset_manager
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_gui_asset_manager
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_gui_factory IMPLEMENTATION.


  METHOD get_asset_manager.

    DATA lo_buf TYPE REF TO zcl_abapgit_string_buffer.
    DATA li_asset_man TYPE REF TO zif_abappm_gui_asset_manager.

    CREATE OBJECT lo_buf.

    li_asset_man = zcl_abappm_gui_asset_manager=>create( ).

    " @@abapmerge include zabapgit_css_common.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_default.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-default.css'
      iv_type      = 'text/css'
      iv_cacheable = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DEFAULT'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_dark.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-dark.css'
      iv_type      = 'text/css'
      iv_cacheable = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DARK'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_css_theme_belize_blue.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-belize-blue.css'
      iv_type      = 'text/css'
      iv_cacheable = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_BELIZE_BLUE'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_js_common.w3mi.data.js > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'js/common.js'
      iv_type      = 'text/javascript'
      iv_mime_name = 'ZABAPGIT_JS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include zabapgit_icon_font_css.w3mi.data.css > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'css/ag-icons.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_ICON_FONT_CSS'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include-base64 zabapgit_icon_font.w3mi.data.woff > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'font/ag-icons.woff'
      iv_type      = 'font/woff'
      iv_mime_name = 'ZABAPGIT_ICON_FONT'
      iv_base64    = lo_buf->join_and_flush( ) ).

    result = li_asset_man.

  ENDMETHOD.


  METHOD get_frontend_services.

    IF frontend_services IS INITIAL.
      CREATE OBJECT frontend_services TYPE zcl_abappm_frontend_services.
    ENDIF.

    result = frontend_services.

  ENDMETHOD.


  METHOD get_gui.

    DATA:
      hotkey_controller TYPE REF TO zif_abappm_gui_hotkey_ctl,
      router            TYPE REF TO zif_abappm_gui_event_handler.

    IF gui IS INITIAL.
      DATA(asset_mananager) = get_asset_manager( ).

      DATA(html_preprocessor) = NEW zcl_abappm_gui_html_processor( ii_asset_man = asset_mananager ).

      html_preprocessor->preserve_css( 'css/ag-icons.css' ).
      html_preprocessor->preserve_css( 'css/common.css' ).

      CREATE OBJECT router TYPE zcl_abappm_gui_router.
      CREATE OBJECT hotkey_controller TYPE zcl_abappm_gui_hotkey_ctl.

      gui = NEW #(
        io_component      = router
        ii_hotkey_ctl     = hotkey_controller
        ii_html_processor = html_preprocessor
        ii_asset_man      = asset_mananager ).
    ENDIF.

    result = gui.

  ENDMETHOD.


  METHOD get_gui_services.
    IF gui_service IS NOT BOUND.
      gui_service ?= get_gui( ).
    ENDIF.
    result = gui_service.
  ENDMETHOD.


  METHOD get_html_viewer.

    IF html_viewer IS NOT BOUND.
      CREATE OBJECT html_viewer TYPE zcl_abappm_gui_html_viewer
        EXPORTING
          io_container           = container
          iv_disable_query_table = disable_query_table.
    ENDIF.

    result = html_viewer.

  ENDMETHOD.


  METHOD get_popups.

    IF popups IS INITIAL.
      CREATE OBJECT popups TYPE zcl_abappm_popups.
    ENDIF.

    result = popups.

  ENDMETHOD.
ENDCLASS.
