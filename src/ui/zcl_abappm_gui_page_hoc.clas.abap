CLASS zcl_abappm_gui_page_hoc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_page
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Page HOC
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
* adapted: gui_page
  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !child_component     TYPE REF TO zif_abapgit_gui_renderable
        !page_title          TYPE string OPTIONAL
        !page_layout         TYPE string DEFAULT zcl_abapgit_gui_page=>c_page_layout-centered
        !page_menu           TYPE REF TO zcl_abapgit_html_toolbar OPTIONAL
        !page_menu_provider  TYPE REF TO zif_abapgit_gui_menu_provider OPTIONAL
        !page_title_provider TYPE REF TO zif_abapgit_gui_page_title OPTIONAL
        !extra_css_url       TYPE string OPTIONAL
        !extra_js_url        TYPE string OPTIONAL
        !show_as_modal       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)        TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS get_child
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_renderable.

    METHODS constructor
      IMPORTING
        !child_component TYPE REF TO zif_abapgit_gui_renderable
        !page_control    TYPE zcl_abapgit_gui_page=>ty_control
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    DATA child_component TYPE REF TO zif_abapgit_gui_renderable.

    METHODS detect_modal
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS detect_menu_provider
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_menu_provider.

    METHODS detect_title_provider
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_page_title.

ENDCLASS.



CLASS zcl_abappm_gui_page_hoc IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    me->child_component = child_component.
    me->page_control    = page_control.

    IF me->page_control-show_as_modal = abap_false.
      me->page_control-show_as_modal = detect_modal( ).
    ENDIF.

    IF me->page_control-page_menu_provider IS NOT BOUND.
      me->page_control-page_menu_provider = detect_menu_provider( ).
    ENDIF.

    IF me->page_control-page_title_provider IS NOT BOUND.
      me->page_control-page_title_provider = detect_title_provider( ).
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA(page_control) = VALUE zcl_abapgit_gui_page=>ty_control(
      page_title          = page_title
      page_layout         = page_layout
      page_menu           = page_menu
      page_menu_provider  = page_menu_provider
      page_title_provider = page_title_provider
      extra_css_url       = extra_css_url
      extra_js_url        = extra_js_url
      show_as_modal       = show_as_modal ).

    IF page_control-page_menu_provider IS NOT BOUND. " try component itself
      TRY.
          page_control-page_menu_provider ?= child_component.
        CATCH cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.

    IF page_control-page_title_provider IS NOT BOUND. " try component itself
      TRY.
          page_control-page_title_provider ?= child_component.
        CATCH cx_sy_move_cast_error.
      ENDTRY.
    ENDIF.

    result = NEW zcl_abappm_gui_page_hoc(
      child_component = child_component
      page_control    = page_control ).

  ENDMETHOD.


  METHOD detect_menu_provider.

    TRY.
        result ?= child_component.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.


  METHOD detect_modal.

    DATA modal TYPE REF TO zif_abapgit_gui_modal.

    TRY.
        modal ?= child_component.
        result = modal->is_modal( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.


  METHOD detect_title_provider.

    TRY.
        result ?= child_component.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_child.

    result = child_component.

  ENDMETHOD.


  METHOD render_content.

    IF child_component IS BOUND.
      result = child_component->render( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
