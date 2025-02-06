CLASS zcl_abappm_popups DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abappm_gui_factory.

************************************************************************
* apm Popups
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* apm: remove irrelevant method implementations
  PUBLIC SECTION.

    INTERFACES zif_abappm_popups.

    CLASS-METHODS center
      IMPORTING
        !iv_width          TYPE i
        !iv_height         TYPE i
      RETURNING
        VALUE(rs_position) TYPE zif_abappm_popups=>ty_popup_position.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_position TYPE zif_abappm_popups=>ty_popup_position.

ENDCLASS.



CLASS zcl_abappm_popups IMPLEMENTATION.


  METHOD center.

    CONSTANTS:
      lc_min_size TYPE i VALUE 10,
      lc_min_pos  TYPE i VALUE 5.

    " Magic math to approximate starting position of popup
    IF sy-scols > lc_min_size AND iv_width > 0 AND sy-scols > iv_width.
      rs_position-start_column = nmax(
        val1 = ( sy-scols - iv_width ) / 2
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_column = lc_min_pos.
    ENDIF.

    IF sy-srows > lc_min_size AND iv_height > 0 AND sy-srows > iv_height.
      rs_position-start_row = nmax(
        val1 = ( sy-srows - iv_height ) / 2 - 1
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_row = lc_min_pos.
    ENDIF.

    rs_position-end_column = rs_position-start_column + iv_width.
    rs_position-end_row = rs_position-start_row + iv_height.

  ENDMETHOD.


  METHOD zif_abappm_popups~popup_folder_logic.
  ENDMETHOD.


  METHOD zif_abappm_popups~popup_search_help.

    DATA lt_ret TYPE TABLE OF ddshretval.
    DATA ls_ret LIKE LINE OF lt_ret.
    DATA lv_tabname TYPE dfies-tabname.
    DATA lv_fieldname TYPE dfies-fieldname.

    SPLIT iv_tab_field AT '-' INTO lv_tabname lv_fieldname.
    lv_tabname = to_upper( lv_tabname ).
    lv_fieldname = to_upper( lv_fieldname ).

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = lv_tabname
        fieldname  = lv_fieldname
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.

    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |F4IF_FIELD_VALUE_REQUEST error [{ iv_tab_field }]| ).
    ENDIF.

    IF lines( lt_ret ) > 0.
      READ TABLE lt_ret WITH KEY fieldname = lv_fieldname INTO ls_ret.
      IF sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ELSE.
        READ TABLE lt_ret INDEX 1 INTO ls_ret.
        ASSERT sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_popups~popup_to_confirm.

    ms_position = center(
      iv_width  = 65
      iv_height = 5 ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_titlebar
        text_question         = iv_text_question
        text_button_1         = iv_text_button_1
        icon_button_1         = iv_icon_button_1
        text_button_2         = iv_text_button_2
        icon_button_2         = iv_icon_button_2
        default_button        = iv_default_button
        display_cancel_button = iv_display_cancel_button
        popup_type            = iv_popup_type
        start_column          = ms_position-start_column
        start_row             = ms_position-start_row
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_popups~popup_to_create_package.

    DATA ls_data TYPE scompkdtln.

    MOVE-CORRESPONDING is_package_data TO ls_data.

    IF zcl_abappm_factory=>get_function_module( )->function_exists( 'PB_POPUP_PACKAGE_CREATE' ) = abap_false.
      " looks like the function module used does not exist on all versions since 702
      zcx_abappm_error=>raise( 'Your system does not support automatic creation of packages.' &&
        'Please, create the package manually.' ).
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = ls_data
      EXCEPTIONS
        action_cancelled = 1.
    ev_create = boolc( sy-subrc = 0 ).

    MOVE-CORRESPONDING ls_data TO es_package_data.

  ENDMETHOD.


  METHOD zif_abappm_popups~popup_to_select_labels.
  ENDMETHOD.


  METHOD zif_abappm_popups~popup_to_select_transport.

    CALL FUNCTION 'TR_F4_REQUESTS'
      IMPORTING
        ev_selected_request = rv_trkorr.

  ENDMETHOD.
ENDCLASS.
