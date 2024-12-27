************************************************************************
* apm Password Dialog
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE sc_title.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(18) sc_url FOR FIELD p_url.
    PARAMETERS p_url TYPE string LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(18) sc_user FOR FIELD p_user.
    PARAMETERS p_user TYPE string LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(18) sc_pass FOR FIELD p_pass.
    PARAMETERS p_pass TYPE c LENGTH 255 LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(18) sc_cmnt FOR FIELD p_cmnt.
    PARAMETERS p_cmnt TYPE c LENGTH 255 LOWER CASE VISIBLE LENGTH 60 ##SEL_WRONG.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 1002.

*-----------------------------------------------------------------------
* LCL_PASSWORD_DIALOG
*-----------------------------------------------------------------------
CLASS lcl_password_dialog DEFINITION FINAL.

**************
* This class will remain local in the report
**************

  PUBLIC SECTION.

    CONSTANTS c_dynnr TYPE c LENGTH 4 VALUE '1002'.

    CLASS-METHODS popup
      IMPORTING
        url      TYPE string
      CHANGING
        username TYPE string
        password TYPE string.

    CLASS-METHODS on_screen_init.
    CLASS-METHODS on_screen_output.
    CLASS-METHODS on_screen_event
      IMPORTING
        command TYPE sy-ucomm.

  PRIVATE SECTION.

    CLASS-DATA is_confirmed TYPE abap_bool.

    CLASS-METHODS enrich_title_by_hostname
      IMPORTING
        url TYPE string.

ENDCLASS.

CLASS lcl_password_dialog IMPLEMENTATION.

  METHOD popup.

    CLEAR p_pass.
    p_url      = url.
    p_user     = username.
    is_confirmed = abap_false.

    p_cmnt = 'Press F1 for Help'.

    enrich_title_by_hostname( url ).

    DATA(position) = zcl_abapgit_popups=>center(
      iv_width  = 65
      iv_height = 7 ) ##NEEDED.

    CALL SELECTION-SCREEN c_dynnr
      STARTING AT position-start_column position-start_row
      ENDING AT position-end_column position-end_row.

    IF is_confirmed = abap_true.
      username = p_user.
      password = p_pass.
    ELSE.
      CLEAR: username, password.
    ENDIF.

    CLEAR: p_url, p_user, p_pass.

  ENDMETHOD.

  METHOD on_screen_init.
    sc_title = 'Login'.
    sc_url   = 'Registry URL'.
    sc_user  = 'User'.
    sc_pass  = 'Password or Token'.
    sc_cmnt  = 'Note'.
  ENDMETHOD.

  METHOD on_screen_output.

    DATA excluded_commands TYPE STANDARD TABLE OF sy-ucomm WITH KEY table_line.

    ASSERT sy-dynnr = c_dynnr.

    LOOP AT SCREEN.
      IF screen-name = 'P_URL' OR screen-name = 'P_CMNT'.
        screen-input       = '0'.
        screen-intensified = '1'.
        screen-display_3d  = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_CMNT' OR screen-name = 'SC_CMNT'.
        screen-active    = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_PASS'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    APPEND 'PICK' TO excluded_commands.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'DETL'
        p_program = 'RSPFPAR'
      TABLES
        p_exclude = excluded_commands.

    IF p_user IS NOT INITIAL.
      SET CURSOR FIELD 'P_PASS'.
    ENDIF.

  ENDMETHOD.

  METHOD on_screen_event.

    ASSERT sy-dynnr = c_dynnr.

    CASE command.
      WHEN 'OK'. " Enter
        is_confirmed = abap_true.
        LEAVE TO SCREEN 0.
      WHEN 'HELP'. " F1
        " TODO: open help page
      WHEN OTHERS. " Escape
        is_confirmed = abap_false.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.


  METHOD enrich_title_by_hostname.

    FIND REGEX 'https?://([^/^:]*)' IN url SUBMATCHES DATA(host).
    IF sy-subrc = 0 AND host IS NOT INITIAL.
      CLEAR sc_title.
      CONCATENATE 'Login:' host INTO sc_title IN CHARACTER MODE SEPARATED BY space.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


FORM password_popup
  USING
    url      TYPE string
  CHANGING
    username TYPE string
    password TYPE string.

  lcl_password_dialog=>popup(
    EXPORTING
      url      = url
    CHANGING
      username = username
      password = password ).

ENDFORM.
