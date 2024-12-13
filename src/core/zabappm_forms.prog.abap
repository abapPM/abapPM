************************************************************************
* apm Forms
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************

FORM run.

  TRY.
      " TODO: Authorization check

      " Initialize persistence and global settings
      zcl_abappm_persist_apm_setup=>install( ).
      zcl_abappm_settings=>initialize_global_settings( ).

      PERFORM open_gui.

    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

FORM open_gui RAISING zcx_abapgit_exception.

  DATA:
    action TYPE string,
    mode   TYPE tabname,
    router TYPE REF TO zif_abapgit_gui_event_handler.

  IF sy-batch = abap_true.
    " FUTURE: One day we will add this
    " zcl_abappm_background=>run( )
    MESSAGE s000(oo) WITH 'apm does not support background processing'.
  ELSE.

    " https://docs.abapgit.org/user-guide/reference/database-util.html#emergency-mode
    GET PARAMETER ID 'DBT' FIELD mode.
    CASE mode.
      WHEN 'ZABAPPM'.
        action = zif_abapgit_definitions=>c_action-go_db.
      WHEN OTHERS.
        action = zif_abapgit_definitions=>c_action-go_home.
    ENDCASE.

    " TODO: zcl_abapgit_services_abapgit=>prepare_gui_startup( ).

    zcl_abappm_gui_factory=>get_gui( )->go_home( action ).

    CALL SELECTION-SCREEN 1001. " trigger screen

  ENDIF.

ENDFORM.

FORM output.

  DATA excluded_commands TYPE TABLE OF sy-ucomm.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO excluded_commands.  "Button Execute
  APPEND 'SPOS' TO excluded_commands.  "Button Save

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = excluded_commands.

  TRY.
      zcl_abappm_gui_factory=>get_gui( )->set_focus( ).
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

FORM exit.

  " The exit logic should only be applied for our 'main' selection screen 1001.
  " All other selection-screens are called as popups and shouldn't influence
  " the gui navigation as it would lead to inpredictable behaviour like dumps.
  IF sy-dynnr <> 1001.
    RETURN.
  ENDIF.

  TRY.
      CASE sy-ucomm.
        WHEN 'CBAC' OR 'CCAN'.  "Back & Escape
          IF zcl_abappm_gui_factory=>get_gui( )->back( iv_graceful = abap_true ) = abap_true. " end of stack
            zcl_abappm_gui_factory=>get_gui( )->free( ). " Graceful shutdown
          ELSE.
            LEAVE TO SCREEN 1001.
          ENDIF.
      ENDCASE.
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

FORM adjust_toolbar USING pv_dynnr TYPE sy-dynnr.

  DATA:
    ls_header               TYPE rpy_dyhead,
    lt_containers           TYPE dycatt_tab,
    lt_fields_to_containers TYPE dyfatc_tab,
    lt_flow_logic           TYPE swydyflow,
    lv_no_toolbar           LIKE ls_header-no_toolbar.

  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname             = sy-cprog
      dynnr                = pv_dynnr
    IMPORTING
      header               = ls_header
    TABLES
      containers           = lt_containers
      fields_to_containers = lt_fields_to_containers
      flow_logic           = lt_flow_logic
    EXCEPTIONS
      cancelled            = 1
      not_found            = 2
      permission_error     = 3
      OTHERS               = 4.
  IF sy-subrc IS NOT INITIAL.
    RETURN. " Ignore errors, just exit
  ENDIF.

  " Remove toolbar on html screen
  lv_no_toolbar = abap_true.

  IF ls_header-no_toolbar = lv_no_toolbar.
    RETURN. " No change required
  ENDIF.

  ls_header-no_toolbar = lv_no_toolbar.

  CALL FUNCTION 'RPY_DYNPRO_INSERT'
    EXPORTING
      header                 = ls_header
      suppress_exist_checks  = abap_true
    TABLES
      containers             = lt_containers
      fields_to_containers   = lt_fields_to_containers
      flow_logic             = lt_flow_logic
    EXCEPTIONS
      cancelled              = 1
      already_exists         = 2
      program_not_exists     = 3
      not_executed           = 4
      missing_required_field = 5
      illegal_field_value    = 6
      field_not_allowed      = 7
      not_generated          = 8
      illegal_field_position = 9
      OTHERS                 = 10.
  IF sy-subrc <> 2 AND sy-subrc <> 0.
    RETURN. " Ignore errors, just exit
  ENDIF.

ENDFORM.
