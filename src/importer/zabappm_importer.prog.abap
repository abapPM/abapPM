************************************************************************
* apm Importer
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* This standalone tool imports all modules of a given package.
* Currently, the individual modules must be installed as global classes.
* The global classes are then copied and renamed according to the
* IMPORT statements found in sub-packages. Dependencies between the
* imported modules are taken into consideration.
*
* The tool will eventually become the "Update Command" and will be
* integrated into the apm UI.
************************************************************************
REPORT zabappm_importer LINE-SIZE 255.

TABLES tadir.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS p_pack TYPE tadir-devclass DEFAULT '$ABAPPM'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
  SELECT-OPTIONS:
    s_type FOR tadir-object,
    s_name FOR tadir-obj_name.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
  PARAMETERS:
    p_defrul TYPE string,
    p_prod   AS CHECKBOX DEFAULT 'X',
    p_log    AS CHECKBOX DEFAULT 'X',
    p_dryrun AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  p_defrul = zif_abappm_importer=>c_default_import_rule.

START-OF-SELECTION.

  DATA(timer) = zcl_abapgit_timer=>create( 'apm Import' ).
  timer->start( ).

  TRY.
      zcl_abappm_importer=>run(
        package       = p_pack
        object_types  = s_type[]
        object_names  = s_name[]
        default_rule  = p_defrul
        is_dryrun     = p_dryrun
        is_production = p_prod ).

    CATCH zcx_abappm_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

  IF p_log = abap_true.
    SKIP.
    WRITE / timer->end( ).
  ENDIF.
