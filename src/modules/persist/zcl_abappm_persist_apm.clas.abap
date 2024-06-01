CLASS zcl_abappm_persist_apm DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abappm_persist_apm.

    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_persist_apm.

    CLASS-METHODS injector
      IMPORTING
        !ii_mock TYPE REF TO zif_abappm_persist_apm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO zif_abappm_persist_apm.

ENDCLASS.



CLASS zcl_abappm_persist_apm IMPLEMENTATION.


  METHOD factory.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance TYPE zcl_abappm_persist_apm.
    ENDIF.
    result = go_instance.
  ENDMETHOD.


  METHOD injector.
    go_instance = ii_mock.
  ENDMETHOD.


  METHOD zif_abappm_persist_apm~delete.

    DELETE FROM (zif_persist_apm=>c_tabname) WHERE key = iv_key.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error deleting { iv_key }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~list.
    IF iv_key IS INITIAL.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE result.
    ELSE.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE result WHERE key LIKE iv_key.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abappm_persist_apm~load.
    SELECT SINGLE * FROM (zif_persist_apm=>c_tabname) INTO result WHERE key = iv_key.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error loading { iv_key }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abappm_persist_apm~save.

    DATA ls_abappm TYPE zif_abappm_persist_apm=>ty_zabappm.

    ls_abappm-key   = iv_key.
    ls_abappm-value = replace(
      val  = iv_value
      sub  = cl_abap_char_utilities=>cr_lf
      with = cl_abap_char_utilities=>newline
      occ  = 0 ).
    ls_abappm-luser = sy-uname.
    ls_abappm-ldate = sy-datum.
    ls_abappm-ltime = sy-uzeit.

    INSERT (zif_persist_apm=>c_tabname) FROM ls_abappm.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error saving { iv_key }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
