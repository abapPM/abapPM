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

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_persist_apm.

    CLASS-METHODS injector
      IMPORTING
        !ii_mock TYPE REF TO zif_abappm_persist_apm.

    CLASS-METHODS validate_key
      IMPORTING
        !iv_key       TYPE zif_abappm_persist_apm=>ty_key
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO zif_abappm_persist_apm.

    DATA mv_update_function TYPE funcname.

    METHODS get_update_function
      RETURNING
        VALUE(rv_funcname) TYPE funcname.

ENDCLASS.



CLASS zcl_abappm_persist_apm IMPLEMENTATION.


  METHOD get_instance.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance TYPE zcl_abappm_persist_apm.
    ENDIF.
    result = go_instance.
  ENDMETHOD.


  METHOD get_update_function.
    IF mv_update_function IS INITIAL.
      mv_update_function = 'CALL_V1_PING'.
      IF zcl_abapgit_factory=>get_function_module( )->function_exists( mv_update_function ) = abap_false.
        mv_update_function = 'BANK_OBJ_WORKL_RELEASE_LOCKS'.
      ENDIF.
    ENDIF.
    rv_funcname = mv_update_function.
  ENDMETHOD.


  METHOD injector.
    go_instance = ii_mock.
  ENDMETHOD.


  METHOD validate_key.

    DATA:
      lv_key_type TYPE string,
      lv_rest     TYPE string.

    SPLIT iv_key AT ':' INTO lv_key_type lv_rest.

    result = boolc( sy-subrc = 0 AND
      ( lv_key_type = zif_abappm_persist_apm=>c_key_type-package OR
        lv_key_type = zif_abappm_persist_apm=>c_key_type-settings ) ).

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~delete.

    DELETE FROM (zif_persist_apm=>c_tabname) WHERE keys = iv_key.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error deleting { iv_key }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~list.
    IF iv_filter IS INITIAL.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE result
        WHERE timestamp BETWEEN iv_from AND iv_to.
    ELSE.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE result
        WHERE timestamp BETWEEN iv_from AND iv_to AND keys LIKE iv_filter.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abappm_persist_apm~load.
    SELECT SINGLE * FROM (zif_persist_apm=>c_tabname) INTO result WHERE keys = iv_key.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error loading { iv_key }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abappm_persist_apm~lock.

    DATA lv_dummy_update_function TYPE funcname.

    CALL FUNCTION 'ENQUEUE_EZABAPPM'
      EXPORTING
        mode_zabappm   = iv_mode
        keys           = iv_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise_t100( ).
    ENDIF.

    lv_dummy_update_function = get_update_function( ).

    " trigger dummy update task to automatically release locks at commit
    CALL FUNCTION lv_dummy_update_function IN UPDATE TASK.

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~save.

    DATA ls_abappm TYPE zif_abappm_persist_apm=>ty_zabappm.

    IF validate_key( iv_key ) = abap_false.
      zcx_abappm_persist_apm=>raise( |Invalid key { iv_key }| ).
    ENDIF.

    ls_abappm-keys  = iv_key.
    ls_abappm-value = replace(
      val  = iv_value
      sub  = cl_abap_char_utilities=>cr_lf
      with = cl_abap_char_utilities=>newline
      occ  = 0 ).
    ls_abappm-luser = sy-uname.
    GET TIME STAMP FIELD ls_abappm-timestamp.

    INSERT (zif_persist_apm=>c_tabname) FROM ls_abappm.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error saving { iv_key }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
