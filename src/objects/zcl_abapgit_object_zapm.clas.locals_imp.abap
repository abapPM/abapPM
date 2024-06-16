************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
INTERFACE lif_persist_apm.

  TYPES:
    ty_key TYPE c LENGTH 120,
    BEGIN OF ty_zabappm,
      keys      TYPE ty_key,
      value     TYPE string,
      luser     TYPE as4user,
      timestamp TYPE timestampl,
    END OF ty_zabappm,
    ty_list TYPE SORTED TABLE OF ty_zabappm WITH UNIQUE KEY keys.

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  CONSTANTS:
    c_tabname  TYPE c LENGTH 30 VALUE 'ZABAPPM',
    c_lock     TYPE c LENGTH 30 VALUE 'EZABAPPM'.

  METHODS load
    IMPORTING
      !iv_key       TYPE ty_key
    RETURNING
      VALUE(result) TYPE ty_zabappm
    RAISING
      zcx_abapgit_exception.

  METHODS save
    IMPORTING
      !iv_key   TYPE ty_key
      !iv_value TYPE ty_zabappm-value
    RAISING
      zcx_abapgit_exception.

  METHODS delete
    IMPORTING
      !iv_key TYPE ty_key
    RAISING
      zcx_abapgit_exception.

  METHODS lock
    IMPORTING
      !iv_key  TYPE ty_key
      !iv_mode TYPE enqmode DEFAULT 'E'
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.

CLASS lcl_persist_apm DEFINITION.

  PUBLIC SECTION.

    INTERFACES lif_persist_apm.

    CLASS-METHODS is_installed
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO lif_persist_apm.

    CLASS-METHODS injector
      IMPORTING
        !ii_mock TYPE REF TO lif_persist_apm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO lif_persist_apm.

ENDCLASS.



CLASS lcl_persist_apm IMPLEMENTATION.

  METHOD is_installed.

    DATA lv_tabname TYPE dd02l-tabname.

    " Is apm installed?
    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = lif_persist_apm=>c_tabname
        AND as4vers = '0000' AND as4local = 'A'.
    result = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD get_instance.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance TYPE lcl_persist_apm.
    ENDIF.
    result = go_instance.
  ENDMETHOD.

  METHOD injector.
    go_instance = ii_mock.
  ENDMETHOD.

  METHOD lif_persist_apm~delete.
    DELETE FROM (lif_persist_apm=>c_tabname) WHERE keys = iv_key.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting { iv_key }| ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_persist_apm~load.
    SELECT SINGLE * FROM (lif_persist_apm=>c_tabname) INTO result WHERE keys = iv_key.
  ENDMETHOD.

  METHOD lif_persist_apm~lock.
    CALL FUNCTION 'ENQUEUE_EZABAPPM'
      EXPORTING
        mode_zabappm   = iv_mode
        keys           = iv_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_persist_apm~save.

    DATA ls_abappm TYPE lif_persist_apm=>ty_zabappm.

    ls_abappm-keys  = iv_key.
    ls_abappm-value = replace(
      val  = iv_value
      sub  = cl_abap_char_utilities=>cr_lf
      with = cl_abap_char_utilities=>newline
      occ  = 0 ).
    ls_abappm-luser = sy-uname.
    GET TIME STAMP FIELD ls_abappm-timestamp.

    UPDATE (lif_persist_apm=>c_tabname) FROM ls_abappm.
    IF sy-subrc <> 0.
      INSERT (lif_persist_apm=>c_tabname) FROM ls_abappm.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error saving { iv_key }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
