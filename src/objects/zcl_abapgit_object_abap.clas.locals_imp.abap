INTERFACE lif_persist_apm.

************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
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
    c_zapm        TYPE tadir-object VALUE 'ZAPM',
    c_devclass    TYPE c LENGTH 30 VALUE '$TMP',
    c_transaction TYPE c LENGTH 30 VALUE 'ZAPM',
    c_tabname     TYPE c LENGTH 30 VALUE 'ZABAPPM',
    c_lock        TYPE c LENGTH 30 VALUE 'EZABAPPM',
    c_english     TYPE c LENGTH 1 VALUE 'E'.

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

************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES lif_persist_apm.

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_persist_apm=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RESULT                         TYPE REF TO lif_persist_apm
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance TYPE lcl_persist_apm.
    ENDIF.
    result = go_instance.
  ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_persist_apm=>INJECTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] II_MOCK                        TYPE REF TO lif_persist_apm
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD injector.
    go_instance = ii_mock.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_persist_apm->lif_persist_apm~DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        TY_KEY
* | [!CX!] ZCX_PERSIST_APM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lif_persist_apm~delete.

    DELETE FROM (lif_persist_apm=>c_tabname) WHERE keys = iv_key.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting { iv_key }| ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_persist_apm->lif_persist_apm~LOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        TY_KEY
* | [<-()] RESULT                         TYPE        TY_ZABAPPM
* | [!CX!] ZCX_PERSIST_APM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lif_persist_apm~load.
    SELECT SINGLE * FROM (lif_persist_apm=>c_tabname) INTO result WHERE keys = iv_key.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_persist_apm->lif_persist_apm~LOCK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        TY_KEY
* | [--->] IV_MODE                        TYPE        ENQMODE (default ='E')
* | [!CX!] ZCX_PERSIST_APM
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method lcl_persist_apm->lif_persist_apm~SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KEY                         TYPE        TY_KEY
* | [--->] IV_VALUE                       TYPE        TY_ZABAPPM-VALUE
* | [!CX!] ZCX_PERSIST_APM
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
