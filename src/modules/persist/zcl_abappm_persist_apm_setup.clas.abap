CLASS zcl_abappm_persist_apm_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Persistence Setup
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS install
      RAISING
        zcx_abappm_persist_apm.

    CLASS-METHODS uninstall
      RAISING
        zcx_abappm_persist_apm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS table_create
      RAISING
        zcx_abappm_persist_apm.

    CLASS-METHODS table_delete
      RAISING
        zcx_abappm_persist_apm.

    CLASS-METHODS table_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS lock_create
      RAISING
        zcx_abappm_persist_apm.

    CLASS-METHODS lock_delete
      RAISING
        zcx_abappm_persist_apm.

    CLASS-METHODS lock_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS delete_ddic
      IMPORTING
        !iv_objtype              TYPE rsedd0-ddobjtype
        !iv_objname              TYPE rsedd0-ddobjname
        !iv_no_ask               TYPE abap_bool DEFAULT abap_true
        !iv_no_ask_delete_append TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_persist_apm.

ENDCLASS.



CLASS zcl_abappm_persist_apm_setup IMPLEMENTATION.


  METHOD delete_ddic.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = iv_objname
            objtype              = iv_objtype
            no_ask_delete_append = iv_no_ask_delete_append
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6.
      CATCH cx_sy_dyn_call_param_not_found.
        TRY.
            " try to force deletion for APPENDs
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = iv_objname
                objtype              = iv_objtype
                aie_force_deletion   = iv_no_ask_delete_append
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6.
          CATCH cx_sy_dyn_call_param_not_found.
            " no_ask_delete_append and aie_force_deletion not available in lower releases
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = iv_objname
                objtype              = iv_objtype
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6.
        ENDTRY.
    ENDTRY.

    IF sy-subrc = 5.
      zcx_abappm_persist_apm=>raise( |{ iv_objtype } { iv_objname } has dependencies and must be deleted manually| ).
    ELSEIF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error deleting { iv_objtype } { iv_objname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD install.

    IF table_exists( ) = abap_false.
      table_create( ).
    ENDIF.

    IF lock_exists( ) = abap_false.
      lock_create( ).
    ENDIF.

  ENDMETHOD.


  METHOD lock_create.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      ls_dd25v    TYPE dd25v,
      lt_dd26e    TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY,
      lt_dd27p    TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_dd26e> LIKE LINE OF lt_dd26e,
      <ls_dd27p> LIKE LINE OF lt_dd27p.

    ls_dd25v-viewname   = zif_abappm_persist_apm=>c_lock.
    ls_dd25v-aggtype    = 'E'.
    ls_dd25v-roottab    = zif_abappm_persist_apm=>c_tabname.
    ls_dd25v-ddlanguage = zif_abappm_persist_apm=>c_english.
    ls_dd25v-ddtext     = 'apm - Persistence'.

    APPEND INITIAL LINE TO lt_dd26e ASSIGNING <ls_dd26e>.
    <ls_dd26e>-viewname   = zif_abappm_persist_apm=>c_lock.
    <ls_dd26e>-tabname    = zif_abappm_persist_apm=>c_tabname.
    <ls_dd26e>-tabpos     = '0001'.
    <ls_dd26e>-fortabname = zif_abappm_persist_apm=>c_tabname.
    <ls_dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = zif_abappm_persist_apm=>c_lock.
    <ls_dd27p>-objpos    = '0001'.
    <ls_dd27p>-viewfield = 'KEYS'.
    <ls_dd27p>-tabname   = zif_abappm_persist_apm=>c_tabname.
    <ls_dd27p>-fieldname = 'KEYS'.
    <ls_dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = zif_abappm_persist_apm=>c_lock
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise_t100( ).
    ENDIF.

    lv_obj_name = zif_abappm_persist_apm=>c_lock.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = zif_abappm_persist_apm=>c_devclass
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = zif_abappm_persist_apm=>c_lock
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error activating { zif_abappm_persist_apm=>c_lock }| ).
    ENDIF.

  ENDMETHOD.


  METHOD lock_delete.
    delete_ddic(
      iv_objtype = 'L'
      iv_objname = zif_abappm_persist_apm=>c_lock ).
  ENDMETHOD.


  METHOD lock_exists.

    DATA lv_viewname TYPE dd25l-viewname.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname WHERE viewname = zif_abappm_persist_apm=>c_lock.
    result = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD table_create.

    DATA:
      lv_subrc    LIKE sy-subrc,
      lv_obj_name TYPE tadir-obj_name,
      ls_dd02v    TYPE dd02v,
      ls_dd09l    TYPE dd09l,
      lt_dd03p    TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_dd03p> LIKE LINE OF lt_dd03p.

    ls_dd02v-tabname    = zif_abappm_persist_apm=>c_tabname.
    ls_dd02v-ddlanguage = zif_abappm_persist_apm=>c_english.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = 'apm - Persistence'.
    ls_dd02v-contflag   = 'A'.
    ls_dd02v-exclass    = '1'.

    ls_dd09l-tabname   = zif_abappm_persist_apm=>c_tabname.
    ls_dd09l-as4local  = 'A'.
    ls_dd09l-tabkat    = '1'.
    ls_dd09l-tabart    = 'APPL0'.
    ls_dd09l-bufallow  = 'X'.
    ls_dd09l-pufferung = 'P'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname    = zif_abappm_persist_apm=>c_tabname.
    <ls_dd03p>-fieldname  = 'KEYS'. "KEY is not allowed
    <ls_dd03p>-position   = '0001'.
    <ls_dd03p>-keyflag    = 'X'.
    <ls_dd03p>-notnull    = 'X'.
    <ls_dd03p>-datatype   = 'CHAR'.
    <ls_dd03p>-leng       = '000120'.
    <ls_dd03p>-ddlanguage = zif_abappm_persist_apm=>c_english.
    <ls_dd03p>-ddtext     = 'Key'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname    = zif_abappm_persist_apm=>c_tabname.
    <ls_dd03p>-fieldname  = 'VALUE'.
    <ls_dd03p>-position   = '0002'.
    <ls_dd03p>-datatype   = 'STRG'.
    <ls_dd03p>-ddlanguage = zif_abappm_persist_apm=>c_english.
    <ls_dd03p>-ddtext     = 'Value'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname    = zif_abappm_persist_apm=>c_tabname.
    <ls_dd03p>-fieldname  = 'LUSER'.
    <ls_dd03p>-position   = '0003'.
    <ls_dd03p>-rollname   = 'AS4USER'.
    <ls_dd03p>-datatype   = 'CHAR'.
    <ls_dd03p>-leng       = '000012'.
    <ls_dd03p>-ddlanguage = zif_abappm_persist_apm=>c_english.
    <ls_dd03p>-ddtext     = 'Last Changed By'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname    = zif_abappm_persist_apm=>c_tabname.
    <ls_dd03p>-fieldname  = 'TIMESTAMP'.
    <ls_dd03p>-position   = '0004'.
    <ls_dd03p>-rollname   = 'TIMESTAMPL'.
    <ls_dd03p>-datatype   = 'DEC'.
    <ls_dd03p>-leng       = '000021'.
    <ls_dd03p>-decimals   = '00007'.
    <ls_dd03p>-ddlanguage = zif_abappm_persist_apm=>c_english.
    <ls_dd03p>-ddtext     = 'Last Changed At'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = zif_abappm_persist_apm=>c_tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise_t100( ).
    ENDIF.

    lv_obj_name = zif_abappm_persist_apm=>c_tabname.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = zif_abappm_persist_apm=>c_devclass
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = zif_abappm_persist_apm=>c_tabname
        auth_chk    = abap_false
      IMPORTING
        rc          = lv_subrc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR lv_subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Error activating { zif_abappm_persist_apm=>c_tabname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD table_delete.

    DATA:
      lv_no_ask TYPE abap_bool,
      lv_subrc  TYPE sy-subrc,
      BEGIN OF ls_dd02l,
        tabname  TYPE dd02l-tabname,
        tabclass TYPE dd02l-tabclass,
        sqltab   TYPE dd02l-sqltab,
      END OF ls_dd02l.

    lv_no_ask = abap_true.

    SELECT SINGLE tabname tabclass sqltab FROM dd02l
      INTO CORRESPONDING FIELDS OF ls_dd02l
      WHERE tabname = zif_abappm_persist_apm=>c_tabname AND as4local = 'A' AND as4vers = '0000'.
    IF sy-subrc <> 0.
      zcx_abappm_persist_apm=>raise( |Table { zif_abappm_persist_apm=>c_tabname } not found| ).
    ENDIF.

    CALL FUNCTION 'DD_EXISTS_DATA'
      EXPORTING
        reftab          = ls_dd02l-sqltab
        tabclass        = ls_dd02l-tabclass
        tabname         = ls_dd02l-tabname
      IMPORTING
        subrc           = lv_subrc
      EXCEPTIONS
        missing_reftab  = 1
        sql_error       = 2
        buffer_overflow = 3
        unknown_error   = 4
        OTHERS          = 5.

    IF sy-subrc = 0 AND lv_subrc = 0.
      lv_no_ask = abap_false.
    ENDIF.

    delete_ddic(
      iv_objtype = 'T'
      iv_objname = zif_abappm_persist_apm=>c_tabname
      iv_no_ask  = lv_no_ask ).

  ENDMETHOD.


  METHOD table_exists.

    DATA lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname WHERE tabname = zif_abappm_persist_apm=>c_tabname.
    result = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD uninstall.

    IF lock_exists( ) = abap_true.
      lock_delete( ).
    ENDIF.

    IF table_exists( ) = abap_true.
      table_delete( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
