CLASS /apmg/cl_apm_persist_apm_setup DEFINITION
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
        /apmg/cx_apm_error.

    CLASS-METHODS uninstall
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS logo_create
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS logo_delete
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS logo_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS table_create
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS table_delete
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS table_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS lock_create
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS lock_delete
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS lock_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS delete_ddic
      IMPORTING
        !objtype              TYPE rsedd0-ddobjtype
        !objname              TYPE rsedd0-ddobjname
        !no_ask               TYPE abap_bool DEFAULT abap_true
        !no_ask_delete_append TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_persist_apm_setup IMPLEMENTATION.


  METHOD delete_ddic.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = no_ask
            objname              = objname
            objtype              = objtype
            no_ask_delete_append = no_ask_delete_append
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
                no_ask               = no_ask
                objname              = objname
                objtype              = objtype
                aie_force_deletion   = no_ask_delete_append
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
                no_ask               = no_ask
                objname              = objname
                objtype              = objtype
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
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |{ objtype } { objname } has dependencies and must be deleted manually|.
    ELSEIF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Error deleting { objtype } { objname }|.
    ENDIF.

  ENDMETHOD.


  METHOD install.

    IF logo_exists( ) = abap_false.
      logo_create( ).
    ENDIF.

    IF table_exists( ) = abap_false.
      table_create( ).
    ENDIF.

    IF lock_exists( ) = abap_false.
      lock_create( ).
    ENDIF.

  ENDMETHOD.


  METHOD lock_create.

    DATA:
      dd26e TYPE STANDARD TABLE OF dd26e WITH KEY ddlanguage viewname tabname tabpos,
      dd27p TYPE STANDARD TABLE OF dd27p WITH KEY viewname objpos ddlanguage viewfield tabname fieldname.

    DATA(dd25v) = VALUE dd25v(
      viewname   = /apmg/if_apm_persist_apm=>c_lock
      aggtype    = 'E'
      roottab    = /apmg/if_apm_persist_apm=>c_tabname
      ddlanguage = /apmg/if_apm_persist_apm=>c_english
      ddtext     = 'apm - Persistence' ).

    APPEND INITIAL LINE TO dd26e ASSIGNING FIELD-SYMBOL(<dd26e>).
    <dd26e>-viewname   = /apmg/if_apm_persist_apm=>c_lock.
    <dd26e>-tabname    = /apmg/if_apm_persist_apm=>c_tabname.
    <dd26e>-tabpos     = '0001'.
    <dd26e>-fortabname = /apmg/if_apm_persist_apm=>c_tabname.
    <dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO dd27p ASSIGNING FIELD-SYMBOL(<dd27p>).
    <dd27p>-viewname  = /apmg/if_apm_persist_apm=>c_lock.
    <dd27p>-objpos    = '0001'.
    <dd27p>-viewfield = 'KEYS'.
    <dd27p>-tabname   = /apmg/if_apm_persist_apm=>c_tabname.
    <dd27p>-fieldname = 'KEYS'.
    <dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = /apmg/if_apm_persist_apm=>c_lock
        dd25v_wa          = dd25v
      TABLES
        dd26e_tab         = dd26e
        dd27p_tab         = dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    DATA(obj_name) = CONV sobj_name( /apmg/if_apm_persist_apm=>c_lock ).

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = /apmg/if_apm_persist_apm=>c_devclass
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = /apmg/if_apm_persist_apm=>c_lock
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Error activating { /apmg/if_apm_persist_apm=>c_lock }|.
    ENDIF.

  ENDMETHOD.


  METHOD lock_delete.

    delete_ddic(
      objtype = 'L'
      objname = /apmg/if_apm_persist_apm=>c_lock ).

  ENDMETHOD.


  METHOD lock_exists.

    SELECT COUNT(*) FROM dd25l INTO @DATA(count)
      WHERE viewname = @/apmg/if_apm_persist_apm=>c_lock.
    result = xsdbool( count > 0 ).

  ENDMETHOD.


  METHOD logo_create.

    DATA objs_table TYPE STANDARD TABLE OF v_obj_s WITH DEFAULT KEY.

    DATA(objh) = VALUE objh(
      objectname = /apmg/if_apm_persist_apm=>c_zapm
      objecttype = 'L'
      objcateg   = 'APPL'
      checkid    = 'L'
      objnamelen = '30'
      objtransp  = '2'
      luser      = sy-uname
      ldate      = sy-datum
      objcharset = '1' ).

    DATA(objt) = VALUE objt(
      language   = /apmg/if_apm_persist_apm=>c_english
      objectname = /apmg/if_apm_persist_apm=>c_zapm
      objecttype = 'L'
      ddtext     = 'apm' ).

    DATA(objs) = VALUE objs(
      objectname = /apmg/if_apm_persist_apm=>c_zapm
      objecttype = 'L'
      tabname    = /apmg/if_apm_persist_apm=>c_tabname
      ddic       = abap_true
      prim_table = abap_true ).

    APPEND objs TO objs_table.

    DATA(objsl) = VALUE objsl(
      objectname = /apmg/if_apm_persist_apm=>c_zapm
      objecttype = 'L'
      trwcount   = '01'
      tpgmid     = 'R3TR'
      tobject    = 'TABU'
      tobj_name  = /apmg/if_apm_persist_apm=>c_tabname
      tobjkey    = '/&/*'
      masknlen   = 7
      maskklen   = 2
      prim_table = abap_true ).

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = objh-objectname
        iv_objecttype         = objh-objecttype
        iv_maint_mode         = 'I'
        iv_objecttext         = objt-ddtext
        iv_objcateg           = objh-objcateg
        iv_objtransp          = objh-objtransp
        iv_no_correction      = abap_true
      TABLES
        tt_v_obj_s            = objs_table
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    " No API? No choice
    INSERT objsl FROM @objsl ##SUBRC_OK.

  ENDMETHOD.


  METHOD logo_delete.

    DATA(objh) = VALUE objh(
      objectname = /apmg/if_apm_persist_apm=>c_zapm
      objecttype = 'L' ).

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = objh-objectname
        iv_objecttype         = objh-objecttype
        iv_maint_mode         = 'D'
        iv_no_correction      = abap_true
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    " No API? No choice
    DELETE FROM objsl WHERE objectname = @/apmg/if_apm_persist_apm=>c_zapm AND objecttype = 'L' ##SUBRC_OK.

  ENDMETHOD.


  METHOD logo_exists.

    SELECT COUNT(*) FROM objh INTO @DATA(count)
      WHERE objectname = @/apmg/if_apm_persist_apm=>c_zapm AND objecttype = 'L'.
    result = xsdbool( count > 0 ).

  ENDMETHOD.


  METHOD table_create.

    DATA:
      subrc LIKE sy-subrc,
      dd03p TYPE STANDARD TABLE OF dd03p WITH KEY tabname fieldname position.

    DATA(dd02v) = VALUE dd02v(
      tabname    = /apmg/if_apm_persist_apm=>c_tabname
      ddlanguage = /apmg/if_apm_persist_apm=>c_english
      tabclass   = 'TRANSP'
      ddtext     = 'apm - Persistence'
      contflag   = 'A'
      exclass    = '1' ).

    DATA(dd09l) = VALUE dd09l(
      tabname   = /apmg/if_apm_persist_apm=>c_tabname
      as4local  = 'A'
      tabkat    = '1'
      tabart    = 'APPL0'
      bufallow  = 'X'
      pufferung = 'P' ).

    APPEND INITIAL LINE TO dd03p ASSIGNING FIELD-SYMBOL(<dd03p>).
    <dd03p>-tabname    = /apmg/if_apm_persist_apm=>c_tabname.
    <dd03p>-fieldname  = 'KEYS'. "KEY is not allowed
    <dd03p>-position   = '0001'.
    <dd03p>-keyflag    = 'X'.
    <dd03p>-notnull    = 'X'.
    <dd03p>-datatype   = 'CHAR'.
    <dd03p>-leng       = '000120'.
    <dd03p>-ddlanguage = /apmg/if_apm_persist_apm=>c_english.
    <dd03p>-ddtext     = 'Key'.

    APPEND INITIAL LINE TO dd03p ASSIGNING <dd03p>.
    <dd03p>-tabname    = /apmg/if_apm_persist_apm=>c_tabname.
    <dd03p>-fieldname  = 'VALUE'.
    <dd03p>-position   = '0002'.
    <dd03p>-datatype   = 'STRG'.
    <dd03p>-ddlanguage = /apmg/if_apm_persist_apm=>c_english.
    <dd03p>-ddtext     = 'Value'.

    APPEND INITIAL LINE TO dd03p ASSIGNING <dd03p>.
    <dd03p>-tabname    = /apmg/if_apm_persist_apm=>c_tabname.
    <dd03p>-fieldname  = 'LUSER'.
    <dd03p>-position   = '0003'.
    <dd03p>-rollname   = 'AS4USER'.
    <dd03p>-datatype   = 'CHAR'.
    <dd03p>-leng       = '000012'.
    <dd03p>-ddlanguage = /apmg/if_apm_persist_apm=>c_english.
    <dd03p>-ddtext     = 'Last Changed By'.

    APPEND INITIAL LINE TO dd03p ASSIGNING <dd03p>.
    <dd03p>-tabname    = /apmg/if_apm_persist_apm=>c_tabname.
    <dd03p>-fieldname  = 'TIMESTAMP'.
    <dd03p>-position   = '0004'.
    <dd03p>-rollname   = 'TIMESTAMPL'.
    <dd03p>-datatype   = 'DEC'.
    <dd03p>-leng       = '000021'.
    <dd03p>-decimals   = '00007'.
    <dd03p>-ddlanguage = /apmg/if_apm_persist_apm=>c_english.
    <dd03p>-ddtext     = 'Last Changed At'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = /apmg/if_apm_persist_apm=>c_tabname
        dd02v_wa          = dd02v
        dd09l_wa          = dd09l
      TABLES
        dd03p_tab         = dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    DATA(obj_name) = CONV sobj_name( /apmg/if_apm_persist_apm=>c_tabname ).

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = /apmg/if_apm_persist_apm=>c_devclass
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = /apmg/if_apm_persist_apm=>c_tabname
        auth_chk    = abap_false
      IMPORTING
        rc          = subrc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Error activating { /apmg/if_apm_persist_apm=>c_tabname }|.
    ENDIF.

  ENDMETHOD.


  METHOD table_delete.

    DATA:
      subrc TYPE sy-subrc,
      BEGIN OF dd02l,
        tabname  TYPE dd02l-tabname,
        tabclass TYPE dd02l-tabclass,
        sqltab   TYPE dd02l-sqltab,
      END OF dd02l.

    DATA(no_ask) = abap_true.

    SELECT SINGLE tabname, tabclass, sqltab FROM dd02l
      INTO CORRESPONDING FIELDS OF @dd02l
      WHERE tabname = @/apmg/if_apm_persist_apm=>c_tabname AND as4local = 'A' AND as4vers = '0000'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Table { /apmg/if_apm_persist_apm=>c_tabname } not found|.
    ENDIF.

    CALL FUNCTION 'DD_EXISTS_DATA'
      EXPORTING
        reftab          = dd02l-sqltab
        tabclass        = dd02l-tabclass
        tabname         = dd02l-tabname
      IMPORTING
        subrc           = subrc
      EXCEPTIONS
        missing_reftab  = 1
        sql_error       = 2
        buffer_overflow = 3
        unknown_error   = 4
        OTHERS          = 5.

    IF sy-subrc = 0 AND subrc = 0.
      no_ask = abap_false.
    ENDIF.

    delete_ddic(
      objtype = 'T'
      objname = /apmg/if_apm_persist_apm=>c_tabname
      no_ask  = no_ask ).

  ENDMETHOD.


  METHOD table_exists.

    SELECT COUNT(*) FROM dd02l INTO @DATA(count)
      WHERE tabname = @/apmg/if_apm_persist_apm=>c_tabname.
    result = xsdbool( count > 0 ).

  ENDMETHOD.


  METHOD uninstall.

    IF lock_exists( ) = abap_true.
      lock_delete( ).
    ENDIF.

    IF table_exists( ) = abap_true.
      table_delete( ).
    ENDIF.

    IF logo_exists( ) = abap_true.
      logo_delete( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
