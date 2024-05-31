CLASS ZCL_ABAPPM_PACKAGE_JSON_SETUP DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS tobj_create
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

    CLASS-METHODS tobj_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS table_create
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

    CLASS-METHODS table_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS lock_create
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

    CLASS-METHODS lock_exists
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS ZCL_ABAPPM_PACKAGE_JSON_SETUP IMPLEMENTATION.


  METHOD lock_create.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      ls_dd25v    TYPE dd25v,
      lt_dd26e    TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY,
      lt_dd27p    TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_dd26e> LIKE LINE OF lt_dd26e,
      <ls_dd27p> LIKE LINE OF lt_dd27p.

    ls_dd25v-viewname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK.
    ls_dd25v-aggtype    = 'E'.
    ls_dd25v-roottab    = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    ls_dd25v-ddlanguage = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ENGLISH.
    ls_dd25v-ddtext     = 'apm - Persistence'.

    APPEND INITIAL LINE TO lt_dd26e ASSIGNING <ls_dd26e>.
    <ls_dd26e>-viewname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK.
    <ls_dd26e>-tabname    = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd26e>-tabpos     = '0001'.
    <ls_dd26e>-fortabname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK.
    <ls_dd27p>-objpos    = '0001'.
    <ls_dd27p>-viewfield = 'NAME'.
    <ls_dd27p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd27p>-fieldname = 'NAME'.
    <ls_dd27p>-keyflag   = abap_true.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK.
    <ls_dd27p>-objpos    = '0002'.
    <ls_dd27p>-viewfield = 'TYPE'.
    <ls_dd27p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd27p>-fieldname = 'TYPE'.
    <ls_dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK
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
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

    lv_obj_name = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_DEVCLASS
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE( |Error activating { ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK }| ).
    ENDIF.

  ENDMETHOD.


  METHOD lock_exists.

    DATA lv_viewname TYPE dd25l-viewname.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_LOCK.
    result = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD run.

    IF table_exists( ) = abap_false.
      table_create( ).
    ENDIF.

    IF lock_exists( ) = abap_false.
      lock_create( ).
    ENDIF.

    IF tobj_exists( ) = abap_false.
      tobj_create( ).
    ENDIF.

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

    ls_dd02v-tabname    = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    ls_dd02v-ddlanguage = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ENGLISH.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = 'apm - Persistence'.
    ls_dd02v-contflag   = 'A'.
    ls_dd02v-exclass    = '1'.

    ls_dd09l-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    ls_dd09l-as4local  = 'A'.
    ls_dd09l-tabkat    = '1'.
    ls_dd09l-tabart    = 'APPL0'.
    ls_dd09l-bufallow  = 'X'.
    ls_dd09l-pufferung = 'P'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd03p>-fieldname = 'NAME'.
    <ls_dd03p>-position  = '0001'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-rollname  = 'DEVCLASS'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000030'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd03p>-fieldname = 'TYPE'.
    <ls_dd03p>-position  = '0002'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000030'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd03p>-fieldname = 'DATA'.
    <ls_dd03p>-position  = '0003'.
    <ls_dd03p>-datatype  = 'STRG'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd03p>-fieldname = 'LUSER'.
    <ls_dd03p>-position  = '0004'.
    <ls_dd03p>-rollname  = 'AS4USER'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd03p>-fieldname = 'LDATE'.
    <ls_dd03p>-position  = '0005'.
    <ls_dd03p>-rollname  = 'AS4DATE'.
    <ls_dd03p>-datatype  = 'DATS'.
    <ls_dd03p>-leng      = '000008'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_dd03p>-fieldname = 'LTIME'.
    <ls_dd03p>-position  = '0006'.
    <ls_dd03p>-rollname  = 'AS4TIME'.
    <ls_dd03p>-datatype  = 'TIMS'.
    <ls_dd03p>-leng      = '000006'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME
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
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

    lv_obj_name = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_DEVCLASS
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME
        auth_chk    = abap_false
      IMPORTING
        rc          = lv_subrc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR lv_subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE( |Error activating { ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME }| ).
    ENDIF.

  ENDMETHOD.


  METHOD table_exists.

    DATA lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    result = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD tobj_create.

    DATA:
      ls_objh  TYPE objh,
      ls_objt  TYPE objt,
      lt_objs  TYPE tt_objs,
      lt_objsl TYPE tt_objsl,
      lt_objm  TYPE tt_objm.

    FIELD-SYMBOLS:
      <ls_objs>  TYPE objs,
      <ls_objsl> TYPE objsl,
      <ls_objm>  TYPE objm.

    ls_objh-objectname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ZAPM.
    ls_objh-objecttype = 'L'.
    ls_objh-objcateg   = 'APPL'.
    ls_objh-checkid    = 'L'.
    ls_objh-objnamelen = 30.
    ls_objh-objtransp  = '2'.
    ls_objh-objcharset = '1'.

    ls_objt-language   = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ENGLISH.
    ls_objt-objectname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ZAPM.
    ls_objt-objecttype = 'L'.
    ls_objt-ddtext     = 'apm'.

    APPEND INITIAL LINE TO lt_objs ASSIGNING <ls_objs>.
    <ls_objs>-objectname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ZAPM.
    <ls_objs>-objecttype = 'L'.
    <ls_objs>-tabname    = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_objs>-ddic       = abap_true.
    <ls_objs>-prim_table = abap_true.

    APPEND INITIAL LINE TO lt_objsl ASSIGNING <ls_objsl>.
    <ls_objsl>-objectname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ZAPM.
    <ls_objsl>-objecttype = 'L'.
    <ls_objsl>-trwcount   = '01'.
    <ls_objsl>-tpgmid     = 'R3TR'.
    <ls_objsl>-tobject    = 'TABU'.
    <ls_objsl>-tobj_name  = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_TABNAME.
    <ls_objsl>-tobjkey    = '/&/*'.
    <ls_objsl>-masknlen   = 7.
    <ls_objsl>-maskklen   = 2.
    <ls_objsl>-prim_table = abap_true.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_korrnum            = ''
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'I'
        iv_objecttext         = ls_objt-ddtext
        iv_objcateg           = ls_objh-objcateg
        iv_objtransp          = ls_objh-objtransp
        iv_devclass           = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_DEVCLASS
      TABLES
        tt_v_obj_s            = lt_objs
        tt_objm               = lt_objm
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      " TOBJ has to be saved/generated after the DDIC tables have been activated
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

    CALL FUNCTION 'OBJ_SET_IMPORTABLE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_importable         = ls_objh-importable
      EXCEPTIONS
        object_not_defined    = 1
        invalid               = 2
        transport_error       = 3
        object_enqueue_failed = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

    UPDATE objh SET objtransp = ls_objh-objtransp
      WHERE objectname = ls_objh-objectname AND objecttype = ls_objh-objecttype.

  ENDMETHOD.


  METHOD tobj_exists.

    DATA lv_objectname TYPE objh-objectname.

    SELECT SINGLE objectname FROM objh INTO lv_objectname
      WHERE objectname = ZCL_ABAPPM_PACKAGE_JSON_DB=>C_ZAPM AND objecttype = 'L'.
    result = boolc( sy-subrc = 0 ).

  ENDMETHOD.
ENDCLASS.
