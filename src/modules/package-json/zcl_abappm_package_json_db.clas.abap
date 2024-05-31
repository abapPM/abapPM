CLASS ZCL_ABAPPM_PACKAGE_JSON_DB DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS ZCL_ABAPPM_PACKAGE_JSON.

************************************************************************
* Package JSON - Persistence
*
* Copyright (c) Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_abappm,
        name  TYPE c LENGTH 30,
        type  TYPE c LENGTH 30,
        data  TYPE string,
        luser TYPE c LENGTH 12,
        ldate TYPE d,
        ltime TYPE t,
      END OF ty_abappm.

    CONSTANTS:
      c_zapm TYPE tadir-object  VALUE 'ZAPM'.

    CONSTANTS:
      c_devclass    TYPE c LENGTH 30 VALUE '$TMP',
      c_transaction TYPE c LENGTH 30 VALUE 'ZAPM',
      c_tabname     TYPE c LENGTH 30 VALUE 'ZABAPPM',
      c_lock        TYPE c LENGTH 30 VALUE 'EZABAPPM',
      c_english     TYPE c LENGTH 1  VALUE 'E'.

    CONSTANTS:
      BEGIN OF c_type,
        package_json TYPE ty_abappm-type VALUE 'PACKAGE_JSON',
        readme       TYPE ty_abappm-type VALUE 'README',
        favicon      TYPE ty_abappm-type VALUE 'FAVICON', " FUTURE
      END OF c_type.

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass.

    METHODS load
      IMPORTING
        !iv_type      TYPE ty_abappm-type DEFAULT c_type-package_json
      RETURNING
        VALUE(result) TYPE ty_abappm
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

    METHODS save
      IMPORTING
        !iv_type    TYPE ty_abappm-type DEFAULT c_type-package_json
        !iv_content TYPE ty_abappm-data
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

    METHODS delete
      IMPORTING
        !iv_type TYPE ty_abappm-type OPTIONAL
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_package TYPE devclass.

    METHODS tadir
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

    METHODS corr
      RAISING
        ZCX_ABAPPM_PACKAGE_JSON.

ENDCLASS.



CLASS ZCL_ABAPPM_PACKAGE_JSON_DB IMPLEMENTATION.


  METHOD constructor.
    mv_package = iv_package.
  ENDMETHOD.


  METHOD corr.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = mv_package
        object_class        = c_zapm
        devclass            = mv_package
        master_language     = 'E' " always English
        global_lock         = abap_true
        mode                = 'I' " insert
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

  ENDMETHOD.


  METHOD delete.

    IF iv_type IS INITIAL.
      DELETE FROM (c_tabname) WHERE name = mv_package.
    ELSE.
      DELETE FROM (c_tabname) WHERE name = mv_package AND type = iv_type.
    ENDIF.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE( |Error deleting { mv_package }| ).
    ENDIF.

    corr( ).

  ENDMETHOD.


  METHOD load.

    SELECT SINGLE * FROM (c_tabname) INTO result
      WHERE name = mv_package AND type = iv_type.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE( |Error loading { mv_package }| ).
    ENDIF.

  ENDMETHOD.


  METHOD save.

    DATA ls_abappm TYPE ty_abappm.

    ls_abappm-name = mv_package.
    ls_abappm-type = iv_type.
    ls_abappm-data = replace(
      val  = iv_content
      sub  = cl_abap_char_utilities=>cr_lf
      with = cl_abap_char_utilities=>newline
      occ  = 0 ).
    ls_abappm-luser = sy-uname.
    ls_abappm-ldate = sy-datum.
    ls_abappm-ltime = sy-uzeit.

    INSERT (c_tabname) FROM ls_abappm.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE( |Error saving { mv_package }| ).
    ENDIF.

    tadir( ).

    corr( ).

  ENDMETHOD.


  METHOD tadir.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = abap_false
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = c_zapm
        wi_tadir_obj_name              = mv_package
        wi_tadir_devclass              = mv_package
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_PACKAGE_JSON=>RAISE_T100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
