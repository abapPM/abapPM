CLASS ZCL_ABAPPM_PERSIST_APM DEFINITION
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

    INTERFACES ZIF_ABAPPM_PERSIST_APM.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_PERSIST_APM.

    CLASS-METHODS injector
      IMPORTING
        !ii_mock TYPE REF TO ZIF_ABAPPM_PERSIST_APM.

    CLASS-METHODS validate_key
      IMPORTING
        !iv_key       TYPE clike
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS explain_key
      IMPORTING
        !iv_key       TYPE clike
      RETURNING
        VALUE(result) TYPE ZIF_ABAPPM_PERSIST_APM=>TY_EXPLAINED.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO ZIF_ABAPPM_PERSIST_APM.

ENDCLASS.



CLASS ZCL_ABAPPM_PERSIST_APM IMPLEMENTATION.


  METHOD explain_key.

    DATA:
      lv_key_type TYPE string,
      lv_name     TYPE string,
      lv_extra    TYPE string.

    SPLIT iv_key AT ':' INTO lv_key_type lv_name lv_extra.

    CASE lv_key_type.
      WHEN ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-PACKAGE.
        result-key_type    = 'Package'.
        result-description = lcl_persist_utils=>get_package_description( lv_name ).

        IF lv_extra = ZIF_ABAPPM_PERSIST_APM=>C_KEY_EXTRA-PACKAGE_JSON.
          result-extra        = 'Package JSON'.
          result-content_type = ZIF_ABAPPM_PERSIST_APM=>C_CONTENT_TYPE-JSON.
        ELSEIF lv_extra = ZIF_ABAPPM_PERSIST_APM=>C_KEY_EXTRA-PACKAGE_README.
          result-extra        = 'Readme'.
          result-content_type = ZIF_ABAPPM_PERSIST_APM=>C_CONTENT_TYPE-MARKDOWN.
        ELSE.
          " Should not happen. Open issue
          result-extra        = 'Unknown key extra'.
          result-content_type = ZIF_ABAPPM_PERSIST_APM=>C_CONTENT_TYPE-TEXT.
        ENDIF.

      WHEN ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-SETTINGS.
        IF lv_name = ZIF_ABAPPM_PERSIST_APM=>C_KEY_NAME-GLOBAL_SETTINGS.
          result-key_type    = 'Global Settings'.
          result-description = 'For All Users'.
        ELSE.
          result-key_type    = 'Personal Settings'.
          result-description = lcl_persist_utils=>get_user_description( lv_name ).
        ENDIF.
        result-content_type = ZIF_ABAPPM_PERSIST_APM=>C_CONTENT_TYPE-JSON.

      WHEN OTHERS.
        result-key_type     = 'Unknown type of key'.
        result-content_type = ZIF_ABAPPM_PERSIST_APM=>C_CONTENT_TYPE-TEXT.

    ENDCASE.

  ENDMETHOD.


  METHOD get_instance.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance TYPE ZCL_ABAPPM_PERSIST_APM.
    ENDIF.
    result = go_instance.
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
      ( lv_key_type = ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-PACKAGE OR
        lv_key_type = ZIF_ABAPPM_PERSIST_APM=>C_KEY_TYPE-SETTINGS ) ).

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PERSIST_APM~DELETE.

    DELETE FROM (zif_persist_apm=>c_tabname) WHERE keys = iv_key.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_ERROR=>RAISE( |Error deleting { iv_key }| ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PERSIST_APM~LIST.

    DATA:
      lt_data   TYPE STANDARD TABLE OF ZIF_ABAPPM_PERSIST_APM=>TY_ZABAPPM WITH DEFAULT KEY,
      ls_result LIKE LINE OF result.

    FIELD-SYMBOLS <ls_data> LIKE LINE OF lt_data.

    IF iv_filter IS INITIAL.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE lt_data
        WHERE timestamp BETWEEN iv_from AND iv_to
        ORDER BY PRIMARY KEY.
    ELSE.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE lt_data
        WHERE timestamp BETWEEN iv_from AND iv_to AND keys LIKE iv_filter
        ORDER BY PRIMARY KEY.
    ENDIF.

    LOOP AT lt_data ASSIGNING <ls_data>.
      CLEAR ls_result.
      ls_result-keys      = <ls_data>-keys.
      ls_result-value     = <ls_data>-value.
      ls_result-user      = <ls_data>-luser.
      ls_result-timestamp = <ls_data>-timestamp.
      SPLIT <ls_data>-keys AT ':' INTO ls_result-key_type ls_result-key_name ls_result-key_extra.
      INSERT ls_result INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PERSIST_APM~LOAD.
    SELECT SINGLE * FROM (zif_persist_apm=>c_tabname) INTO result WHERE keys = iv_key.
    IF sy-subrc <> 0.
      ZCX_ABAPPM_ERROR=>RAISE( |Error loading { iv_key }| ).
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_ABAPPM_PERSIST_APM~LOCK.

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
      ZCX_ABAPPM_ERROR=>RAISE_T100( ).
    ENDIF.

    lv_dummy_update_function = lcl_persist_utils=>get_update_function( ).

    " trigger dummy update task to automatically release locks at commit
    CALL FUNCTION lv_dummy_update_function IN UPDATE TASK.

  ENDMETHOD.


  METHOD ZIF_ABAPPM_PERSIST_APM~SAVE.

    DATA ls_abappm TYPE ZIF_ABAPPM_PERSIST_APM=>TY_ZABAPPM.

    IF validate_key( iv_key ) = abap_false.
      ZCX_ABAPPM_ERROR=>RAISE( |Invalid key { iv_key }| ).
    ENDIF.

    ls_abappm-keys  = iv_key.
    ls_abappm-value = replace(
      val  = iv_value
      sub  = cl_abap_char_utilities=>cr_lf
      with = cl_abap_char_utilities=>newline
      occ  = 0 ).
    ls_abappm-luser = sy-uname.
    GET TIME STAMP FIELD ls_abappm-timestamp.

    UPDATE (zif_persist_apm=>c_tabname) FROM ls_abappm.
    IF sy-subrc <> 0.
      INSERT (zif_persist_apm=>c_tabname) FROM ls_abappm.
      IF sy-subrc <> 0.
        ZCX_ABAPPM_ERROR=>RAISE( |Error saving { iv_key }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
