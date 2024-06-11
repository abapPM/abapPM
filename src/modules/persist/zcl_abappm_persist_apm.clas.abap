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

ENDCLASS.



CLASS zcl_abappm_persist_apm IMPLEMENTATION.


  METHOD get_instance.
    IF go_instance IS INITIAL.
      CREATE OBJECT go_instance TYPE zcl_abappm_persist_apm.
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
      ( lv_key_type = zif_abappm_persist_apm=>c_key_type-package OR
        lv_key_type = zif_abappm_persist_apm=>c_key_type-settings ) ).

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~delete.

    DELETE FROM (zif_persist_apm=>c_tabname) WHERE keys = iv_key.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error deleting { iv_key }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~explain.

    DATA:
      lv_key_type TYPE string,
      lv_name     TYPE string,
      lv_extra    TYPE string.

    SPLIT iv_key AT ':' INTO lv_key_type lv_name lv_extra.

    CASE lv_key_type.
      WHEN zif_abappm_persist_apm=>c_key_type-package.
        result-key_type    = 'Package'.
        result-description = lcl_persist_utils=>get_package_description( lv_name ).

        IF lv_extra = zif_abappm_persist_apm=>c_key_extra-package_json.
          result-extra = 'Package JSON'.
        ELSEIF lv_extra = zif_abappm_persist_apm=>c_key_extra-package_readme.
          result-extra = 'Readme'.
        ELSE.
          " Should not happen. Open issue
          result-extra = 'Unknown key extra'.
        ENDIF.

      WHEN zif_abappm_persist_apm=>c_key_type-settings.
        IF lv_name = zif_abappm_persist_apm=>c_key_extra-global_settings.
          result-key_type = 'Global Settings'.
        ELSE.
          result-key_type = 'Personal Settings'.
          result-description = lcl_persist_utils=>get_user_description( lv_name ).
        ENDIF.

      WHEN OTHERS.
        result-key_type = 'Unknown type of key'.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~explain_formatted.

    DATA ls_explained TYPE zif_abappm_persist_apm~ty_explained.

    ls_explained = zif_abappm_persist_apm~explain( iv_key ).

    IF ls_explained-key_type IS NOT INITIAL.
      ls_explained-key_type = |{ ls_explained-key_type }: |.
    ENDIF.

    IF ls_explained-extra IS NOT INITIAL.
      ls_explained-extra = | ({ ls_explained-extra })|.
    ENDIF.

    result = |{ ls_explained-key_type }<br/><strong>{ ls_explained-description }</strong><br/>{ ls_explained-extra }|.

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~list.

    DATA:
      lt_data   TYPE STANDARD TABLE OF zif_abappm_persist_apm=>ty_zabappm WITH DEFAULT KEY,
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


  METHOD zif_abappm_persist_apm~load.
    SELECT SINGLE * FROM (zif_persist_apm=>c_tabname) INTO result WHERE keys = iv_key.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error loading { iv_key }| ).
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
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

    lv_dummy_update_function = lcl_persist_utils=>get_update_function( ).

    " trigger dummy update task to automatically release locks at commit
    CALL FUNCTION lv_dummy_update_function IN UPDATE TASK.

  ENDMETHOD.


  METHOD zif_abappm_persist_apm~save.

    DATA ls_abappm TYPE zif_abappm_persist_apm=>ty_zabappm.

    IF validate_key( iv_key ) = abap_false.
      zcx_abappm_error=>raise( |Invalid key { iv_key }| ).
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
        zcx_abappm_error=>raise( |Error saving { iv_key }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
