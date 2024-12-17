CLASS zcl_abappm_installer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_enum_zip,
        local    TYPE i VALUE 0,
        internet TYPE i VALUE 1,
        server   TYPE i VALUE 2,
        data     TYPE i VALUE 3,
        registry TYPE i VALUE 4,
      END OF c_enum_zip.

    CONSTANTS:
      BEGIN OF c_enum_package,
        default       TYPE i VALUE 0,
        local         TYPE i VALUE 1,
        transportable TYPE i VALUE 2,
      END OF c_enum_package.

    CONSTANTS:
      BEGIN OF c_enum_transport,
        prompt   TYPE i VALUE 0,
        existing TYPE i VALUE 1,
      END OF c_enum_transport.

    CONSTANTS:
      BEGIN OF c_enum_folder_logic,
        default TYPE i VALUE 0,
        prefix  TYPE i VALUE 1,
        mixed   TYPE i VALUE 2,
        full    TYPE i VALUE 3,
      END OF c_enum_folder_logic.

    CLASS-METHODS init
      IMPORTING
        !iv_tabname TYPE tabname OPTIONAL
        !iv_lock    TYPE viewname OPTIONAL
        !iv_name    TYPE string OPTIONAL
        !iv_names   TYPE string OPTIONAL.

    CLASS-METHODS install
      IMPORTING
        !iv_apm_name          TYPE string OPTIONAL
        !iv_apm_version       TYPE string OPTIONAL
        !iv_enum_zip          TYPE i OPTIONAL
        !iv_name              TYPE char255 OPTIONAL
        !iv_data              TYPE xstring OPTIONAL
        !iv_enum_package      TYPE i OPTIONAL
        !iv_package           TYPE devclass OPTIONAL
        !iv_dlvunit           TYPE dlvunit OPTIONAL
        !iv_devlayer          TYPE devlayer OPTIONAL
        !iv_enum_transport    TYPE i OPTIONAL
        !iv_transport         TYPE trkorr OPTIONAL
        !iv_user              TYPE char255 OPTIONAL
        !iv_password          TYPE char255 OPTIONAL
        !iv_proxy_host        TYPE char255 OPTIONAL
        !iv_proxy_service     TYPE char5 OPTIONAL
        !iv_proxy_user        TYPE char255 OPTIONAL
        !iv_proxy_password    TYPE char255 OPTIONAL
        !iv_enum_folder_logic TYPE i OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall
      IMPORTING
        !iv_apm  TYPE abap_bool DEFAULT abap_false
        !iv_name TYPE zif_abappm_installer_def=>ty_name OPTIONAL
        !iv_pack TYPE zif_abappm_installer_def=>ty_pack OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS list
      RAISING
        zcx_abappm_error.

    CLASS-METHODS f4
      RETURNING
        VALUE(rs_inst) TYPE zif_abappm_installer_def=>ty_inst
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      BEGIN OF gs_apm,
        name    TYPE string,
        version TYPE string,
      END OF gs_apm,
      go_db           TYPE REF TO zcl_abapinst_persistence,
      gt_remote       TYPE zif_abapgit_git_definitions=>ty_files_tt,
      gs_inst         TYPE zif_abappm_installer_def=>ty_inst,
      go_dot          TYPE REF TO zcl_abapgit_dot_abapgit,
      gi_log          TYPE REF TO zif_abapgit_log,
      gs_packaging    TYPE zif_abappm_installer_dot=>ty_packaging,
      gt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      gv_name         TYPE string,
      gv_names        TYPE string,
      gt_clmcus       TYPE STANDARD TABLE OF clmcus WITH DEFAULT KEY.

    CONSTANTS:
      c_success TYPE sy-msgty VALUE 'S' ##NO_TEXT,
      c_warning TYPE sy-msgty VALUE 'W' ##NO_TEXT,
      c_error   TYPE sy-msgty VALUE 'E' ##NO_TEXT.

    CLASS-METHODS _system_check
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _clear.

    CLASS-METHODS _nothing_found
      IMPORTING
        !it_list         TYPE ANY TABLE
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS _files
      IMPORTING
        !iv_enum_zip       TYPE i
        !iv_name           TYPE char255 OPTIONAL
        !iv_data           TYPE xstring OPTIONAL
        !iv_user           TYPE char255 OPTIONAL
        !iv_password       TYPE char255 OPTIONAL
        !iv_proxy_host     TYPE char255 OPTIONAL
        !iv_proxy_service  TYPE char5 OPTIONAL
        !iv_proxy_user     TYPE char255 OPTIONAL
        !iv_proxy_password TYPE char255 OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _packaging
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _sap_package
      IMPORTING
        !iv_enum_package TYPE i
        !iv_package      TYPE devclass OPTIONAL
        !iv_dlvunit      TYPE dlvunit OPTIONAL
        !iv_devlayer     TYPE devlayer OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _check
      IMPORTING
        !iv_force TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _check_version
      IMPORTING
        !is_new_version       TYPE zif_abappm_installer_def=>ty_version
        !is_installed_version TYPE zif_abappm_installer_def=>ty_version
        !iv_force             TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _check_requirements
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _check_dependencies
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _folder_logic
      IMPORTING
        !iv_enum_folder_logic TYPE i
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport
      IMPORTING
        !iv_enum_transport TYPE i
        !iv_transport      TYPE trkorr OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport_get
      RETURNING
        VALUE(rv_trkorr) TYPE trkorr
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport_check
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport_reset.

    CLASS-METHODS _namespaces
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _confirm_messages.

    CLASS-METHODS _restore_messages.

    CLASS-METHODS _deserialize_objects
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS _deserialize_data
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS _save
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _load
      IMPORTING
        !iv_name       TYPE zif_abappm_installer_def=>ty_name OPTIONAL
        !iv_pack       TYPE zif_abappm_installer_def=>ty_pack OPTIONAL
      RETURNING
        VALUE(rs_inst) TYPE zif_abappm_installer_def=>ty_inst
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _delete
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _log_start.

    CLASS-METHODS _log_end
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _final_message
      IMPORTING
        !iv_type TYPE string.

    CLASS-METHODS _find_remote_dot_abapgit
      IMPORTING
        !it_remote    TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _find_remote_dot_apack
      IMPORTING
        !it_remote    TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _find_remote_namespaces
      RETURNING
        VALUE(rt_remote) TYPE zif_abapgit_git_definitions=>ty_files_tt.

    CLASS-METHODS _find_remote_data_config
      RETURNING
        VALUE(ri_config) TYPE REF TO zif_abapgit_data_config
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS _check_uninstalled
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CLASS-METHODS _uninstall_sotr
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CLASS-METHODS _uninstall_sots
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

ENDCLASS.



CLASS zcl_abappm_installer IMPLEMENTATION.


  METHOD f4.

    DATA:
      lt_list     TYPE zif_abappm_installer_def=>ty_list,
      lt_selected LIKE lt_list,
      lo_popup    TYPE REF TO zcl_abappm_installer_popups,
      lt_columns  TYPE zcl_abappm_installer_popups=>ty_alv_column_tt,
      lv_question TYPE string,
      lv_answer   TYPE sy-input.

    FIELD-SYMBOLS:
      <ls_column> LIKE LINE OF lt_columns.

    init( ).

***    lt_list = go_db->list( ).

    CHECK _nothing_found( lt_list ) IS INITIAL.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'NAME'.
    <ls_column>-text   = 'Name'.
    <ls_column>-length = 30.
    <ls_column>-key    = abap_true.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'PACK'.
    <ls_column>-text   = 'Package'.
    <ls_column>-length = 30.
    <ls_column>-key    = abap_true.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'VERSION'.
    <ls_column>-text   = 'Version'.
    <ls_column>-length = 15.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'DESCRIPTION'.
    <ls_column>-text   = 'Description'.
    <ls_column>-length = 60.

    CREATE OBJECT lo_popup.

    TRY.
        lo_popup->popup_to_select_from_list(
          EXPORTING
            it_list               = lt_list
            iv_title              = sy-title
            iv_header_text        = |Select the { gv_name } that you want to uninstall:|
            iv_end_column         = 150
            iv_striped_pattern    = abap_true
            iv_optimize_col_width = abap_false
            iv_selection_mode     = if_salv_c_selection_mode=>single
            it_columns_to_display = lt_columns
          IMPORTING
            et_list               = lt_selected ).
      CATCH zcx_abappm_error.
        RETURN.
    ENDTRY.

    IF lt_selected IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE lt_selected INTO rs_inst INDEX 1.
    ASSERT sy-subrc = 0.

    TRY.
        lv_question = |Are you sure, you want to uninstall "{ rs_inst-description } ({ rs_inst-name })"?|.

        lv_answer = lo_popup->popup_to_confirm(
          iv_title          = sy-title
          iv_question       = lv_question
          iv_default_button = '2' ).

        IF lv_answer <> '1'.
          CLEAR rs_inst.
        ENDIF.
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD init.

    IF go_db IS NOT BOUND.
      IF iv_tabname IS INITIAL AND iv_lock IS INITIAL.
        go_db = zcl_abapinst_persistence=>get_instance( ).
      ELSE.
        go_db = zcl_abapinst_persistence=>get_instance(
          iv_tabname = iv_tabname
          iv_lock    = iv_lock ).
      ENDIF.
    ENDIF.

    IF iv_name IS NOT INITIAL OR iv_names IS NOT INITIAL.
      gv_name  = iv_name.
      gv_names = iv_names.
    ENDIF.

  ENDMETHOD.


  METHOD install.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception.

    gs_apm-name    = iv_apm_name.
    gs_apm-version = iv_apm_version.

    init( ).

    TRY.
        _clear( ).

        _log_start( ).

        _system_check( ).

        _files(
          iv_enum_zip       = iv_enum_zip
          iv_name           = iv_name
          iv_data           = iv_data
          iv_user           = iv_user
          iv_password       = iv_password
          iv_proxy_host     = iv_proxy_host
          iv_proxy_service  = iv_proxy_service
          iv_proxy_user     = iv_proxy_user
          iv_proxy_password = iv_proxy_password ).

        _packaging( ).

        _sap_package(
          iv_enum_package = iv_enum_package
          iv_package      = iv_package ).

        _check( ).

        _folder_logic( iv_enum_folder_logic ).

        _transport(
          iv_enum_transport = iv_enum_transport
          iv_transport      = iv_transport ).

        _confirm_messages( ).

        _namespaces( ).

        _deserialize_objects( ).

        _deserialize_data( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        _transport_reset( ).

        gi_log->add_exception( lx_error ).
    ENDTRY.

    TRY.
        _log_end( ).

        IF gs_apm-name IS INITIAL.
          _save( ).
        ENDIF.

        _restore_messages( ).

        _final_message( 'Installation' ).

      CATCH zcx_abapgit_exception INTO lx_error.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD list.

    DATA:
      lt_list          TYPE zif_abappm_installer_def=>ty_list,
      lo_list          TYPE REF TO cl_salv_table,
      lo_disp_settings TYPE REF TO cl_salv_display_settings,
      lo_functions     TYPE REF TO cl_salv_functions,
      lo_columns       TYPE REF TO cl_salv_columns_table,
      ls_column        TYPE salv_s_column_ref,
      lt_columns       TYPE salv_t_column_ref,
      lo_column        TYPE REF TO cl_salv_column,
      lr_column        TYPE REF TO cl_salv_column_table,
      lx_error         TYPE REF TO cx_salv_error.

    FIELD-SYMBOLS:
      <ls_list> LIKE LINE OF lt_list.

    init( ).

***    lt_list = go_db->list( ).

    CHECK _nothing_found( lt_list ) IS INITIAL.

    LOOP AT lt_list ASSIGNING <ls_list>.
      CASE <ls_list>-status.
        WHEN space.
          <ls_list>-status = icon_led_inactive.
        WHEN c_success.
          <ls_list>-status = icon_led_green.
        WHEN c_warning.
          <ls_list>-status = icon_led_yellow.
        WHEN OTHERS.
          <ls_list>-status = icon_led_red.
      ENDCASE.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_list
          CHANGING
            t_table      = lt_list ).

        lo_functions = lo_list->get_functions( ).
        lo_functions->set_all( ).

        lo_columns = lo_list->get_columns( ).
        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( 'NAME' ).
        lo_column->set_medium_text( 'Name' ).
        lo_column->set_output_length( 30 ).
        lr_column ?= lo_columns->get_column( 'NAME' ).
        lr_column->set_key( ).

        lo_column = lo_columns->get_column( 'PACK' ).
        lo_column->set_medium_text( 'SAP Package' ).
        lo_column->set_output_length( 30 ).
        lr_column ?= lo_columns->get_column( 'PACK' ).
        lr_column->set_key( ).

        lo_column = lo_columns->get_column( 'VERSION' ).
        lo_column->set_medium_text( 'Version' ).
        lo_column->set_output_length( 12 ).

        lt_columns = lo_columns->get( ).
        LOOP AT lt_columns INTO ls_column WHERE columnname CP 'SEM_VERSION-*'.
          ls_column-r_column->set_technical( ).
        ENDLOOP.

        lo_column = lo_columns->get_column( 'STATUS' ).
        lo_column->set_medium_text( 'Status' ).
        lo_column->set_output_length( 6 ).

        lo_column = lo_columns->get_column( 'DESCRIPTION' ).
        lo_column->set_medium_text( 'Description' ).
        lo_column->set_output_length( 60 ).

        lo_column = lo_columns->get_column( 'SOURCE_TYPE' ).
        lo_column->set_medium_text( 'Type' ).
        lo_column->set_output_length( 10 ).

        lo_column = lo_columns->get_column( 'SOURCE_NAME' ).
        lo_column->set_medium_text( 'Source' ).
        lo_column->set_output_length( 50 ).

        lo_column = lo_columns->get_column( 'TRANSPORT' ).
        lo_column->set_medium_text( 'Transport' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'FOLDER_LOGIC' ).
        lo_column->set_medium_text( 'Folder Logic' ).
        lo_column->set_output_length( 10 ).

        lo_column = lo_columns->get_column( 'INSTALLED_LANGU' ).
        lo_column->set_medium_text( 'Installed Language' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'INSTALLED_BY' ).
        lo_column->set_medium_text( 'Installed By' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'INSTALLED_AT' ).
        lo_column->set_short_text( 'Installed' ).
        lo_column->set_medium_text( 'Installed At' ).
        lo_column->set_output_length( 18 ).

        lo_column = lo_columns->get_column( 'UPDATED_BY' ).
        lo_column->set_medium_text( 'Updated By' ).
        lo_column->set_output_length( 12 ).

        lo_column = lo_columns->get_column( 'UPDATED_AT' ).
        lo_column->set_short_text( 'Updated' ).
        lo_column->set_medium_text( 'Updated At' ).
        lo_column->set_output_length( 18 ).

        lo_disp_settings = lo_list->get_display_settings( ).
        lo_disp_settings->set_list_header( sy-title ).
        lo_disp_settings->set_fit_column_to_table_size( ).

        lo_list->display( ).
      CATCH cx_salv_error INTO lx_error.
        zcx_abappm_error=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD uninstall.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception,
      lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    IF iv_apm IS INITIAL.
      init( ).
    ENDIF.

    TRY.
        _clear( ).

        _log_start( ).

        _system_check( ).

        IF iv_apm IS INITIAL.
          gs_inst = _load(
            iv_name = iv_name
            iv_pack = iv_pack ).

          IF gs_inst IS INITIAL.
            zcx_abappm_error=>raise( |Package { iv_name } ({ iv_pack }) not found| ).
          ENDIF.

          " TODO: needs to work for apm
          _transport( c_enum_transport-prompt ).
        ELSE.
          gs_inst-pack = iv_pack.
        ENDIF.

        _confirm_messages( ).

        " A few tries to tackle dependencies
        DO 3 TIMES.
          lt_tadir = zcl_abapgit_factory=>get_tadir( )->read( gs_inst-pack ).

          DELETE lt_tadir WHERE object = 'NSPC'.

          IF lt_tadir IS NOT INITIAL.
            _uninstall_sotr( lt_tadir ).

            _uninstall_sots( lt_tadir ).

            zcl_abapinst_objects=>delete(
              it_tadir     = lt_tadir
              iv_transport = gs_inst-transport
              ii_log       = gi_log ).
          ENDIF.
        ENDDO.

      CATCH zcx_abapgit_exception INTO lx_error.
        _transport_reset( ).

        gi_log->add_exception( lx_error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _check_uninstalled( lt_tadir ).

        IF iv_apm IS INITIAL.
          IF gs_inst-status = c_success.
            _delete( ).
          ELSE.
            _save( ).
          ENDIF.
        ENDIF.

        _restore_messages( ).

        _final_message( 'Uninstall' ).

      CATCH zcx_abapgit_exception INTO lx_error.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD _check.

    DATA ls_inst TYPE zif_abappm_installer_def=>ty_inst.

    IF gs_apm-name IS INITIAL.
      ls_inst = _load(
        iv_name = gs_inst-name
        iv_pack = gs_inst-pack ).

      IF ls_inst IS INITIAL.

        ls_inst = _load( iv_pack = gs_inst-pack ).

        IF ls_inst IS NOT INITIAL.
          zcx_abappm_error=>raise( |SAP package { gs_inst-pack } already contains a different { gv_name }| ).
        ENDIF.

      ELSE.

        _check_version(
          is_new_version       = gs_inst-sem_version
          is_installed_version = ls_inst-sem_version
          iv_force             = iv_force ).

      ENDIF.
    ENDIF.

    _check_requirements( ).

    _check_dependencies( ).

  ENDMETHOD.


  METHOD _check_dependencies.

    DATA:
      ls_deps LIKE LINE OF gs_packaging-dependencies,
      ls_inst TYPE zif_abappm_installer_def=>ty_inst,
      lv_comp TYPE i,
      lv_msg  TYPE string.

    LOOP AT gs_packaging-dependencies INTO ls_deps.

      ls_inst = _load( iv_name = |{ ls_deps-name }| ).

      IF ls_inst IS INITIAL.
        zcx_abappm_error=>raise( |Package { ls_deps-name } is a dependency and must be installed, first| ).
      ELSE.
        lv_comp = zcl_abapgit_version=>compare(
          is_a = ls_deps-sem_version
          is_b = ls_inst-sem_version ).
        IF lv_comp > 0.
          lv_msg = |Package { ls_deps-name } is a dependency and must be updated to { ls_deps-version }, first|.
          zcx_abappm_error=>raise( lv_msg ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _check_requirements.

    DATA:
      lv_met   TYPE c LENGTH 1,
      lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        lv_met = zcl_abapinst_requirements=>is_requirements_met( gt_requirements ).

        IF lv_met = zif_abapgit_definitions=>c_no.
          zcx_abappm_error=>raise( |Minimum requirements for software components have not been met| ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD _check_uninstalled.

    DATA:
      lv_msg   TYPE string,
      ls_tadir LIKE LINE OF it_tadir,
      lt_tadir LIKE it_tadir.

    CHECK it_tadir IS NOT INITIAL.

    SELECT pgmid object obj_name FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      FOR ALL ENTRIES IN it_tadir
      WHERE pgmid    = it_tadir-pgmid
        AND object   = it_tadir-object
        AND obj_name = it_tadir-obj_name ##TOO_MANY_ITAB_FIELDS.
    IF sy-subrc = 0.
      LOOP AT lt_tadir INTO ls_tadir WHERE object <> 'DEVC'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        gs_inst-status = c_warning.
        lv_msg = |Some objects could not be uninstalled. Uninstall the remaining objects |
              && |of pacakge { gs_inst-pack } manually|.
      ELSE.
        lv_msg = |Release the transport and deleted the remaining pacakge { gs_inst-pack } manually|.
      ENDIF.
      MESSAGE lv_msg TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD _check_version.

    DATA:
      lv_comp     TYPE i,
      lv_msg      TYPE string,
      lv_question TYPE string,
      lo_popup    TYPE REF TO zcl_abappm_installer_popups,
      lv_answer   TYPE c LENGTH 1.

    lv_comp = zcl_abapgit_version=>compare(
      is_a = is_new_version
      is_b = is_installed_version ).
    IF lv_comp <= 0.

      lv_msg = |{ gs_inst-name } is already installed (with same or newer version)|.
      lv_question = lv_msg  && '. Do you want to overwrite it?'.

      IF iv_force IS INITIAL.
        CREATE OBJECT lo_popup.

        lv_answer = lo_popup->popup_to_confirm(
          iv_title          = sy-title
          iv_question       = lv_question
          iv_default_button = '2' ).

        IF lv_answer <> '1'.
          zcx_abappm_error=>raise( lv_msg ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD _clear.
    CLEAR: gs_inst, gs_packaging, go_dot.
  ENDMETHOD.


  METHOD _confirm_messages.

    " Temporarily suppress certain messages that are not relevant for installation

    CONSTANTS c_toolflag_set TYPE funcname VALUE 'SCWG_TOOLFLAG_SET'.

    DATA ls_clmcus TYPE clmcus.

    " Set tool flag to avoid messages
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = c_toolflag_set
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      CALL FUNCTION c_toolflag_set.
    ENDIF.

    " Confirm message about modification mode (DT, CLM_INFORMATION)
    " and backup old state (see _restore_messages)
    SELECT * FROM clmcus INTO TABLE gt_clmcus WHERE username = sy-uname ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    ls_clmcus-username = sy-uname.
    ls_clmcus-obj_type = 'CLAS'.
    INSERT clmcus FROM ls_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    ls_clmcus-obj_type = 'INTF'.
    INSERT clmcus FROM ls_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    ls_clmcus-obj_type = 'METH'.
    INSERT clmcus FROM ls_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint

  ENDMETHOD.


  METHOD _delete.

***    go_db->delete(
***      iv_name = gs_inst-name
***      iv_pack = gs_inst-pack ).

  ENDMETHOD.


  METHOD _deserialize_data.

    DATA:
      lo_support   TYPE REF TO lcl_abapgit_data_supporter,
      lo_inject    TYPE REF TO zcl_abapgit_data_injector,
      li_config    TYPE REF TO zif_abapgit_data_config,
      li_deser     TYPE REF TO zif_abapgit_data_deserializer,
      ls_checks    TYPE zif_abapgit_definitions=>ty_deserialize_checks,
      ls_overwrite TYPE LINE OF zif_abapgit_definitions=>ty_deserialize_checks-overwrite,
      ls_result    TYPE LINE OF zif_abapgit_data_deserializer=>ty_results,
      lt_result    TYPE zif_abapgit_data_deserializer=>ty_results.

    CREATE OBJECT lo_support.
*** MBT FIXME
*    CREATE OBJECT lo_inject.
*    lo_inject->set_supporter( lo_support ).

    li_config = _find_remote_data_config( ).

    li_deser = zcl_abapgit_data_factory=>get_deserializer( ).

    lt_result = li_deser->deserialize(
      ii_config = li_config
      it_files  = gt_remote ).

    LOOP AT lt_result INTO ls_result.
      CLEAR ls_overwrite.
      ls_overwrite-obj_type = ls_result-type.
      ls_overwrite-obj_name = ls_result-name.
      ls_overwrite-decision = zif_abapgit_definitions=>c_yes.
      COLLECT ls_overwrite INTO ls_checks-overwrite.
    ENDLOOP.

    li_deser->actualize(
      is_checks = ls_checks
      it_result = lt_result ).

  ENDMETHOD.


  METHOD _deserialize_objects.

    zcl_abapinst_objects=>deserialize(
      iv_package   = gs_inst-pack
      iv_language  = gs_inst-installed_langu
      iv_transport = gs_inst-transport
      it_remote    = gt_remote
      io_dot       = go_dot
      ii_log       = gi_log ).

  ENDMETHOD.


  METHOD _files.

    DATA:
      li_progress TYPE REF TO zif_abapgit_progress,
      lv_xstr     TYPE xstring,
      lt_files    TYPE zcl_tar=>ty_files.

    li_progress = zcl_abapgit_progress=>get_instance( 100 ).

    li_progress->show(
      iv_text    = 'Uploading package'
      iv_current = 5 ).

    " Load abapGit ZIP File
    gs_inst-source_name = iv_name.

    CASE iv_enum_zip.
      WHEN c_enum_zip-internet.
        gs_inst-source_type = 'INTERNET'.
        lv_xstr = zcl_abappm_installer_files=>load_internet(
                    iv_url            = |{ iv_name }|
                    iv_user           = |{ iv_user }|
                    iv_password       = |{ iv_password }|
                    iv_proxy_host     = |{ iv_proxy_host }|
                    iv_proxy_port     = |{ iv_proxy_service }|
                    iv_proxy_user     = |{ iv_proxy_user }|
                    iv_proxy_password = |{ iv_proxy_password }| ).
      WHEN c_enum_zip-local.
        gs_inst-source_type = 'LOCAL'.
        lv_xstr = zcl_abappm_installer_files=>load_local( iv_name ).
      WHEN c_enum_zip-server.
        gs_inst-source_type = 'SERVER'.
        lv_xstr = zcl_abappm_installer_files=>load_server( iv_name ).
      WHEN c_enum_zip-data.
        gs_inst-source_type = 'DATA'.
        lv_xstr = iv_data.
      WHEN c_enum_zip-registry.
        gs_inst-source_type = 'REGISTRY'.
        lv_xstr = iv_data.
      WHEN OTHERS.
        zcx_abappm_error=>raise( |Unknown source for { gv_name }| ).
    ENDCASE.

    " Scan for viruses and unzip
    li_progress->show(
      iv_text    = 'Scanning package for viruses'
      iv_current = 10 ).

    zcl_abappm_installer_files=>virus_scan( lv_xstr ).

    li_progress->show(
      iv_text    = 'Unzipping files from package'
      iv_current = 20 ).

    IF iv_enum_zip = c_enum_zip-registry.
      DATA lx_error TYPE REF TO zcx_error.

      TRY.
          DATA ls_remote LIKE LINE OF gt_remote.
          DATA lo_tar TYPE REF TO zcl_tar.
          lo_tar = zcl_tar=>new( )->load( zcl_tar=>new( )->gunzip( lv_xstr ) ).
          lt_files = lo_tar->list( ).

          LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>) WHERE typeflag = '0'.
            CLEAR ls_remote.
            IF <ls_file>-name CA '/'.
              FIND REGEX '(.*[\\/])?([^\\/]+)' IN <ls_file>-name SUBMATCHES ls_remote-path ls_remote-filename.
            ELSE.
              ls_remote-filename = <ls_file>-name.
            ENDIF.
            ls_remote-path = '/' && ls_remote-path.
            ls_remote-path = replace(
              val   = ls_remote-path
              sub   = '/package/'
              with  = '/' ). " packaged with npm
            ls_remote-data = lo_tar->get( <ls_file>-name ).
            TRY.
                ls_remote-sha1 = zcl_abapgit_hash=>sha1_raw( ls_remote-data ).
              CATCH zcx_abapgit_exception ##NO_HANDLER.
            ENDTRY.
            INSERT ls_remote INTO TABLE gt_remote.
          ENDLOOP.
        CATCH zcx_error INTO lx_error.
          zcx_abappm_error=>raise_with_text( lx_error ).
      ENDTRY.

    ELSE.
      gt_remote = zcl_abappm_installer_files=>unzip( lv_xstr ).
    ENDIF.

  ENDMETHOD.


  METHOD _final_message.

    DATA lv_msg TYPE string.

    CASE gs_inst-status.
      WHEN c_success.
        lv_msg = |{ iv_type } of "{ gs_inst-name }" successfully completed|.
        MESSAGE lv_msg TYPE c_success.
      WHEN c_warning.
        lv_msg = |{ iv_type } of "{ gs_inst-name }" finished with warnings|.
        MESSAGE lv_msg TYPE c_success DISPLAY LIKE c_warning.
      WHEN c_error.
        lv_msg = |{ iv_type } of "{ gs_inst-name }" finshed with errors|.
        MESSAGE lv_msg TYPE c_success DISPLAY LIKE c_error.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

  ENDMETHOD.


  METHOD _find_remote_data_config.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF gt_remote.

    CREATE OBJECT ri_config TYPE zcl_abapgit_data_config.

    READ TABLE gt_remote ASSIGNING <ls_remote>
      WITH KEY path = zif_abapgit_data_config=>c_default_path ##PRIMKEY[FILE_PATH].
    IF sy-subrc = 0.
      ri_config->from_json( gt_remote ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote.

    READ TABLE it_remote ASSIGNING <ls_remote> WITH TABLE KEY file_path COMPONENTS
      path     = zif_abapgit_definitions=>c_root_dir
      filename = zif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      TRY.
          ro_dot = zcl_abapgit_dot_abapgit=>deserialize( <ls_remote>-data ).
        CATCH zcx_abapgit_exception.
          zcx_abappm_error=>raise( 'Error decoding .abapgit.xml' ).
      ENDTRY.
    ELSE.
      zcx_abappm_error=>raise( |Error finding .abapgit.xml - Is this an { gv_name }?| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_dot_apack.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote.

    READ TABLE it_remote ASSIGNING <ls_remote> WITH TABLE KEY file_path COMPONENTS
      path     = zif_abapgit_definitions=>c_root_dir
      filename = '.apack-manifest.xml'.
    IF sy-subrc = 0.
      zcx_abappm_error=>raise( |Please migrate APACK to { gv_name } setting| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_namespaces.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF gt_remote.

    LOOP AT gt_remote ASSIGNING <ls_remote> WHERE filename CP '*.nspc.xml'.
      INSERT <ls_remote> INTO TABLE rt_remote.
    ENDLOOP.

  ENDMETHOD.


  METHOD _folder_logic.

    CASE iv_enum_folder_logic.
      WHEN c_enum_folder_logic-default.
        gs_inst-folder_logic = go_dot->get_folder_logic( ).
      WHEN c_enum_folder_logic-prefix.
        gs_inst-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
      WHEN c_enum_folder_logic-mixed.
        gs_inst-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-mixed.
      WHEN c_enum_folder_logic-full.
        gs_inst-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full.
      WHEN OTHERS.
        zcx_abappm_error=>raise( 'Unknown folder logic' ).
    ENDCASE.

  ENDMETHOD.


  METHOD _load.

***    rs_inst = go_db->select(
***      iv_name = iv_name
***      iv_pack = iv_pack ).

  ENDMETHOD.


  METHOD _log_end.
    gs_inst-status = gi_log->get_status( ).
    IF gs_inst-status <> c_success.
      zcl_abapinst_log_viewer=>show_log( gi_log ).
    ENDIF.
  ENDMETHOD.


  METHOD _log_start.
    CREATE OBJECT gi_log TYPE zcl_abapgit_log.
    gi_log->set_title( |{ sy-title } Log| ).
  ENDMETHOD.


  METHOD _namespaces.

    DATA:
      lx_error  TYPE REF TO zcx_abapgit_exception,
      lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.

    " Namespaces must be created upfront,
    " otherwise folder_logic->path_to_package will fail
    lt_remote = _find_remote_namespaces( ).

    IF lines( lt_remote ) > 0.
      TRY.
          zcl_abapinst_objects=>deserialize(
            iv_package   = gs_inst-pack
            iv_language  = gs_inst-installed_langu
            iv_transport = gs_inst-transport
            it_remote    = lt_remote
            io_dot       = go_dot
            ii_log       = gi_log ).

          COMMIT WORK.
        CATCH zcx_abapgit_exception INTO lx_error.
          zcx_abappm_error=>raise( lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD _nothing_found.

    DATA lv_msg TYPE string.

    IF it_list IS INITIAL.
      lv_msg = |No { gv_names } found|.
      MESSAGE lv_msg TYPE c_success.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _packaging.

    DATA:
      lv_name  TYPE string,
      lo_popup TYPE REF TO zcl_abappm_installer_popups.

    go_dot = _find_remote_dot_abapgit( gt_remote ).

    " Check language
    gs_inst-installed_langu = go_dot->get_main_language( ).

    IF gs_inst-installed_langu <> sy-langu.
      zcx_abappm_error=>raise(
        |Unable to install. Logon in main language of package which is { gs_inst-installed_langu }| ).
    ENDIF.

    " Check requirements
    gt_requirements = go_dot->get_requirements( ).

    IF gs_apm IS NOT INITIAL.
      MOVE-CORRESPONDING gs_apm TO gs_inst.
      RETURN. ">>>>>
    ENDIF.

    TRY.
        " FIXME: gs_packaging = go_dot->get_packaging( ).
      CATCH zcx_abapgit_exception.
        CLEAR gs_packaging.
    ENDTRY.

    IF gs_packaging IS INITIAL.
      " Check if APACK file exists and ask migrate it abapGit settings
      _find_remote_dot_apack( gt_remote ).

      " Heuristic to get a name proposal
      TRY.
          lv_name = zcl_abapgit_url=>name( gs_inst-source_name ).
          IF lv_name CS 'URL error'.
            lv_name = gs_inst-source_name.
          ENDIF.
        CATCH zcx_abapgit_exception.
          lv_name = gs_inst-source_name.
      ENDTRY.
      REPLACE '.zip' IN lv_name WITH '' IGNORING CASE.
      REPLACE '.git' IN lv_name WITH '' IGNORING CASE.

      CREATE OBJECT lo_popup.
      gs_packaging = lo_popup->popup_to_enter_packaging(
                       iv_name    = lv_name
                       iv_version = '1.0.0' ).

      IF gs_packaging IS INITIAL.
        zcx_abappm_error=>raise( 'Unable to install without name and version details' ).
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING gs_packaging TO gs_inst.

  ENDMETHOD.


  METHOD _restore_messages.

    DELETE FROM clmcus WHERE username = sy-uname ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    INSERT clmcus FROM TABLE gt_clmcus ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint

  ENDMETHOD.


  METHOD _sap_package.

    CASE iv_enum_package.
      WHEN c_enum_package-default.
        gs_inst-pack = gs_packaging-target_package.
      WHEN c_enum_package-local.
        IF iv_package(1) <> '$'.
          zcx_abappm_error=>raise( 'Local package must begin with $' ).
        ENDIF.
        gs_inst-pack = iv_package.
      WHEN c_enum_package-transportable.
        IF iv_package(1) = '$'.
          zcx_abappm_error=>raise( 'Transportable package must not begin with $' ).
        ENDIF.
        gs_inst-pack = iv_package.
      WHEN OTHERS.
        zcx_abappm_error=>raise( 'Unknown type of target package' ).
    ENDCASE.

  ENDMETHOD.


  METHOD _save.

    DATA:
      ls_inst      TYPE zif_abappm_installer_def=>ty_inst,
      lv_timestamp TYPE timestamp.

    GET TIME STAMP FIELD lv_timestamp.

    ls_inst = _load(
      iv_name = gs_inst-name
      iv_pack = gs_inst-pack ).

    IF ls_inst IS INITIAL.
      ls_inst-name            = gs_inst-name.
      ls_inst-pack            = gs_inst-pack.
      ls_inst-version         = gs_inst-version.
      ls_inst-sem_version     = gs_inst-sem_version.
      ls_inst-description     = gs_inst-description.
      ls_inst-source_type     = gs_inst-source_type.
      ls_inst-source_name     = gs_inst-source_name.
      ls_inst-transport       = gs_inst-transport.
      ls_inst-folder_logic    = gs_inst-folder_logic.
      ls_inst-installed_langu = gs_inst-installed_langu.
      ls_inst-installed_by    = sy-uname.
      ls_inst-installed_at    = lv_timestamp.
      ls_inst-status          = gs_inst-status.

***      go_db->insert( ls_inst ).
    ELSE.
      ls_inst-version         = gs_inst-version.
      ls_inst-sem_version     = gs_inst-sem_version.
      ls_inst-description     = gs_inst-description.
      ls_inst-source_type     = gs_inst-source_type.
      ls_inst-source_name     = gs_inst-source_name.
      ls_inst-transport       = gs_inst-transport.
      ls_inst-folder_logic    = gs_inst-folder_logic.
      ls_inst-installed_langu = gs_inst-installed_langu.
      ls_inst-updated_by      = sy-uname.
      ls_inst-updated_at      = lv_timestamp.
      ls_inst-status          = gs_inst-status.

***      go_db->update( ls_inst ).
    ENDIF.

  ENDMETHOD.


  METHOD _system_check.

    DATA:
      lv_systemedit         TYPE tadir-edtflag,
      lv_sys_cliinddep_edit TYPE t000-ccnocliind,
      lv_msg                TYPE string.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemedit         = lv_systemedit
        sys_cliinddep_edit = lv_sys_cliinddep_edit
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

    IF lv_systemedit = 'N'.
      MESSAGE e102(tk) INTO lv_msg.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.
    IF lv_sys_cliinddep_edit CA '23'.
      MESSAGE e729(tk) INTO lv_msg.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD _transport.

    CHECK gs_inst-pack(1) <> '$'.

    CASE iv_enum_transport.
      WHEN c_enum_transport-existing.
        gs_inst-transport = iv_transport.
      WHEN c_enum_transport-prompt.
        gs_inst-transport = zcl_abapinst_screen=>f4_transport(
          iv_package   = gs_inst-pack
          iv_transport = _transport_get( ) ).

        IF gs_inst-transport IS INITIAL.
          zcx_abappm_error=>raise( 'No transport selected. Installation cancelled' ).
        ENDIF.
    ENDCASE.

    _transport_check( ).

  ENDMETHOD.


  METHOD _transport_check.

    DATA:
      lv_text            TYPE as4text,
      ls_request_header  TYPE trwbo_request_header,
      lt_request_headers TYPE trwbo_request_headers.

    CHECK gs_inst-pack(1) <> '$'.

    lv_text = gs_inst-name && ':' && gs_inst-description.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = gs_inst-transport
      IMPORTING
        et_request_headers = lt_request_headers
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

    " Request Type: Workbench
    READ TABLE lt_request_headers INTO ls_request_header
      WITH KEY trfunction = 'K' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Transport { gs_inst-transport } is not a changeable "workbench request"| ).
    ENDIF.

    " Task Type: Unclassified (ok)
    READ TABLE lt_request_headers TRANSPORTING NO FIELDS
      WITH KEY trfunction = 'X' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " Task Type: Development
    READ TABLE lt_request_headers TRANSPORTING NO FIELDS
      WITH KEY trfunction = 'S' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc <> 0.
      CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
        EXPORTING
          wi_kurztext   = lv_text
          wi_trfunction = 'S'
          wi_strkorr    = ls_request_header-trkorr
        EXCEPTIONS
          OTHERS        = 1.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise_t100( ).
      ENDIF.
    ENDIF.

    " Task Type: Repair (for namespaced projects)
    IF gs_inst-pack(1) = '/'.
      READ TABLE lt_request_headers TRANSPORTING NO FIELDS
        WITH KEY trfunction = 'R' trstatus = 'D' korrdev = 'SYST'.
      IF sy-subrc <> 0.
        CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
          EXPORTING
            wi_kurztext   = lv_text
            wi_trfunction = 'R'
            wi_strkorr    = ls_request_header-trkorr
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc <> 0.
          zcx_abappm_error=>raise_t100( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _transport_get.

    DATA ls_request TYPE trwbo_request.

    " Get previously used transport
***    rv_trkorr = go_db->select( iv_name = gs_inst-name
***                               iv_pack = gs_inst-pack )-transport.

    IF rv_trkorr IS INITIAL.
      " Or last used transport
***      rv_trkorr = go_db->last( )-transport.
    ENDIF.

    IF rv_trkorr IS NOT INITIAL.
      " Check if transport is still open
      CALL FUNCTION 'TR_READ_REQUEST'
        EXPORTING
          iv_read_attributes = 'X'
          iv_trkorr          = rv_trkorr
        CHANGING
          cs_request         = ls_request
        EXCEPTIONS
          error_occured      = 1
          no_authorization   = 2
          OTHERS             = 3.
      IF sy-subrc = 0 AND ls_request-h-trstatus = 'D'.
        RETURN.
      ENDIF.
    ENDIF.

    " Get default transport
    TRY.
        rv_trkorr = zcl_abapgit_factory=>get_default_transport( )->get( )-ordernum.
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD _transport_reset.

    TRY.
        zcl_abapgit_factory=>get_default_transport( )->reset( ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD _uninstall_sotr.

    " Necessary since older releases do not delete SOTR when package is deleted

    DATA:
      lv_use_korr  TYPE abap_bool,
      lt_sotr_head TYPE STANDARD TABLE OF sotr_head WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_tadir>     LIKE LINE OF it_tadir,
      <ls_sotr_head> LIKE LINE OF lt_sotr_head.

    lv_use_korr = boolc( gs_inst-transport IS NOT INITIAL ).

    LOOP AT it_tadir ASSIGNING <ls_tadir> WHERE object = 'DEVC'.

      SELECT * FROM sotr_head INTO TABLE lt_sotr_head
        WHERE paket = <ls_tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head>.
          DELETE FROM sotr_use WHERE concept = <ls_sotr_head>-concept.

          CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
            EXPORTING
              concept                  = <ls_sotr_head>-concept
              corr_num                 = gs_inst-transport
              use_korrnum_immediatedly = lv_use_korr
              flag_string              = abap_false
            EXCEPTIONS
              text_not_found           = 1
              invalid_package          = 2
              text_not_changeable      = 3
              text_enqueued            = 4
              no_correction            = 5
              parameter_error          = 6
              OTHERS                   = 7.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDLOOP.

      ENDIF.

      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_test_modus         = abap_false
          wi_delete_tadir_entry = abap_true
          wi_tadir_pgmid        = 'R3TR'
          wi_tadir_object       = 'SOTR'
          wi_tadir_obj_name     = <ls_tadir>-obj_name
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _uninstall_sots.

    " Necessary since older releases do not delete SOTS when package is deleted

    DATA:
      lv_use_korr  TYPE abap_bool,
      lt_sotr_head TYPE STANDARD TABLE OF sotr_headu WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_tadir>     LIKE LINE OF it_tadir,
      <ls_sotr_head> LIKE LINE OF lt_sotr_head.

    lv_use_korr = boolc( gs_inst-transport IS NOT INITIAL ).

    LOOP AT it_tadir ASSIGNING <ls_tadir> WHERE object = 'DEVC'.

      SELECT * FROM sotr_headu INTO TABLE lt_sotr_head
        WHERE paket = <ls_tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head>.
          DELETE FROM sotr_useu WHERE concept = <ls_sotr_head>-concept.

          CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
            EXPORTING
              concept                  = <ls_sotr_head>-concept
              corr_num                 = gs_inst-transport
              use_korrnum_immediatedly = lv_use_korr
              flag_string              = abap_true
            EXCEPTIONS
              text_not_found           = 1
              invalid_package          = 2
              text_not_changeable      = 3
              text_enqueued            = 4
              no_correction            = 5
              parameter_error          = 6
              OTHERS                   = 7.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDLOOP.

      ENDIF.

      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_test_modus         = abap_false
          wi_delete_tadir_entry = abap_true
          wi_tadir_pgmid        = 'R3TR'
          wi_tadir_object       = 'SOTS'
          wi_tadir_obj_name     = <ls_tadir>-obj_name
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
