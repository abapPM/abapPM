CLASS zcl_abappm_installer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  " TODO: This installer is a copy from Marc Bernard Tools
  " Several of the features are not relevant for apm and can be removed
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
        !tabname TYPE tabname OPTIONAL
        !lock    TYPE viewname OPTIONAL
        !name    TYPE string OPTIONAL
        !names   TYPE string OPTIONAL.

    CLASS-METHODS install
      IMPORTING
        !apm_name          TYPE string OPTIONAL
        !apm_version       TYPE string OPTIONAL
        !enum_zip          TYPE i OPTIONAL
        !name              TYPE char255 OPTIONAL
        !data              TYPE xstring OPTIONAL
        !enum_package      TYPE i OPTIONAL
        !package           TYPE devclass OPTIONAL
        !dlvunit           TYPE dlvunit OPTIONAL
        !devlayer          TYPE devlayer OPTIONAL
        !enum_transport    TYPE i OPTIONAL
        !transport         TYPE trkorr OPTIONAL
*        !user              TYPE char255 OPTIONAL
*        !password          TYPE char255 OPTIONAL
*        !proxy_host        TYPE char255 OPTIONAL
*        !proxy_service     TYPE char5 OPTIONAL
*        !proxy_user        TYPE char255 OPTIONAL
*        !proxy_password    TYPE char255 OPTIONAL
        !enum_folder_logic TYPE i OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall
      IMPORTING
        !apm  TYPE abap_bool DEFAULT abap_false
        !name TYPE zif_abappm_installer_def=>ty_name OPTIONAL
        !pack TYPE zif_abappm_installer_def=>ty_pack OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS list
      RAISING
        zcx_abappm_error.

    CLASS-METHODS f4
      RETURNING
        VALUE(result) TYPE zif_abappm_installer_def=>ty_inst
      RAISING
        zcx_abappm_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      BEGIN OF gs_apm,
        name    TYPE string,
        version TYPE string,
      END OF gs_apm,
      db_persist    TYPE REF TO zcl_abapinst_persistence,
      remote_files  TYPE zif_abapgit_git_definitions=>ty_files_tt,
      install_data  TYPE zif_abappm_installer_def=>ty_inst,
      dot_abapgit   TYPE REF TO zcl_abapgit_dot_abapgit,
      log           TYPE REF TO zif_abapgit_log,
      gv_name       TYPE string,
      gv_names      TYPE string,
      clmcus_backup TYPE STANDARD TABLE OF clmcus WITH DEFAULT KEY.

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
        !list         TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS _files
      IMPORTING
        !enum_zip TYPE i
        !name     TYPE char255 OPTIONAL
        !data     TYPE xstring OPTIONAL
*        !user           TYPE char255 OPTIONAL
*        !password       TYPE char255 OPTIONAL
*        !proxy_host     TYPE char255 OPTIONAL
*        !proxy_service  TYPE char5 OPTIONAL
*        !proxy_user     TYPE char255 OPTIONAL
*        !proxy_password TYPE char255 OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _packaging
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _sap_package
      IMPORTING
        !enum_package TYPE i
        !package      TYPE devclass OPTIONAL
        !dlvunit      TYPE dlvunit OPTIONAL
        !devlayer     TYPE devlayer OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _check
      IMPORTING
        !force TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _check_version
      IMPORTING
        !new_version       TYPE zif_abappm_installer_def=>ty_version
        !installed_version TYPE zif_abappm_installer_def=>ty_version
        !force             TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _folder_logic
      IMPORTING
        !enum_folder_logic TYPE i
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport
      IMPORTING
        !enum_transport TYPE i
        !transport      TYPE trkorr OPTIONAL
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport_get
      RETURNING
        VALUE(result) TYPE trkorr
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
        !name         TYPE zif_abappm_installer_def=>ty_name OPTIONAL
        !pack         TYPE zif_abappm_installer_def=>ty_pack OPTIONAL
      RETURNING
        VALUE(result) TYPE zif_abappm_installer_def=>ty_inst
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
        !type TYPE string.

    CLASS-METHODS _find_remote_dot_abapgit
      IMPORTING
        !it_remote    TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _find_remote_dot_apack
      IMPORTING
        !it_remote    TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _find_remote_namespaces
      RETURNING
        VALUE(result) TYPE zif_abapgit_git_definitions=>ty_files_tt.

    CLASS-METHODS _find_remote_data_config
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_data_config
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
      list     TYPE zif_abappm_installer_def=>ty_list,
      selected LIKE list,
      popup    TYPE REF TO zcl_abappm_installer_popups,
      columns  TYPE zcl_abappm_installer_popups=>ty_alv_column_tt,
      question TYPE string,
      answer   TYPE sy-input.

    FIELD-SYMBOLS:
      <column> LIKE LINE OF columns.

    init( ).

***    list = db_persist->list( ).

    CHECK _nothing_found( list ) IS INITIAL.

    APPEND INITIAL LINE TO columns ASSIGNING <column>.
    <column>-name   = 'NAME'.
    <column>-text   = 'Name'.
    <column>-length = 30.
    <column>-key    = abap_true.
    APPEND INITIAL LINE TO columns ASSIGNING <column>.
    <column>-name   = 'PACK'.
    <column>-text   = 'Package'.
    <column>-length = 30.
    <column>-key    = abap_true.
    APPEND INITIAL LINE TO columns ASSIGNING <column>.
    <column>-name   = 'VERSION'.
    <column>-text   = 'Version'.
    <column>-length = 15.
    APPEND INITIAL LINE TO columns ASSIGNING <column>.
    <column>-name   = 'DESCRIPTION'.
    <column>-text   = 'Description'.
    <column>-length = 60.

    CREATE OBJECT popup.

    TRY.
        popup->popup_to_select_from_list(
          EXPORTING
            import_list        = list
            title              = sy-title
            header_text        = |Select the { gv_name } that you want to uninstall:|
            end_column         = 150
            striped_pattern    = abap_true
            optimize_col_width = abap_false
            selection_mode     = if_salv_c_selection_mode=>single
            columns_to_display = columns
          IMPORTING
            export_list        = selected ).
      CATCH zcx_abappm_error.
        RETURN.
    ENDTRY.

    IF selected IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE selected INTO result INDEX 1.
    ASSERT sy-subrc = 0.

    TRY.
        question = |Are you sure, you want to uninstall "{ result-description } ({ result-name })"?|.

        answer = popup->popup_to_confirm(
          title          = sy-title
          question       = question
          default_button = '2' ).

        IF answer <> '1'.
          CLEAR result.
        ENDIF.
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD init.

*    IF db_persist IS NOT BOUND.
*      IF tabname IS INITIAL AND lock IS INITIAL.
*        db_persist = zcl_abapinst_persistence=>get_instance( ).
*      ELSE.
*        db_persist = zcl_abapinst_persistence=>get_instance(
*          iv_tabname = tabname
*          iv_lock    = lock ).
*      ENDIF.
*    ENDIF.

    IF name IS NOT INITIAL OR names IS NOT INITIAL.
      gv_name  = name.
      gv_names = names.
    ENDIF.

  ENDMETHOD.


  METHOD install.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception.

    gs_apm-name    = apm_name.
    gs_apm-version = apm_version.

    init( ).

    TRY.
        _clear( ).

        _log_start( ).

        _system_check( ).

        _files(
          enum_zip       = enum_zip
          name           = name
          data           = data ).
*          user           = user
*          password       = password
*          proxy_host     = proxy_host
*          proxy_service  = proxy_service
*          proxy_user     = proxy_user
*          proxy_password = proxy_password ).

        _packaging( ).

        _sap_package(
          enum_package = enum_package
          package      = package ).

        _check( ).

        _folder_logic( enum_folder_logic ).

        _transport(
          enum_transport = enum_transport
          transport      = transport ).

        _confirm_messages( ).

        _namespaces( ).

        _deserialize_objects( ).

        _deserialize_data( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        _transport_reset( ).

        log->add_exception( lx_error ).
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
      lr_column        TYPE REF TO cl_salv_column_table.

    FIELD-SYMBOLS:
      <list> LIKE LINE OF lt_list.

    init( ).

***    lt_list = db_persist->list( ).

    CHECK _nothing_found( lt_list ) IS INITIAL.

    LOOP AT lt_list ASSIGNING <list>.
      CASE <list>-status.
        WHEN space.
          <list>-status = icon_led_inactive.
        WHEN c_success.
          <list>-status = icon_led_green.
        WHEN c_warning.
          <list>-status = icon_led_yellow.
        WHEN OTHERS.
          <list>-status = icon_led_red.
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
      CATCH cx_salv_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD uninstall.

    IF apm IS INITIAL.
      init( ).
    ENDIF.

    TRY.
        _clear( ).

        _log_start( ).

        _system_check( ).

        IF apm IS INITIAL.
          install_data = _load(
            name = name
            pack = pack ).

          IF install_data IS INITIAL.
            zcx_abappm_error=>raise( |Package { name } ({ pack }) not found| ).
          ENDIF.

          " TODO: needs to work for apm
          _transport( c_enum_transport-prompt ).
        ELSE.
          install_data-pack = pack.
        ENDIF.

        _confirm_messages( ).

        " A few tries to tackle dependencies
        DO 3 TIMES.
          DATA(lt_tadir) = zcl_abapgit_factory=>get_tadir( )->read( install_data-pack ).

          DELETE lt_tadir WHERE object = 'NSPC'.

          IF lt_tadir IS NOT INITIAL.
            _uninstall_sotr( lt_tadir ).

            _uninstall_sots( lt_tadir ).

            zcl_abapinst_objects=>delete(
              it_tadir     = lt_tadir
              iv_transport = install_data-transport
              ii_log       = log ).
          ENDIF.
        ENDDO.

      CATCH zcx_abapgit_exception INTO DATA(error).
        _transport_reset( ).

        log->add_exception( error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _check_uninstalled( lt_tadir ).

        IF apm IS INITIAL.
          IF install_data-status = c_success.
            _delete( ).
          ELSE.
            _save( ).
          ENDIF.
        ENDIF.

        _restore_messages( ).

        _final_message( 'Uninstall' ).

      CATCH zcx_abapgit_exception INTO error ##NEEDED.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD _check.

    DATA install_local TYPE zif_abappm_installer_def=>ty_inst.

    IF gs_apm-name IS INITIAL.
      install_local = _load(
        name = install_data-name
        pack = install_data-pack ).

      IF install_local IS INITIAL.

        install_local = _load( pack = install_data-pack ).

        IF install_local IS NOT INITIAL.
          zcx_abappm_error=>raise( |SAP package { install_data-pack } already contains a different { gv_name }| ).
        ENDIF.

      ELSE.

        _check_version(
          new_version       = install_data-sem_version
          installed_version = install_local-sem_version
          force             = force ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _check_uninstalled.

    DATA:
      msg      TYPE string,
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
        install_data-status = c_warning.
        msg = |Some objects could not be uninstalled. Uninstall the remaining objects |
              && |of pacakge { install_data-pack } manually|.
      ELSE.
        msg = |Release the transport and deleted the remaining pacakge { install_data-pack } manually|.
      ENDIF.
      MESSAGE msg TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD _check_version.

    DATA:
      comp     TYPE i,
      msg      TYPE string,
      question TYPE string,
      lo_popup TYPE REF TO zcl_abappm_installer_popups,
      answer   TYPE c LENGTH 1.

    comp = zcl_abapgit_version=>compare(
      is_a = new_version
      is_b = installed_version ).
    IF comp <= 0.

      msg = |{ install_data-name } is already installed (with same or newer version)|.
      question = msg  && '. Do you want to overwrite it?'.

      IF force IS INITIAL.
        CREATE OBJECT lo_popup.

        answer = lo_popup->popup_to_confirm(
          title          = sy-title
          question       = question
          default_button = '2' ).

        IF answer <> '1'.
          zcx_abappm_error=>raise( msg ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD _clear.
    CLEAR: install_data, dot_abapgit.
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
    SELECT * FROM clmcus INTO TABLE clmcus_backup WHERE username = sy-uname ##SUBRC_OK.
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

***    db_persist->delete(
***      name = install_data-name
***      pack = install_data-pack ).

  ENDMETHOD.


  METHOD _deserialize_data.

    DATA:
      support   TYPE REF TO lcl_abapgit_data_supporter,
      inject    TYPE REF TO zcl_abapgit_data_injector,
      config    TYPE REF TO zif_abapgit_data_config,
      deser     TYPE REF TO zif_abapgit_data_deserializer,
      checks    TYPE zif_abapgit_definitions=>ty_deserialize_checks,
      overwrite TYPE LINE OF zif_abapgit_definitions=>ty_deserialize_checks-overwrite,
      result    TYPE LINE OF zif_abapgit_data_deserializer=>ty_results,
      results   TYPE zif_abapgit_data_deserializer=>ty_results.

    CREATE OBJECT support.
    " FIXME:
    "    CREATE OBJECT inject
    "    inject->set_supporter( lo_support )

    config = _find_remote_data_config( ).

    deser = zcl_abapgit_data_factory=>get_deserializer( ).

    results = deser->deserialize(
      ii_config = config
      it_files  = remote_files ).

    LOOP AT results INTO result.
      CLEAR overwrite.
      overwrite-obj_type = result-type.
      overwrite-obj_name = result-name.
      overwrite-decision = zif_abapgit_definitions=>c_yes.
      COLLECT overwrite INTO checks-overwrite.
    ENDLOOP.

    deser->actualize(
      is_checks = checks
      it_result = results ).

  ENDMETHOD.


  METHOD _deserialize_objects.

    zcl_abapinst_objects=>deserialize(
      iv_package   = install_data-pack
      iv_language  = install_data-installed_langu
      iv_transport = install_data-transport
      it_remote    = remote_files
      io_dot       = dot_abapgit
      ii_log       = log ).

  ENDMETHOD.


  METHOD _files.

    DATA:
      progress  TYPE REF TO zif_abapgit_progress,
      xstr      TYPE xstring,
      files     TYPE zcl_tar=>ty_files,
      ls_remote LIKE LINE OF remote_files,
      lo_tar    TYPE REF TO zcl_abappm_tar.

    progress = zcl_abapgit_progress=>get_instance( 100 ).

    progress->show(
      iv_text    = 'Uploading package'
      iv_current = 5 ).

    " Load abapGit ZIP File
    install_data-source_name = name.

    CASE enum_zip.
      WHEN c_enum_zip-internet.
        install_data-source_type = 'INTERNET'.
        xstr = zcl_abappm_installer_files=>load_internet(
          url            = |{ name }| ).
*          user           = |{ user }|
*          password       = |{ password }|
*          proxy_host     = |{ proxy_host }|
*          proxy_port     = |{ proxy_service }|
*          proxy_user     = |{ proxy_user }|
*          proxy_password = |{ proxy_password }| ).
      WHEN c_enum_zip-local.
        install_data-source_type = 'LOCAL'.
        xstr = zcl_abappm_installer_files=>load_local( name ).
      WHEN c_enum_zip-server.
        install_data-source_type = 'SERVER'.
        xstr = zcl_abappm_installer_files=>load_server( name ).
      WHEN c_enum_zip-data.
        install_data-source_type = 'DATA'.
        xstr = data.
      WHEN c_enum_zip-registry.
        install_data-source_type = 'REGISTRY'.
        xstr = data.
      WHEN OTHERS.
        zcx_abappm_error=>raise( |Unknown source for { gv_name }| ).
    ENDCASE.

    " Scan for viruses and unzip
    progress->show(
      iv_text    = 'Scanning package for viruses'
      iv_current = 10 ).

    zcl_abappm_installer_files=>virus_scan( xstr ).

    progress->show(
      iv_text    = 'Unzipping files from package'
      iv_current = 20 ).

    IF enum_zip = c_enum_zip-registry.

      lo_tar = zcl_abappm_tar=>new( )->load( zcl_abappm_tar=>new( )->gunzip( xstr ) ).
      files = lo_tar->list( ).

      LOOP AT files ASSIGNING FIELD-SYMBOL(<file>) WHERE typeflag = '0'.
        CLEAR ls_remote.
        IF <file>-name CA '/'.
          FIND REGEX '(.*[\\/])?([^\\/]+)' IN <file>-name SUBMATCHES ls_remote-path ls_remote-filename.
        ELSE.
          ls_remote-filename = <file>-name.
        ENDIF.
        ls_remote-path = '/' && ls_remote-path.
        ls_remote-path = replace(
          val   = ls_remote-path
          sub   = '/package/'
          with  = '/' ). " packaged with npm
        ls_remote-data = lo_tar->get( <file>-name ).
        TRY.
            ls_remote-sha1 = zcl_abapgit_hash=>sha1_raw( ls_remote-data ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
        INSERT ls_remote INTO TABLE remote_files.
      ENDLOOP.

    ELSE.
      remote_files = zcl_abappm_installer_files=>unzip( xstr ).
    ENDIF.

  ENDMETHOD.


  METHOD _final_message.

    DATA msg TYPE string.

    CASE install_data-status.
      WHEN c_success.
        msg = |{ type } of "{ install_data-name }" successfully completed|.
        MESSAGE msg TYPE c_success.
      WHEN c_warning.
        msg = |{ type } of "{ install_data-name }" finished with warnings|.
        MESSAGE msg TYPE c_success DISPLAY LIKE c_warning.
      WHEN c_error.
        msg = |{ type } of "{ install_data-name }" finshed with errors|.
        MESSAGE msg TYPE c_success DISPLAY LIKE c_error.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

  ENDMETHOD.


  METHOD _find_remote_data_config.

    CREATE OBJECT result TYPE zcl_abapgit_data_config.

    READ TABLE remote_files ASSIGNING FIELD-SYMBOL(<remote>)
      WITH KEY path = zif_abapgit_data_config=>c_default_path ##PRIMKEY[FILE_PATH].
    IF sy-subrc = 0.
      result->from_json( remote_files ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_dot_abapgit.

    FIELD-SYMBOLS: <remote> LIKE LINE OF it_remote.

    READ TABLE it_remote ASSIGNING <remote> WITH TABLE KEY file_path COMPONENTS
      path     = zif_abapgit_definitions=>c_root_dir
      filename = zif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      TRY.
          result = zcl_abapgit_dot_abapgit=>deserialize( <remote>-data ).
        CATCH zcx_abapgit_exception.
          zcx_abappm_error=>raise( 'Error decoding .abapgit.xml' ).
      ENDTRY.
    ELSE.
      zcx_abappm_error=>raise( |Error finding .abapgit.xml - Is this an { gv_name }?| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_dot_apack.

    FIELD-SYMBOLS: <remote> LIKE LINE OF it_remote.

    READ TABLE it_remote ASSIGNING <remote> WITH TABLE KEY file_path COMPONENTS
      path     = zif_abapgit_definitions=>c_root_dir
      filename = '.apack-manifest.xml'.
    IF sy-subrc = 0.
      zcx_abappm_error=>raise( |Please migrate APACK to { gv_name } setting| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_namespaces.

    FIELD-SYMBOLS: <remote> LIKE LINE OF remote_files.

    LOOP AT remote_files ASSIGNING <remote> WHERE filename CP '*.nspc.xml'.
      INSERT <remote> INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD _folder_logic.

    CASE enum_folder_logic.
      WHEN c_enum_folder_logic-default.
        install_data-folder_logic = dot_abapgit->get_folder_logic( ).
      WHEN c_enum_folder_logic-prefix.
        install_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
      WHEN c_enum_folder_logic-mixed.
        install_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-mixed.
      WHEN c_enum_folder_logic-full.
        install_data-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full.
      WHEN OTHERS.
        zcx_abappm_error=>raise( 'Unknown folder logic' ).
    ENDCASE.

  ENDMETHOD.


  METHOD _load.

***    result = db_persist->select(
***      name = name
***      pack = pack ).

  ENDMETHOD.


  METHOD _log_end.
    install_data-status = log->get_status( ).
    IF install_data-status <> c_success.
      zcl_abapinst_log_viewer=>show_log( log ).
    ENDIF.
  ENDMETHOD.


  METHOD _log_start.
    CREATE OBJECT log TYPE zcl_abapgit_log.
    log->set_title( |{ sy-title } Log| ).
  ENDMETHOD.


  METHOD _namespaces.

    " Namespaces must be created upfront,
    " otherwise folder_logic->path_to_package will fail
    DATA(remote_files) = _find_remote_namespaces( ).

    IF lines( remote_files ) > 0.
      TRY.
          zcl_abapinst_objects=>deserialize(
            iv_package   = install_data-pack
            iv_language  = install_data-installed_langu
            iv_transport = install_data-transport
            it_remote    = remote_files
            io_dot       = dot_abapgit
            ii_log       = log ).

          COMMIT WORK.
        CATCH zcx_abapgit_exception INTO DATA(error).
          zcx_abappm_error=>raise_with_text( error ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD _nothing_found.

    DATA msg TYPE string.

    IF list IS INITIAL.
      msg = |No { gv_names } found|.
      MESSAGE msg TYPE c_success.
      result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _packaging.

    DATA:
      name     TYPE string,
      lo_popup TYPE REF TO zcl_abappm_installer_popups.

    dot_abapgit = _find_remote_dot_abapgit( remote_files ).

    " Check language
    install_data-installed_langu = dot_abapgit->get_main_language( ).

    IF install_data-installed_langu <> sy-langu.
      zcx_abappm_error=>raise(
        |Unable to install. Logon in main language of package which is { install_data-installed_langu }| ).
    ENDIF.

    MOVE-CORRESPONDING gs_apm TO install_data.

  ENDMETHOD.


  METHOD _restore_messages.

    DELETE FROM clmcus WHERE username = sy-uname ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint
    INSERT clmcus FROM TABLE clmcus_backup ##SUBRC_OK.
    CHECK sy-subrc >= 0. "abaplint

  ENDMETHOD.


  METHOD _sap_package.

    CASE enum_package.
      WHEN c_enum_package-local.
        IF package(1) <> '$'.
          zcx_abappm_error=>raise( 'Local package must begin with $' ).
        ENDIF.
      WHEN c_enum_package-transportable.
        IF package(1) = '$'.
          zcx_abappm_error=>raise( 'Transportable package must not begin with $' ).
        ENDIF.
      WHEN OTHERS.
        zcx_abappm_error=>raise( 'Unknown type of target package' ).
    ENDCASE.

    install_data-pack = package.

  ENDMETHOD.


  METHOD _save.

    GET TIME STAMP FIELD DATA(timestamp).

    DATA(install_local) = _load(
      name = install_data-name
      pack = install_data-pack ).

    IF install_local IS INITIAL.
      install_local-name            = install_data-name.
      install_local-pack            = install_data-pack.
      install_local-version         = install_data-version.
      install_local-sem_version     = install_data-sem_version.
      install_local-description     = install_data-description.
      install_local-source_type     = install_data-source_type.
      install_local-source_name     = install_data-source_name.
      install_local-transport       = install_data-transport.
      install_local-folder_logic    = install_data-folder_logic.
      install_local-installed_langu = install_data-installed_langu.
      install_local-installed_by    = sy-uname.
      install_local-installed_at    = timestamp.
      install_local-status          = install_data-status.

***      db_persist->insert( install_local ).
    ELSE.
      install_local-version         = install_data-version.
      install_local-sem_version     = install_data-sem_version.
      install_local-description     = install_data-description.
      install_local-source_type     = install_data-source_type.
      install_local-source_name     = install_data-source_name.
      install_local-transport       = install_data-transport.
      install_local-folder_logic    = install_data-folder_logic.
      install_local-installed_langu = install_data-installed_langu.
      install_local-updated_by      = sy-uname.
      install_local-updated_at      = timestamp.
      install_local-status          = install_data-status.

***      db_persist->update( install_local ).
    ENDIF.

  ENDMETHOD.


  METHOD _system_check.

    DATA:
      systemedit         TYPE tadir-edtflag,
      sys_cliinddep_edit TYPE t000-ccnocliind.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemedit         = systemedit
        sys_cliinddep_edit = sys_cliinddep_edit
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

    IF systemedit = 'N'.
      MESSAGE e102(tk) INTO zcx_abappm_error=>null.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.
    IF sys_cliinddep_edit CA '23'.
      MESSAGE e729(tk) INTO zcx_abappm_error=>null.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD _transport.

    CHECK install_data-pack(1) <> '$'.

    CASE enum_transport.
      WHEN c_enum_transport-existing.
        install_data-transport = transport.
      WHEN c_enum_transport-prompt.
        install_data-transport = zcl_abapinst_screen=>f4_transport(
          iv_package   = install_data-pack
          iv_transport = _transport_get( ) ).

        IF install_data-transport IS INITIAL.
          zcx_abappm_error=>raise( 'No transport selected. Installation cancelled' ).
        ENDIF.
    ENDCASE.

    _transport_check( ).

  ENDMETHOD.


  METHOD _transport_check.

    DATA:
      request_header  TYPE trwbo_request_header,
      request_headers TYPE trwbo_request_headers.

    CHECK install_data-pack(1) <> '$'.

    DATA(text) = install_data-name && ':' && install_data-description.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        trkorr             = install_data-transport
      IMPORTING
        et_request_headers = request_headers
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

    " Request Type: Workbench
    READ TABLE request_headers INTO request_header
      WITH KEY trfunction = 'K' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Transport { install_data-transport } is not a changeable "workbench request"| ).
    ENDIF.

    " Task Type: Unclassified (ok)
    READ TABLE request_headers TRANSPORTING NO FIELDS
      WITH KEY trfunction = 'X' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " Task Type: Development
    READ TABLE request_headers TRANSPORTING NO FIELDS
      WITH KEY trfunction = 'S' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc <> 0.
      CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
        EXPORTING
          wi_kurztext   = text
          wi_trfunction = 'S'
          wi_strkorr    = request_header-trkorr
        EXCEPTIONS
          OTHERS        = 1.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise_t100( ).
      ENDIF.
    ENDIF.

    " Task Type: Repair (for namespaced projects)
    IF install_data-pack(1) = '/'.
      READ TABLE request_headers TRANSPORTING NO FIELDS
        WITH KEY trfunction = 'R' trstatus = 'D' korrdev = 'SYST'.
      IF sy-subrc <> 0.
        CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
          EXPORTING
            wi_kurztext   = text
            wi_trfunction = 'R'
            wi_strkorr    = request_header-trkorr
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc <> 0.
          zcx_abappm_error=>raise_t100( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _transport_get.

    DATA request TYPE trwbo_request.

    " Get previously used transport
***    result = db_persist->select( name = install_data-name
***                               pack = install_data-pack )-transport.

    IF result IS INITIAL.
      " Or last used transport
***      result = db_persist->last( )-transport.
    ENDIF.

    IF result IS NOT INITIAL.
      " Check if transport is still open
      CALL FUNCTION 'TR_READ_REQUEST'
        EXPORTING
          read_attributes  = 'X'
          trkorr           = result
        CHANGING
          cs_request       = request
        EXCEPTIONS
          error_occured    = 1
          no_authorization = 2
          OTHERS           = 3.
      IF sy-subrc = 0 AND request-h-trstatus = 'D'.
        RETURN.
      ENDIF.
    ENDIF.

    " Get default transport
    TRY.
        result = zcl_abapgit_factory=>get_default_transport( )->get( )-ordernum.
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

    DATA(use_korr) = xsdbool( install_data-transport IS NOT INITIAL ).

    LOOP AT it_tadir ASSIGNING FIELD-SYMBOL(<tadir>) WHERE object = 'DEVC'.

      SELECT * FROM sotr_head INTO TABLE @DATA(sotr_head)
        WHERE paket = @<tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT sotr_head ASSIGNING FIELD-SYMBOL(<sotr_head>).
          DELETE FROM sotr_use WHERE concept = <sotr_head>-concept.

          CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
            EXPORTING
              concept                  = <sotr_head>-concept
              corr_num                 = install_data-transport
              use_korrnum_immediatedly = use_korr
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
          wi_tadir_obj_name     = <tadir>-obj_name
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _uninstall_sots.

    " Necessary since older releases do not delete SOTS when package is deleted

    DATA(use_korr) = xsdbool( install_data-transport IS NOT INITIAL ).

    LOOP AT it_tadir ASSIGNING FIELD-SYMBOL(<tadir>) WHERE object = 'DEVC'.

      SELECT * FROM sotr_headu INTO TABLE @DATA(sotr_head)
        WHERE paket = @<tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT sotr_head ASSIGNING FIELD-SYMBOL(<sotr_head>).
          DELETE FROM sotr_useu WHERE concept = <sotr_head>-concept.

          CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
            EXPORTING
              concept                  = <sotr_head>-concept
              corr_num                 = install_data-transport
              use_korrnum_immediatedly = use_korr
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
          wi_tadir_obj_name     = <tadir>-obj_name
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
