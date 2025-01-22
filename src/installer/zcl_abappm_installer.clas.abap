CLASS zcl_abappm_installer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Installer
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* TODO: This installer is a copy from Marc Bernard Tools
* Some of the features are not relevant for apm and can be removed
************************************************************************
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

    CLASS-METHODS install
      IMPORTING
        !apm_name          TYPE string OPTIONAL
        !apm_version       TYPE string OPTIONAL
        !enum_zip          TYPE i OPTIONAL
        !name              TYPE string OPTIONAL
        !data              TYPE xstring OPTIONAL
        !enum_package      TYPE i OPTIONAL
        !package           TYPE devclass OPTIONAL
        !dlvunit           TYPE dlvunit OPTIONAL
        !devlayer          TYPE devlayer OPTIONAL
        !enum_transport    TYPE i OPTIONAL
        !transport         TYPE trkorr OPTIONAL
        !enum_folder_logic TYPE i OPTIONAL
        !is_production     TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall
      IMPORTING
        !apm  TYPE abap_bool DEFAULT abap_false
        !name TYPE zif_abappm_installer_def=>ty_name OPTIONAL
        !pack TYPE zif_abappm_installer_def=>ty_pack OPTIONAL
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      BEGIN OF apm_params,
        name    TYPE string,
        version TYPE string,
      END OF apm_params,
      remote_files  TYPE zif_abapgit_git_definitions=>ty_files_tt,
      install_data  TYPE zif_abappm_installer_def=>ty_inst,
      dot_abapgit   TYPE REF TO zcl_abapgit_dot_abapgit,
      log           TYPE REF TO zif_abapgit_log,
      clmcus_backup TYPE STANDARD TABLE OF clmcus WITH DEFAULT KEY.

    CONSTANTS:
      c_success TYPE sy-msgty VALUE 'S' ##NO_TEXT,
      c_warning TYPE sy-msgty VALUE 'W' ##NO_TEXT,
      c_error   TYPE sy-msgty VALUE 'E' ##NO_TEXT.

    CLASS-METHODS _system_check
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _clear.

    CLASS-METHODS _files
      IMPORTING
        !enum_zip TYPE i
        !name     TYPE string OPTIONAL
        !data     TYPE xstring OPTIONAL
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
        zcx_abappm_error.

    CLASS-METHODS _deserialize_data
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
        !remote       TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _find_remote_dot_apack
      IMPORTING
        !remote       TYPE zif_abapgit_git_definitions=>ty_files_tt
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
        zcx_abappm_error.

    CLASS-METHODS _check_uninstalled
      IMPORTING
        !tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CLASS-METHODS _uninstall_sotr
      IMPORTING
        !tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CLASS-METHODS _uninstall_sots
      IMPORTING
        !tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.



CLASS zcl_abappm_installer IMPLEMENTATION.


  METHOD install.

    apm_params-name    = apm_name.
    apm_params-version = apm_version.

    TRY.
        _clear( ).

        _log_start( ).

        _system_check( ).

        _files(
          enum_zip       = enum_zip
          name           = name
          data           = data ).

        _packaging( ).

        _sap_package(
          enum_package = enum_package
          package      = package ).

        _folder_logic( enum_folder_logic ).

        _transport(
          enum_transport = enum_transport
          transport      = transport ).

        _confirm_messages( ).

        _namespaces( ).

        _deserialize_objects( ).

        _deserialize_data( ).

      CATCH cx_root INTO DATA(error).
        _transport_reset( ).

        log->add_exception( error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _restore_messages( ).

        _final_message( 'Installation' ).

      CATCH cx_root INTO error.
        log->add_exception( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD uninstall.

    TRY.
        _clear( ).

        _log_start( ).

        _system_check( ).

        IF apm IS INITIAL.
          " TODO: needs to work for apm
          _transport( c_enum_transport-prompt ).
        ELSE.
          install_data-pack = pack.
        ENDIF.

        _confirm_messages( ).

        " A few tries to tackle dependencies
        DO 3 TIMES.
          DATA(tadir) = zcl_abapgit_factory=>get_tadir( )->read( install_data-pack ).

          DELETE tadir WHERE object = 'NSPC'.

          IF tadir IS NOT INITIAL.
            _uninstall_sotr( tadir ).

            _uninstall_sots( tadir ).

            " Bridge to abapGit
            zcl_abappm_installer_objects=>delete(
              it_tadir     = tadir
              iv_transport = install_data-transport
              ii_log       = log ).
          ENDIF.
        ENDDO.

      CATCH cx_root INTO DATA(error).
        _transport_reset( ).

        log->add_exception( error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _check_uninstalled( tadir ).

        _restore_messages( ).

        _final_message( 'Uninstall' ).

      CATCH cx_root INTO error.
        log->add_exception( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD _check_uninstalled.

    DATA tadir_list LIKE tadir.

    CHECK tadir IS NOT INITIAL.

    SELECT pgmid, object, obj_name FROM tadir INTO TABLE @tadir_list
      FOR ALL ENTRIES IN @tadir
      WHERE pgmid    = @tadir-pgmid
        AND object   = @tadir-object
        AND obj_name = @tadir-obj_name ##TOO_MANY_ITAB_FIELDS.
    IF sy-subrc = 0.
      LOOP AT tadir_list TRANSPORTING NO FIELDS WHERE object <> 'DEVC'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        install_data-status = c_warning.
        DATA(msg) = |Some objects could not be uninstalled. Uninstall the remaining objects |
                 && |of pacakge { install_data-pack } manually|.
      ELSE.
        msg = |Release the transport and deleted the remaining pacakge { install_data-pack } manually|.
      ENDIF.
      MESSAGE msg TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD _check_version.

    DATA(comp) = zcl_abapgit_version=>compare(
      is_a = new_version
      is_b = installed_version ).
    IF comp <= 0.

      DATA(msg) = |{ install_data-name } is already installed (with same or newer version)|.
      DATA(question) = msg  && '. Do you want to overwrite it?'.

      IF force IS INITIAL.
        DATA(popup) = NEW zcl_abappm_installer_popups( ).

        DATA(answer) = popup->popup_to_confirm(
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

    " Set tool flag to avoid messages
    TRY.
        CALL FUNCTION c_toolflag_set.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    " Confirm message about modification mode (DT, CLM_INFORMATION)
    " and backup old state (see _restore_messages)
    SELECT * FROM clmcus INTO TABLE @clmcus_backup WHERE username = @sy-uname ##SUBRC_OK.

    DATA(customizing) = VALUE clmcus(
      username = sy-uname
      obj_type = 'CLAS' ).
    INSERT clmcus FROM @customizing ##SUBRC_OK.

    customizing-obj_type = 'INTF'.
    INSERT clmcus FROM @customizing ##SUBRC_OK.

    customizing-obj_type = 'METH'.
    INSERT clmcus FROM @customizing ##SUBRC_OK.

  ENDMETHOD.


  METHOD _deserialize_data.

    DATA checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    " DATA inject TYPE REF TO zcl_abapgit_data_injector


    " Bridge to abapGit
    TRY.
        DATA(support) = NEW lcl_abapgit_data_supporter( ).
        " FIXME:
        "    CREATE OBJECT inject
        "    inject->set_supporter( lo_support )

        DATA(config) = _find_remote_data_config( ).

        DATA(deser) = zcl_abapgit_data_factory=>get_deserializer( ).

        DATA(results) = deser->deserialize(
          ii_config = config
          it_files  = remote_files ).

        LOOP AT results INTO DATA(result).
          DATA(overwrite) = VALUE zif_abapgit_definitions=>ty_overwrite(
            obj_type = result-type
            obj_name = result-name
            decision = zif_abapgit_definitions=>c_yes ).
          COLLECT overwrite INTO checks-overwrite.
        ENDLOOP.

        deser->actualize(
          is_checks = checks
          it_result = results ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD _deserialize_objects.

    " Bridge to abapGit
    TRY.
        zcl_abappm_installer_objects=>deserialize(
          iv_package   = install_data-pack
          iv_language  = install_data-installed_langu
          iv_transport = install_data-transport
          it_remote    = remote_files
          io_dot       = dot_abapgit
          ii_log       = log ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD _files.

    DATA package_data TYPE xstring.

    DATA(progress) = zcl_abapgit_progress=>get_instance( 100 ).

    progress->show(
      iv_text    = 'Uploading package'
      iv_current = 5 ).

    " Load abapGit ZIP File
    install_data-source_name = name.

    CASE enum_zip.
      WHEN c_enum_zip-internet.
        install_data-source_type = 'INTERNET'.
        package_data = zcl_abappm_installer_files=>load_internet( name ).
      WHEN c_enum_zip-local.
        install_data-source_type = 'LOCAL'.
        package_data = zcl_abappm_installer_files=>load_local( name ).
      WHEN c_enum_zip-server.
        install_data-source_type = 'SERVER'.
        package_data = zcl_abappm_installer_files=>load_server( name ).
      WHEN c_enum_zip-data.
        install_data-source_type = 'DATA'.
        package_data = data.
      WHEN c_enum_zip-registry.
        install_data-source_type = 'REGISTRY'.
        package_data = data.
      WHEN OTHERS.
        zcx_abappm_error=>raise( |Unknown source for package| ).
    ENDCASE.

    progress->show(
      iv_text    = 'Unpacking files from package'
      iv_current = 10 ).

    CASE enum_zip.
      WHEN c_enum_zip-internet.
        remote_files = zcl_abappm_installer_files=>unzip( package_data ).
      WHEN c_enum_zip-local.
        remote_files = zcl_abappm_installer_files=>unzip( package_data ).
      WHEN c_enum_zip-server.
        remote_files = zcl_abappm_installer_files=>unzip( package_data ).
      WHEN c_enum_zip-data.
        remote_files = zcl_abappm_installer_files=>unzip( package_data ).
      WHEN c_enum_zip-registry.
        remote_files = zcl_abappm_installer_files=>untar( package_data ).
      WHEN OTHERS.
        zcx_abappm_error=>raise( |Unknown source for package| ).
    ENDCASE.

    " Scan for viruses and unzip
    progress->show(
      iv_text    = 'Scanning files for viruses'
      iv_current = 20 ).

    zcl_abappm_installer_files=>virus_scan( package_data ).

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

    TRY.
        result = NEW zcl_abapgit_data_config( ).

        READ TABLE remote_files ASSIGNING FIELD-SYMBOL(<remote>)
          WITH KEY path = zif_abapgit_data_config=>c_default_path ##PRIMKEY[FILE_PATH].
        IF sy-subrc = 0.
          result->from_json( remote_files ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD _find_remote_dot_abapgit.

    READ TABLE remote ASSIGNING FIELD-SYMBOL(<remote>) WITH TABLE KEY file_path COMPONENTS
      path     = zif_abapgit_definitions=>c_root_dir
      filename = zif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      TRY.
          result = zcl_abapgit_dot_abapgit=>deserialize( <remote>-data ).
        CATCH zcx_abapgit_exception.
          zcx_abappm_error=>raise( 'Error decoding .abapgit.xml' ).
      ENDTRY.
    ELSE.
      zcx_abappm_error=>raise( |Error finding .abapgit.xml - Is this an package?| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_dot_apack.

    READ TABLE remote ASSIGNING FIELD-SYMBOL(<remote>) WITH TABLE KEY file_path COMPONENTS
      path     = zif_abapgit_definitions=>c_root_dir
      filename = '.apack-manifest.xml'.
    IF sy-subrc = 0.
      zcx_abappm_error=>raise( |Please migrate APACK to package setting| ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_remote_namespaces.

    LOOP AT remote_files ASSIGNING FIELD-SYMBOL(<remote>) WHERE filename CP '*.nspc.xml'.
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


  METHOD _log_end.
    install_data-status = log->get_status( ).
    IF install_data-status <> c_success.
      " FUTURE: Pass log to caller
      " zcl_abapinst_log_viewer=>show_log( log )
    ENDIF.
  ENDMETHOD.


  METHOD _log_start.
    log = NEW zcl_abapgit_log( ).
    log->set_title( |{ sy-title } Log| ).
  ENDMETHOD.


  METHOD _namespaces.

    " Namespaces must be created upfront,
    " otherwise folder_logic->path_to_package will fail
    DATA(remote_files) = _find_remote_namespaces( ).

    IF lines( remote_files ) > 0.
      TRY.
          zcl_abappm_installer_objects=>deserialize(
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


  METHOD _packaging.

    dot_abapgit = _find_remote_dot_abapgit( remote_files ).

    " Check language
    install_data-installed_langu = dot_abapgit->get_main_language( ).

    IF install_data-installed_langu <> sy-langu.
      zcx_abappm_error=>raise(
        |Unable to install. Logon in main language of package which is { install_data-installed_langu }| ).
    ENDIF.

    install_data = CORRESPONDING #( apm_params ).

  ENDMETHOD.


  METHOD _restore_messages.

    DELETE FROM clmcus WHERE username = @sy-uname ##SUBRC_OK.
    INSERT clmcus FROM TABLE @clmcus_backup ##SUBRC_OK.

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
        " TODO: Remove. Transport should be passed from caller
        " install_data-transport = zcl_abapinst_screen=>f4_transport(
        "   iv_package   = install_data-pack
        "   iv_transport = _transport_get( ) )

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
    IF line_exists( request_headers[ trfunction = 'X' trstatus = 'D' korrdev = 'SYST' ] ).
      RETURN.
    ENDIF.

    " Task Type: Development
    IF NOT line_exists( request_headers[ trfunction = 'S' trstatus = 'D' korrdev = 'SYST' ] ).
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
      IF NOT line_exists( request_headers[ trfunction = 'R' trstatus = 'D' korrdev = 'SYST' ] ).
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

    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>) WHERE object = 'DEVC'.

      SELECT * FROM sotr_head INTO TABLE @DATA(sotr_head)
        WHERE paket = @<tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT sotr_head ASSIGNING FIELD-SYMBOL(<sotr_head>).
          DELETE FROM sotr_use WHERE concept = @<sotr_head>-concept.

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

    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>) WHERE object = 'DEVC'.

      SELECT * FROM sotr_headu INTO TABLE @DATA(sotr_head)
        WHERE paket = @<tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT sotr_head ASSIGNING FIELD-SYMBOL(<sotr_head>).
          DELETE FROM sotr_useu WHERE concept = @<sotr_head>-concept.

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
