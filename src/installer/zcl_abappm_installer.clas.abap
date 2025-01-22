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
      BEGIN OF c_enum_folder_logic,
        default TYPE i VALUE 0,
        prefix  TYPE i VALUE 1,
        mixed   TYPE i VALUE 2,
        full    TYPE i VALUE 3,
      END OF c_enum_folder_logic.

    CLASS-METHODS install
      IMPORTING
        !name              TYPE string
        !data              TYPE xstring
        !package           TYPE devclass
        !transport         TYPE trkorr
        !enum_zip          TYPE i
        !enum_folder_logic TYPE i
        !is_production     TYPE abap_bool
      RAISING
        zcx_abappm_error ##NEEDED.

    CLASS-METHODS uninstall
      IMPORTING
        !package   TYPE devclass
        !transport TYPE trkorr OPTIONAL
      RAISING
        zcx_abappm_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      remote_files  TYPE zif_abapgit_git_definitions=>ty_files_tt,
      dot_abapgit   TYPE REF TO zcl_abapgit_dot_abapgit,
      main_language TYPE sy-langu,
      folder_logic  TYPE string,
      log           TYPE REF TO zif_abapgit_log,
      clmcus_backup TYPE STANDARD TABLE OF clmcus WITH DEFAULT KEY.

    CLASS-METHODS _system_check
      RAISING
        zcx_abappm_error.

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

    CLASS-METHODS _folder_logic
      IMPORTING
        !enum_folder_logic TYPE i
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport_check
      IMPORTING
        !package   TYPE devclass
        !transport TYPE trkorr
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _transport_reset.

    CLASS-METHODS _namespaces
      IMPORTING
        !package       TYPE devclass
        !transport     TYPE trkorr
        !main_language TYPE sy-langu
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _confirm_messages.

    CLASS-METHODS _restore_messages.

    CLASS-METHODS _deserialize_objects
      IMPORTING
        !package       TYPE devclass
        !transport     TYPE trkorr
        !main_language TYPE sy-langu
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _deserialize_data
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _log_start.

    CLASS-METHODS _log_end
      RETURNING
        VALUE(result) TYPE sy-msgty
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _find_remote_dot_abapgit
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
        !package TYPE devclass
        !tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CLASS-METHODS _uninstall_sotr
      IMPORTING
        !transport TYPE trkorr
        !tadir     TYPE zif_abapgit_definitions=>ty_tadir_tt.

    CLASS-METHODS _uninstall_sots
      IMPORTING
        !transport TYPE trkorr
        !tadir     TYPE zif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.



CLASS zcl_abappm_installer IMPLEMENTATION.


  METHOD install.

    TRY.
        _log_start( ).

        _system_check( ).

        _files(
          enum_zip       = enum_zip
          name           = name
          data           = data ).

        _packaging( ).

        _folder_logic( enum_folder_logic ).

        _transport_check(
          package   = package
          transport = transport ).

        _confirm_messages( ).

        _namespaces(
          package       = package
          transport     = transport
          main_language = main_language ).

        _deserialize_objects(
          package       = package
          transport     = transport
          main_language = main_language ).

        _deserialize_data( ).

      CATCH cx_root INTO DATA(error).
        _transport_reset( ).

        log->add_exception( error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _restore_messages( ).

      CATCH cx_root INTO error.
        log->add_exception( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD uninstall.

    TRY.
        _log_start( ).

        _system_check( ).

        " TODO: needs to work for apm
        " _transport( c_enum_transport-prompt )

        _confirm_messages( ).

        " A few tries to tackle dependencies
        DO 3 TIMES.
          DATA(tadir) = zcl_abapgit_factory=>get_tadir( )->read( package ).

          DELETE tadir WHERE object = 'NSPC'.

          IF tadir IS NOT INITIAL.
            _uninstall_sotr(
              tadir     = tadir
              transport = transport ).

            _uninstall_sots(
              tadir     = tadir
              transport = transport ).

            " Bridge to abapGit
            zcl_abappm_installer_objects=>delete(
              it_tadir     = tadir
              iv_transport = transport
              ii_log       = log ).
          ENDIF.
        ENDDO.

      CATCH cx_root INTO DATA(error).
        _transport_reset( ).

        log->add_exception( error ).
    ENDTRY.

    TRY.
        _log_end( ).

        _check_uninstalled(
          package = package
          tadir   = tadir ).

        _restore_messages( ).

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
      LOOP AT tadir_list TRANSPORTING NO FIELDS WHERE object <> 'DEVC' AND object <> 'NSPC'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        DATA(msg) = |Some objects could not be uninstalled. Uninstall the remaining objects |
                 && |of pacakge { package } manually|.
      ELSE.
        msg = |Release the transport and deleted the remaining pacakge { package } manually|.
      ENDIF.
      MESSAGE msg TYPE 'I'.
    ENDIF.

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
        " DATA(support) = NEW lcl_abapgit_data_supporter( )
        " FIXME:
        "    CREATE OBJECT inject
        "    inject->set_supporter( lo_support )

        DATA(config) = _find_remote_data_config( ).

        DATA(deserializer) = zcl_abapgit_data_factory=>get_deserializer( ).

        DATA(results) = deserializer->deserialize(
          ii_config = config
          it_files  = remote_files ).

        LOOP AT results INTO DATA(result).
          DATA(overwrite) = VALUE zif_abapgit_definitions=>ty_overwrite(
            obj_type = result-type
            obj_name = result-name
            decision = zif_abapgit_definitions=>c_yes ).
          COLLECT overwrite INTO checks-overwrite.
        ENDLOOP.

        deserializer->actualize(
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
          iv_package   = package
          iv_language  = main_language
          iv_transport = transport
          it_remote    = remote_files
          io_dot       = dot_abapgit
          ii_log       = log ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD _files.

    DATA(progress) = zcl_abapgit_progress=>get_instance( 100 ).

    progress->show(
      iv_text    = 'Uploading package'
      iv_current = 5 ).

    " Load abapGit ZIP File
    CASE enum_zip.
      WHEN c_enum_zip-internet.
        DATA(package_data) = zcl_abappm_installer_files=>load_internet( name ).
      WHEN c_enum_zip-local.
        package_data = zcl_abappm_installer_files=>load_local( name ).
      WHEN c_enum_zip-server.
        package_data = zcl_abappm_installer_files=>load_server( name ).
      WHEN c_enum_zip-data.
        package_data = data.
      WHEN c_enum_zip-registry.
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


  METHOD _find_remote_data_config.

    TRY.
        result = NEW zcl_abapgit_data_config( ).

        IF line_exists( remote_files[ KEY file_path COMPONENTS path = zif_abapgit_data_config=>c_default_path ] ).
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


  METHOD _find_remote_namespaces.

    LOOP AT remote_files ASSIGNING FIELD-SYMBOL(<remote>) WHERE filename CP '*.nspc.xml'.
      INSERT <remote> INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD _folder_logic.

    CASE enum_folder_logic.
      WHEN c_enum_folder_logic-default.
        folder_logic = dot_abapgit->get_folder_logic( ).
      WHEN c_enum_folder_logic-prefix.
        folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
      WHEN c_enum_folder_logic-mixed.
        folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-mixed.
      WHEN c_enum_folder_logic-full.
        folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full.
      WHEN OTHERS.
        zcx_abappm_error=>raise( 'Unknown folder logic' ).
    ENDCASE.

  ENDMETHOD.


  METHOD _log_end.
    result = log->get_status( ).
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
            iv_package   = package
            iv_language  = main_language
            iv_transport = transport
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
    main_language = dot_abapgit->get_main_language( ).

    IF main_language <> sy-langu.
      zcx_abappm_error=>raise(
        |Unable to install. Logon in main language of package which is { main_language }| ).
    ENDIF.

  ENDMETHOD.


  METHOD _restore_messages.

    DELETE FROM clmcus WHERE username = @sy-uname ##SUBRC_OK.
    INSERT clmcus FROM TABLE @clmcus_backup ##SUBRC_OK.

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


  METHOD _transport_check.

    DATA:
      request_header  TYPE trwbo_request_header,
      request_headers TYPE trwbo_request_headers.

    DATA(text) = |apm: Package { package }|.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        trkorr             = transport
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
      zcx_abappm_error=>raise( |Transport { transport } is not a changeable "workbench request"| ).
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
    IF package(1) = '/'.
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


  METHOD _transport_reset.

    TRY.
        zcl_abapgit_factory=>get_default_transport( )->reset( ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD _uninstall_sotr.

    " Necessary since older releases do not delete SOTR when package is deleted

    DATA(use_korr) = xsdbool( transport IS NOT INITIAL ).

    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>) WHERE object = 'DEVC'.

      SELECT * FROM sotr_head INTO TABLE @DATA(sotr_head)
        WHERE paket = @<tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT sotr_head ASSIGNING FIELD-SYMBOL(<sotr_head>).
          DELETE FROM sotr_use WHERE concept = @<sotr_head>-concept.

          CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
            EXPORTING
              concept                  = <sotr_head>-concept
              corr_num                 = transport
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

    DATA(use_korr) = xsdbool( transport IS NOT INITIAL ).

    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>) WHERE object = 'DEVC'.

      SELECT * FROM sotr_headu INTO TABLE @DATA(sotr_head)
        WHERE paket = @<tadir>-obj_name.
      IF sy-subrc = 0.

        LOOP AT sotr_head ASSIGNING FIELD-SYMBOL(<sotr_head>).
          DELETE FROM sotr_useu WHERE concept = @<sotr_head>-concept.

          CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
            EXPORTING
              concept                  = <sotr_head>-concept
              corr_num                 = transport
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
