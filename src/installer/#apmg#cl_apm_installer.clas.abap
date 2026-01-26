CLASS /apmg/cl_apm_installer DEFINITION
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
      BEGIN OF c_enum_source,
        local    TYPE i VALUE 0,
        internet TYPE i VALUE 1,
        server   TYPE i VALUE 2,
        data     TYPE i VALUE 3,
        registry TYPE i VALUE 4,
      END OF c_enum_source.

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
        !version           TYPE string
        !data              TYPE xstring
        !package           TYPE devclass
        !transport         TYPE trkorr OPTIONAL
        !enum_source       TYPE i " FUTURE
        !enum_folder_logic TYPE i " FUTURE
        !is_production     TYPE abap_bool
      RAISING
        /apmg/cx_apm_error ##NEEDED.

    CLASS-METHODS uninstall
      IMPORTING
        !name      TYPE string
        !version   TYPE string
        !package   TYPE devclass
        !transport TYPE trkorr OPTIONAL
      RAISING
        /apmg/cx_apm_error.

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
        /apmg/cx_apm_error.

    CLASS-METHODS _files
      IMPORTING
        !enum_source TYPE i
        !name        TYPE string OPTIONAL
        !data        TYPE xstring OPTIONAL
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _packaging
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _folder_logic
      IMPORTING
        !enum_folder_logic TYPE i
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _transport_check
      IMPORTING
        !package   TYPE devclass
        !transport TYPE trkorr
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _transport_reset.

    CLASS-METHODS _namespaces
      IMPORTING
        !package       TYPE devclass
        !transport     TYPE trkorr
        !main_language TYPE sy-langu
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _confirm_messages.

    CLASS-METHODS _restore_messages.

    CLASS-METHODS _deserialize_objects
      IMPORTING
        !package       TYPE devclass
        !transport     TYPE trkorr
        !main_language TYPE sy-langu
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _log_start
      IMPORTING
        !title   TYPE string
        !name    TYPE string
        !version TYPE string.

    CLASS-METHODS _log_end
      RETURNING
        VALUE(result) TYPE sy-msgty
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _find_remote_dot_abapgit
      IMPORTING
        !remote       TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS _find_remote_namespaces
      RETURNING
        VALUE(result) TYPE zif_abapgit_git_definitions=>ty_files_tt.

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



CLASS /apmg/cl_apm_installer IMPLEMENTATION.


  METHOD install.

    TRY.
        _log_start(
          title   = 'Install'
          name    = name
          version = version ).

        _system_check( ).

        _files(
          enum_source = enum_source
          name        = name
          data        = data ).

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
        _log_start(
          title   = 'Uninstall'
          name    = name
          version = version ).

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
            /apmg/cl_apm_abapgit_objects=>delete(
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


  METHOD _deserialize_objects.

    " Bridge to abapGit
    TRY.
        /apmg/cl_apm_abapgit_objects=>deserialize(
          iv_package   = package
          iv_language  = main_language
          iv_transport = transport
          it_remote    = remote_files
          io_dot       = dot_abapgit
          ii_log       = log ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD _files.

    DATA(progress) = /apmg/cl_apm_progress_bar=>get_instance( 100 ).

    progress->show(
      text    = 'Uploading package'
      current = 5 ).

    " Load ZIP File
    CASE enum_source.
      WHEN c_enum_source-internet.
        DATA(package_data) = /apmg/cl_apm_installer_files=>load_internet( name ).
      WHEN c_enum_source-local.
        package_data = /apmg/cl_apm_installer_files=>load_local( name ).
      WHEN c_enum_source-server.
        package_data = /apmg/cl_apm_installer_files=>load_server( name ).
      WHEN c_enum_source-data.
        package_data = data.
      WHEN c_enum_source-registry.
        package_data = data.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Unknown source for package'.
    ENDCASE.

    progress->show(
      text    = 'Unpacking files from package'
      current = 10 ).

    CASE enum_source.
      WHEN c_enum_source-internet
        OR c_enum_source-local
        OR c_enum_source-server
        OR c_enum_source-data.
        remote_files = /apmg/cl_apm_installer_files=>unzip( package_data ).
      WHEN c_enum_source-registry.
        remote_files = /apmg/cl_apm_installer_files=>untar( package_data ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Unknown source for package'.
    ENDCASE.

    " Scan for viruses and unzip
    progress->show(
      text    = 'Scanning files for viruses'
      current = 20 ).

    /apmg/cl_apm_installer_files=>virus_scan( package_data ).

  ENDMETHOD.


  METHOD _find_remote_dot_abapgit.

    READ TABLE remote ASSIGNING FIELD-SYMBOL(<remote>) WITH TABLE KEY file_path COMPONENTS
      path     = zif_abapgit_definitions=>c_root_dir
      filename = zif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      TRY.
          result = zcl_abapgit_dot_abapgit=>deserialize( <remote>-data ).
        CATCH zcx_abapgit_exception.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Error decoding .abapgit.xml'.
      ENDTRY.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Error finding .abapgit.xml - Is this an apm package?'.
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
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Unknown folder logic'.
    ENDCASE.

  ENDMETHOD.


  METHOD _log_end.
    result = log->get_status( ).
  ENDMETHOD.


  METHOD _log_start.
    log = NEW zcl_abapgit_log( ).
    log->set_title( |{ title } Log for { name }@{ version }| ).
  ENDMETHOD.


  METHOD _namespaces.

    " Namespaces must be created upfront,
    " otherwise folder_logic->path_to_package will fail
    DATA(remote_files) = _find_remote_namespaces( ).

    IF lines( remote_files ) > 0.
      TRY.
          /apmg/cl_apm_abapgit_objects=>deserialize(
            iv_package   = package
            iv_language  = main_language
            iv_transport = transport
            it_remote    = remote_files
            io_dot       = dot_abapgit
            ii_log       = log ).

          COMMIT WORK.
        CATCH zcx_abapgit_exception INTO DATA(error).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD _packaging.

    dot_abapgit = _find_remote_dot_abapgit( remote_files ).

    " Check language
    main_language = dot_abapgit->get_main_language( ).

    IF main_language <> sy-langu.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Unable to install. Logon in main language of package which is { main_language }|.
    ENDIF.

  ENDMETHOD.


  METHOD _restore_messages.

    CONSTANTS c_toolflag_reset TYPE funcname VALUE 'SCWG_TOOLFLAG_RESET'.

    " Reset tool flag
    TRY.
        CALL FUNCTION c_toolflag_reset.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

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
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    IF systemedit = 'N'.
      MESSAGE e102(tk) INTO /apmg/cx_apm_error=>null.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.
    IF sys_cliinddep_edit CA '23'.
      MESSAGE e729(tk) INTO /apmg/cx_apm_error=>null.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
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
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    " Request Type: Workbench
    READ TABLE request_headers INTO request_header
      WITH KEY trfunction = 'K' trstatus = 'D' korrdev = 'SYST'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Transport { transport } is not a changeable workbench request|.
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
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
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
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
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
          DELETE FROM sotr_use WHERE concept = @<sotr_head>-concept ##SUBRC_OK.

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

      TRY.
          zcl_abapgit_factory=>get_tadir( )->delete_single(
            iv_object   = 'SOTR'
            iv_obj_name = <tadir>-obj_name ).
        CATCH zcx_abapgit_exception ##NO_HANDLER.
      ENDTRY.

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
          DELETE FROM sotr_useu WHERE concept = @<sotr_head>-concept ##SUBRC_OK.

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

      TRY.
          zcl_abapgit_factory=>get_tadir( )->delete_single(
            iv_object   = 'SOTS'
            iv_obj_name = <tadir>-obj_name ).
        CATCH zcx_abapgit_exception ##NO_HANDLER.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
