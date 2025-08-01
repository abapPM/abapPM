CLASS /apmg/cl_apm_frontend_services DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS /apmg/cl_apm_gui_factory.

  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_frontend_services.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gv_initial_folder TYPE string.

    METHODS get_path_from_fullname
      IMPORTING
        iv_fullname    TYPE string
      RETURNING
        VALUE(rv_path) TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_frontend_services IMPLEMENTATION.


  METHOD /apmg/if_apm_frontend_services~clipboard_export.

    DATA lv_rc TYPE i.

    " Note: do not use a string table for 'it_data'!

    TRY.
        CALL METHOD cl_gui_frontend_services=>('CLIPBOARD_EXPORT')
          EXPORTING
            no_auth_check        = iv_no_auth_check " >= 740
          IMPORTING
            data                 = it_data
          CHANGING
            rc                   = lv_rc
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            no_authority         = 4
            OTHERS               = 5.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
        ENDIF.

      CATCH cx_sy_dyn_call_param_missing.

        cl_gui_frontend_services=>clipboard_export(
          IMPORTING
            data                 = it_data
          CHANGING
            rc                   = lv_rc
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            no_authority         = 4
          OTHERS               = 5 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
        ENDIF.

    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~directory_browse.

    IF iv_initial_folder IS NOT INITIAL.
      gv_initial_folder = iv_initial_folder.
    ENDIF.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = iv_window_title
        initial_folder       = gv_initial_folder
      CHANGING
        selected_folder      = cv_selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    gv_initial_folder = cv_selected_folder.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~directory_create.

    cl_gui_frontend_services=>directory_create(
      EXPORTING
        directory                = iv_directory
      CHANGING
        rc                       = cv_rc
      EXCEPTIONS
        directory_create_failed  = 1
        cntl_error               = 2
        error_no_gui             = 3
        directory_access_denied  = 4
        directory_already_exists = 5
        path_not_found           = 6
        unknown_error            = 7
        not_supported_by_gui     = 8
        wrong_parameter          = 9
        OTHERS                   = 10 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~directory_exist.

    cl_gui_frontend_services=>directory_exist(
      EXPORTING
        directory            = iv_directory
      RECEIVING
        result               = rv_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~execute.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = iv_document
        application            = iv_application
        parameter              = iv_parameter
        default_directory      = iv_default_directory
        maximized              = iv_maximized
        minimized              = iv_minimized
        synchronous            = iv_synchronous
        operation              = iv_operation
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~file_download.

    TYPES ty_hex TYPE x LENGTH 200.
    DATA lt_rawdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY.

    zcl_abapgit_convert=>xstring_to_bintab(
      EXPORTING iv_xstr   = iv_xstr
      IMPORTING et_bintab = lt_rawdata ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_xstr )
        filename                  = iv_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~file_upload.

    TYPES: ty_hex TYPE x LENGTH 255.

    DATA: lt_data   TYPE TABLE OF ty_hex WITH DEFAULT KEY,
          lv_length TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xstr IN BYTE MODE.
    rv_xstr = rv_xstr(lv_length).

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~get_file_separator.

    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = cv_file_separator
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~get_gui_version.

    DATA:
      lt_version_table TYPE filetable,
      lv_rc            TYPE i,
      ls_version       LIKE LINE OF lt_version_table.

    cl_gui_frontend_services=>get_gui_version(
      CHANGING
        version_table            = lt_version_table
        rc                       = lv_rc
      EXCEPTIONS
        get_gui_version_failed   = 1
        cant_write_version_table = 2
        gui_no_version           = 3
        cntl_error               = 4
        error_no_gui             = 5
        not_supported_by_gui     = 6
        OTHERS                   = 7 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

    READ TABLE lt_version_table INTO ls_version INDEX 1. " gui release
    ev_gui_release = ls_version-filename.
    READ TABLE lt_version_table INTO ls_version INDEX 2. " gui sp
    ev_gui_sp = ls_version-filename.
    READ TABLE lt_version_table INTO ls_version INDEX 3. " gui patch
    ev_gui_patch = ls_version-filename.

    ev_gui_version_string = |{ ev_gui_release }.{ condense( ev_gui_sp ) }.{ condense( ev_gui_patch ) }|.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~get_system_directory.

    cl_gui_frontend_services=>get_system_directory(
      CHANGING
        system_directory     = cv_system_directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~gui_is_available.

    TRY.
        CALL FUNCTION 'GUI_IS_AVAILABLE'
          IMPORTING
            return = rv_gui_is_available.
      CATCH cx_sy_dyn_call_illegal_func.
* when running on open-abap
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~is_sapgui_for_java.

    TRY.
        CALL FUNCTION 'GUI_HAS_JAVABEANS'
          IMPORTING
            return = rv_result.
      CATCH cx_sy_dyn_call_illegal_func.
* when running on open-abap
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~is_sapgui_for_windows.

    TRY.
        CALL FUNCTION 'GUI_HAS_ACTIVEX'
          IMPORTING
            return = rv_result.
      CATCH cx_sy_dyn_call_illegal_func.
* when running on open-abap
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~is_webgui.

    TRY.
        CALL FUNCTION 'GUI_IS_ITS'
          IMPORTING
            return = rv_is_webgui.
      CATCH cx_sy_dyn_call_illegal_func.
* when running on open-abap
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~open_ie_devtools.

    DATA: lv_system_directory TYPE string,
          lv_exe_full_path    TYPE string.

    IF /apmg/if_apm_frontend_services~is_sapgui_for_windows( ) = abap_false.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'IE DevTools not supported on frontend OS'.
    ENDIF.

    /apmg/if_apm_frontend_services~get_system_directory( CHANGING cv_system_directory = lv_system_directory ).

    cl_gui_cfw=>flush( ).

    lv_exe_full_path = lv_system_directory && `\F12\IEChooser.exe`.

    /apmg/if_apm_frontend_services~execute( iv_application = lv_exe_full_path ).

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~show_file_open_dialog.

    DATA:
      lt_file_table TYPE filetable,
      ls_file_table LIKE LINE OF lt_file_table,
      lv_filter     TYPE string,
      lv_action     TYPE i,
      lv_rc         TYPE i.

    IF iv_extension = 'zip'.
      lv_filter = 'ZIP Files (*.zip)|*.zip|' && cl_gui_frontend_services=>filetype_all.
    ENDIF.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = iv_title
        default_filename        = iv_default_filename
        file_filter             = lv_filter
        initial_directory       = gv_initial_folder
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Cancelled'.
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.
    rv_path = ls_file_table-filename.

    gv_initial_folder = get_path_from_fullname( rv_path ).

  ENDMETHOD.


  METHOD /apmg/if_apm_frontend_services~show_file_save_dialog.

    DATA:
      lv_action   TYPE i,
      lv_filter   TYPE string,
      lv_filename TYPE string,
      lv_path     TYPE string.

    IF iv_extension = 'zip'.
      lv_filter = 'ZIP Files (*.zip)|*.zip|' && cl_gui_frontend_services=>filetype_all.
    ENDIF.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = iv_title
        default_extension    = iv_extension
        default_file_name    = iv_default_filename
        file_filter          = lv_filter
        initial_directory    = gv_initial_folder
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = rv_path
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Cancelled'.
    ENDIF.

    gv_initial_folder = lv_path.

  ENDMETHOD.


  METHOD get_path_from_fullname.

    DATA lv_len TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullname MATCH LENGTH lv_len.
    IF sy-subrc = 0.
      rv_path = iv_fullname(lv_len).
    ELSE.
      FIND FIRST OCCURRENCE OF REGEX '^(.*\\)?' IN iv_fullname MATCH LENGTH lv_len.
      IF sy-subrc = 0.
        rv_path = iv_fullname(lv_len).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
