CLASS zcl_abappm_installer_files DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS load_internet
      IMPORTING
        !iv_url            TYPE string
        !iv_user           TYPE string
        !iv_password       TYPE string
        !iv_proxy_host     TYPE string
        !iv_proxy_port     TYPE string
        !iv_proxy_user     TYPE string
        !iv_proxy_password TYPE string
      RETURNING
        VALUE(rv_file)     TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS load_local
      IMPORTING
        !iv_filename   TYPE csequence
      RETURNING
        VALUE(rv_file) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS load_server
      IMPORTING
        !iv_filename   TYPE csequence
      RETURNING
        VALUE(rv_file) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS virus_scan
      IMPORTING
        !iv_data TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS unzip
      IMPORTING
        !iv_xstr        TYPE xstring
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS _filename
      IMPORTING
        !iv_str      TYPE string
      EXPORTING
        !ev_path     TYPE string
        !ev_filename TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _normalize_path
      CHANGING
        !ct_files TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_installer_files IMPLEMENTATION.


  METHOD load_internet.

    DATA:
      li_client  TYPE REF TO if_http_client,
      lx_error   TYPE REF TO zcx_abapgit_exception,
      lv_url     TYPE string,
      lv_code    TYPE i,
      lv_message TYPE string,
      lv_reason  TYPE string.

    TRY.
        lv_url = zcl_abapgit_url=>host( iv_url ).
      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    IF iv_proxy_host IS NOT INITIAL AND iv_proxy_port IS NOT INITIAL.
      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_url
          ssl_id             = 'ANONYM'
          proxy_host         = iv_proxy_host
          proxy_service      = iv_proxy_port
        IMPORTING
          client             = li_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    ELSE.
      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_url
          ssl_id             = 'ANONYM'
        IMPORTING
          client             = li_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error creating HTTP client (check certificates in STRUST)| ).
    ENDIF.

    IF iv_proxy_user IS NOT INITIAL AND iv_proxy_password IS NOT INITIAL.
      li_client->authenticate(
        proxy_authentication = abap_true
        username             = iv_proxy_user
        password             = iv_proxy_password ).
    ENDIF.

    IF iv_user IS NOT INITIAL AND iv_password IS NOT INITIAL.
      li_client->authenticate(
        username = iv_user
        password = iv_password ).
    ENDIF.

    cl_http_utility=>set_request_uri(
      request = li_client->request
      uri     = iv_url ).

    li_client->request->set_method( 'GET' ).
    li_client->request->set_compression( ).
    li_client->request->set_header_field(
      name  = 'content-type'
      value = 'application/zip' ).

    li_client->send(
      EXPORTING
        timeout                    = '6000'
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc  = 0.
      li_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      li_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).

      zcx_abappm_error=>raise( |{ lv_code } { lv_message }| ).
    ENDIF.

    li_client->response->get_status(
      IMPORTING
        code   = lv_code
        reason = lv_reason ).
    IF lv_code <> 200.
      zcx_abappm_error=>raise( |{ lv_code } { lv_reason }| ).
    ENDIF.

    rv_file = li_client->response->get_data( ).

    IF rv_file IS INITIAL.
      zcx_abappm_error=>raise( 'Error downloading file. No data returned.' ).
    ENDIF.

    li_client->close( ).

  ENDMETHOD.


  METHOD load_local.

    TYPES:
      ty_hex TYPE x LENGTH 255.

    DATA:
      lt_data   TYPE TABLE OF ty_hex WITH DEFAULT KEY,
      lv_length TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = |{ iv_filename }|
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
      zcx_abappm_error=>raise_t100( ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_file IN BYTE MODE.
    rv_file = rv_file(lv_length).

  ENDMETHOD.


  METHOD load_server.

    DATA:
      lv_eps_inbox TYPE eps2path,
      lv_filename  TYPE file_name,
      lv_filesize  TYPE i,
      lv_data      TYPE x LENGTH 1024,
      lt_data      LIKE TABLE OF lv_data.

    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
      EXPORTING
        eps_subdir             = 'in'
      IMPORTING
        ev_long_dir_name       = lv_eps_inbox
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error getting EPS directory from server| ).
    ENDIF.

    IF lv_eps_inbox CA '\'.
      lv_filename = lv_eps_inbox && '\' && iv_filename.
    ELSE.
      lv_filename = lv_eps_inbox && '/' && iv_filename.
    ENDIF.

    CALL FUNCTION 'SCMS_UPLOAD'
      EXPORTING
        filename = lv_filename
        binary   = abap_true
        frontend = abap_false
      IMPORTING
        filesize = lv_filesize
      TABLES
        data     = lt_data
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error loading file from server: { lv_filename }| ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_file IN BYTE MODE.
    rv_file = rv_file(lv_filesize).

  ENDMETHOD.


  METHOD unzip.

    DATA:
      lo_zip  TYPE REF TO cl_abap_zip,
      lv_data TYPE xstring.

    FIELD-SYMBOLS:
      <ls_zipfile> LIKE LINE OF lo_zip->files,
      <ls_file>    LIKE LINE OF rt_files.

    CREATE OBJECT lo_zip.

    lo_zip->load(
      EXPORTING
        zip             = iv_xstr
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error loading ZIP' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Error getting file from ZIP' ).
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.

      _filename(
        EXPORTING
          iv_str      = <ls_zipfile>-name
        IMPORTING
          ev_path     = <ls_file>-path
          ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_data.

      TRY.
          <ls_file>-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_git_definitions=>c_type-blob
                                                   iv_data = <ls_file>-data ).
        CATCH zcx_abapgit_exception.
          zcx_abappm_error=>raise( 'Error during hashing' ).
      ENDTRY.

    ENDLOOP.

    DELETE rt_files WHERE filename IS INITIAL.

    _normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.


  METHOD virus_scan.

    DATA:
      lo_scanner  TYPE REF TO cl_vsi,
      lv_scanrc   TYPE vscan_scanrc,
      lv_msg      TYPE string ##NEEDED,
      ls_message  TYPE bapiret2,
      lt_bapiret2 TYPE vscan_bapiret2_t.

    " Data was download from Internet and uploaded here
    " so we will use the HTTP_UPLOAD profile
    cl_vsi=>get_instance(
      EXPORTING
        if_profile         = '/SIHTTP/HTTP_UPLOAD'
      IMPORTING
        eo_instance        = lo_scanner
      EXCEPTIONS
        profile_not_active = 1
        OTHERS             = 2 ).
    CASE sy-subrc.
      WHEN 0.
        " Perform virus scan
        lo_scanner->if_vscan_instance~scan_bytes(
          EXPORTING
            if_data             = iv_data
          IMPORTING
            ef_scanrc           = lv_scanrc
            et_bapiret          = lt_bapiret2
          EXCEPTIONS
            not_available       = 1
            configuration_error = 2
            internal_error      = 3
            OTHERS              = 4 ).
        " Severe errors of the scanner (NOT: Virus found) are reported
        " as exceptions and must be reported as technical errors
        IF sy-subrc <> 0.
          zcx_abappm_error=>raise_t100( ).
        ENDIF.

        " Result of virus scan
        " Any scan error or virus infection will be reported there
        IF lv_scanrc <> 0.
          LOOP AT lt_bapiret2 INTO ls_message WHERE type = 'E'.
            MESSAGE ID ls_message-id TYPE 'E' NUMBER ls_message-number
              WITH ls_message-message_v1 ls_message-message_v2 ls_message-message_v3 ls_message-message_v4
              INTO lv_msg.
            zcx_abappm_error=>raise_t100( ).
          ENDLOOP.
        ENDIF.

      WHEN 1.
        " No Virus Scan active --> nothing to do
      WHEN 2.
        " Error getting scanner. Reporting needed
        zcx_abappm_error=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.


  METHOD _filename.

    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES ev_path ev_filename.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Malformed path' ).
      ENDIF.
      IF ev_path <> '/'.
        CONCATENATE '/' ev_path INTO ev_path.
      ENDIF.
    ELSE.
      ev_path = '/'.
      ev_filename = iv_str.
    ENDIF.
    TRANSLATE ev_filename TO LOWER CASE.

  ENDMETHOD.


  METHOD _normalize_path.
* removes first folder from path if needed

    DATA:
      lt_split  TYPE TABLE OF string,
      lv_needed TYPE abap_bool,
      lv_length TYPE i,
      lv_split  LIKE LINE OF lt_split.

    FIELD-SYMBOLS:
      <ls_file> LIKE LINE OF ct_files.

    READ TABLE ct_files INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <ls_file>-path AT '/' INTO TABLE lt_split.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_split INDEX 2 INTO lv_split.
    IF sy-subrc <> 0 OR strlen( lv_split ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' lv_split '/*' INTO lv_split.

    lv_needed = abap_true.
    LOOP AT ct_files ASSIGNING <ls_file>.
      IF <ls_file>-path NP lv_split.
        lv_needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF lv_needed = abap_true.
      lv_length = strlen( lv_split ) - 2.
      LOOP AT ct_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_file>-path+lv_length.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
