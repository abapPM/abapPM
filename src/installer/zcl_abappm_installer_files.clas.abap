CLASS zcl_abappm_installer_files DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS load_internet
      IMPORTING
        !url            TYPE string
        !user           TYPE string OPTIONAL
        !password       TYPE string OPTIONAL
        !proxy_host     TYPE string OPTIONAL
        !proxy_port     TYPE string OPTIONAL
        !proxy_user     TYPE string OPTIONAL
        !proxy_password TYPE string OPTIONAL
      RETURNING
        VALUE(result)   TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS load_local
      IMPORTING
        !filename     TYPE csequence
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS load_server
      IMPORTING
        !filename     TYPE csequence
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS virus_scan
      IMPORTING
        !data TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS unzip
      IMPORTING
        !xstr         TYPE xstring
      RETURNING
        VALUE(result) TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_hex TYPE x LENGTH 1024.

    CLASS-METHODS _filename
      IMPORTING
        !str      TYPE string
      EXPORTING
        !path     TYPE string
        !filename TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _normalize_path
      CHANGING
        !files TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_installer_files IMPLEMENTATION.


  METHOD load_internet.

    DATA:
      client  TYPE REF TO if_http_client,
      code    TYPE i,
      message TYPE string,
      reason  TYPE string.

    TRY.
        DATA(host) = zcl_abapgit_url=>host( url ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    IF proxy_host IS NOT INITIAL AND proxy_port IS NOT INITIAL.
      cl_http_client=>create_by_url(
        EXPORTING
          url                = host
          ssl_id             = 'ANONYM'
          proxy_host         = proxy_host
          proxy_service      = proxy_port
        IMPORTING
          client             = client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    ELSE.
      cl_http_client=>create_by_url(
        EXPORTING
          url                = host
          ssl_id             = 'ANONYM'
        IMPORTING
          client             = client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error creating HTTP client (check certificates in STRUST)| ).
    ENDIF.

    IF proxy_user IS NOT INITIAL AND proxy_password IS NOT INITIAL.
      client->authenticate(
        proxy_authentication = abap_true
        username             = proxy_user
        password             = proxy_password ).
    ENDIF.

    IF user IS NOT INITIAL AND password IS NOT INITIAL.
      client->authenticate(
        username = user
        password = password ).
    ENDIF.

    cl_http_utility=>set_request_uri(
      request = client->request
      uri     = url ).

    client->request->set_method( 'GET' ).
    client->request->set_compression( ).
    client->request->set_header_field(
      name  = 'content-type'
      value = 'application/zip' ).

    client->send(
      EXPORTING
        timeout                    = '6000'
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc  = 0.
      client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      client->get_last_error(
        IMPORTING
          code    = code
          message = message ).

      zcx_abappm_error=>raise( |{ code } { message }| ).
    ENDIF.

    client->response->get_status(
      IMPORTING
        code   = code
        reason = reason ).
    IF code <> 200.
      zcx_abappm_error=>raise( |{ code } { reason }| ).
    ENDIF.

    result = client->response->get_data( ).

    IF result IS INITIAL.
      zcx_abappm_error=>raise( 'Error downloading file. No data returned.' ).
    ENDIF.

    client->close( ).

  ENDMETHOD.


  METHOD load_local.

    DATA:
      file_size  TYPE i,
      data_table TYPE TABLE OF ty_hex WITH DEFAULT KEY.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = |{ filename }|
        filetype                = 'BIN'
      IMPORTING
        filelength              = file_size
      CHANGING
        data_tab                = data_table
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

    CONCATENATE LINES OF data_table INTO result IN BYTE MODE.
    result = result(file_size).

  ENDMETHOD.


  METHOD load_server.

    DATA:
      eps_inbox  TYPE eps2path,
      file_name  TYPE file_name,
      file_size  TYPE i,
      data_table TYPE TABLE OF ty_hex WITH DEFAULT KEY.

    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
      EXPORTING
        eps_subdir             = 'in'
      IMPORTING
        long_dir_name          = eps_inbox
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error getting EPS directory from server| ).
    ENDIF.

    IF eps_inbox CA '\'.
      file_name = eps_inbox && '\' && filename.
    ELSE.
      file_name = eps_inbox && '/' && filename.
    ENDIF.

    CALL FUNCTION 'SCMS_UPLOAD'
      EXPORTING
        filename = file_name
        binary   = abap_true
        frontend = abap_false
      IMPORTING
        filesize = file_size
      TABLES
        data     = data_table
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( |Error loading file from server: { file_name }| ).
    ENDIF.

    CONCATENATE LINES OF data_table INTO result IN BYTE MODE.
    result = result(file_size).

  ENDMETHOD.


  METHOD unzip.

    DATA file_data TYPE xstring.

    DATA(zip) = NEW cl_abap_zip( ).

    zip->load(
      EXPORTING
        zip             = xstr
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error loading ZIP' ).
    ENDIF.

    LOOP AT zip->files ASSIGNING FIELD-SYMBOL(<zipfile>).

      zip->get(
        EXPORTING
          name                    = <zipfile>-name
        IMPORTING
          content                 = file_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Error getting file from ZIP' ).
      ENDIF.

      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<file>).

      _filename(
        EXPORTING
          str      = <zipfile>-name
        IMPORTING
          path     = <file>-path
          filename = <file>-filename ).

      <file>-data = file_data.

      TRY.
          <file>-sha1 = zcl_abapgit_hash=>sha1(
            iv_type = zif_abapgit_git_definitions=>c_type-blob
            iv_data = <file>-data ).
        CATCH zcx_abapgit_exception.
          zcx_abappm_error=>raise( 'Error during hashing' ).
      ENDTRY.

    ENDLOOP.

    DELETE result WHERE filename IS INITIAL.

    _normalize_path( CHANGING files = result ).

  ENDMETHOD.


  METHOD virus_scan.

    DATA:
      scanner   TYPE REF TO cl_vsi,
      scanrc    TYPE vscan_scanrc,
      bapi_msg  TYPE bapiret2,
      bapi_msgs TYPE vscan_bapiret2_t.

    " Data was download from Internet and uploaded here
    " so we will use the HTTP_UPLOAD profile
    cl_vsi=>get_instance(
      EXPORTING
        if_profile         = '/SIHTTP/HTTP_UPLOAD'
      IMPORTING
        eo_instance        = scanner
      EXCEPTIONS
        profile_not_active = 1
        OTHERS             = 2 ).
    CASE sy-subrc.
      WHEN 0.
        " Perform virus scan
        scanner->if_vscan_instance~scan_bytes(
          EXPORTING
            if_data             = data
          IMPORTING
            ef_scanrc           = scanrc
            et_bapiret          = bapi_msgs
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
        IF scanrc <> 0.
          LOOP AT bapi_msgs INTO bapi_msg WHERE type = 'E'.
            MESSAGE ID bapi_msg-id TYPE 'E' NUMBER bapi_msg-number
              WITH bapi_msg-message_v1 bapi_msg-message_v2 bapi_msg-message_v3 bapi_msg-message_v4
              INTO zcx_abappm_error=>null.
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

    IF str CA '/'.
      FIND REGEX '(.*/)(.*)' IN str
        SUBMATCHES path filename.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Malformed path' ).
      ENDIF.
      IF path <> '/'.
        CONCATENATE '/' path INTO path.
      ENDIF.
    ELSE.
      path = '/'.
      filename = str.
    ENDIF.
    TRANSLATE filename TO LOWER CASE.

  ENDMETHOD.


  METHOD _normalize_path.

    " removes first folder from path if needed

    READ TABLE files INDEX 1 ASSIGNING FIELD-SYMBOL(<file>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <file>-path AT '/' INTO TABLE DATA(parts).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE parts INDEX 2 INTO DATA(part).
    IF sy-subrc <> 0 OR strlen( part ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' part '/*' INTO part.

    DATA(needed) = abap_true.
    LOOP AT files ASSIGNING <file>.
      IF <file>-path NP part.
        needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF needed = abap_true.
      DATA(length) = strlen( part ) - 2.
      LOOP AT files ASSIGNING <file>.
        <file>-path = <file>-path+length.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
