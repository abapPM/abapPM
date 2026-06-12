* == ABAP Enviroment
CLASS lcl_abap_environment DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS new
      RETURNING
        VALUE(result) TYPE REF TO lcl_abap_environment.

    METHODS get
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_release_patch,
        release  TYPE n LENGTH 3,
        version  TYPE n LENGTH 5,
        patch    TYPE n LENGTH 5,
        platform TYPE c LENGTH 255,
        other    TYPE c LENGTH 255,
      END OF ty_release_patch.

    METHODS get_kernel
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_database
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_hana
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_host
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_connection
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_license
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_language
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS is_gui
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS is_ecatt
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_client
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_timezone
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_spam
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS has_process
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS is_process
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_other
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

*    METHODS is_system_modifiable
*      RETURNING
*        VALUE(result) TYPE abap_bool
*
*    METHODS is_snote_allowed
*      RETURNING
*        VALUE(result) TYPE abap_bool
*
*    METHODS is_upgrage_running
*      RETURNING
*        VALUE(result) TYPE abap_bool
*
*    METHODS is_spam_locked
*      RETURNING
*        VALUE(result) TYPE abap_bool

  PRIVATE SECTION.

    METHODS get_database_release
      RETURNING
        VALUE(result) TYPE dbrelinfo.

    METHODS get_hana_release
      RETURNING
        VALUE(result) TYPE ty_release_patch.

    METHODS get_spam_release
      RETURNING
        VALUE(result) TYPE ty_release_patch.

    METHODS get_kernel_release
      RETURNING
        VALUE(result) TYPE ty_release_patch.

    METHODS get_kernel_cryptolib
      RETURNING
        VALUE(result) TYPE ty_release_patch.

    METHODS get_host_details
      RETURNING
        VALUE(result) TYPE msxxlist.

    METHODS get_work_process
      RETURNING
        VALUE(result) TYPE wpinfo.

    METHODS format
      IMPORTING
        !number       TYPE simple
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS lcl_abap_environment IMPLEMENTATION.

  METHOD new.
    result = NEW #( ).
  ENDMETHOD.

  METHOD get.

    IF name CP 'KERNEL*' OR name = /apmg/if_apm_env=>is_64bit.
      result = get_kernel( name ).
    ELSEIF name CP 'HOST*'.
      result = get_host( name ).
    ELSEIF name CP 'CONN*' OR name = /apmg/if_apm_env=>is_secure_conn.
      result = get_connection( name ).
    ELSEIF name CP 'LICENSE*'.
      result = get_license( name ).
    ELSEIF name CP 'DATABASE*' OR name CP 'DBSL*'.
      result = get_database( name ).
    ELSEIF name CP 'HANA*' OR name = /apmg/if_apm_env=>is_hana.
      result = get_hana( name ).
    ELSEIF name CP 'ABAP*' OR name CP 'IS_CLIENT*'.
      result = get_client( name ).
    ELSEIF name CP 'LANGUAGE*'.
      result = get_language( name ).
    ELSEIF name CP 'TIMEZONE*'.
      result = get_timezone( name ).
    ELSEIF name CP 'SPAM*'.
      result = get_spam( name ).
    ELSEIF name CP 'HAS_PROCESS*'.
      result = has_process( name ).
    ELSEIF name CP 'IS_PROCESS*'.
      result = is_process( name ).
    ELSEIF name CP 'IS_GUI*'.
      result = is_gui( name ).
    ELSEIF name CP 'IS_ECATT*'.
      result = is_ecatt( name ).
    ELSE.
      result = get_other( name ).
    ENDIF.

  ENDMETHOD.

  METHOD get_kernel.

    CASE name.
      WHEN /apmg/if_apm_env=>kernel.
        DATA(release) = format( get_kernel_release( )-release ).
        DATA(patch) = format( get_kernel_release( )-patch ).
        result = |{ release(1) }.{ release+1 }.{ patch }|.
      WHEN /apmg/if_apm_env=>kernel_release.
        result = format( get_kernel_release( )-release ).
      WHEN /apmg/if_apm_env=>kernel_patch.
        result = format( get_kernel_release( )-patch ).
      WHEN /apmg/if_apm_env=>kernel_platform.
        result = get_kernel_release( )-platform.
      WHEN /apmg/if_apm_env=>kernel_arch.
        result = get_kernel_release( )-other.
      WHEN /apmg/if_apm_env=>kernel_cryptolib.
        result = get_kernel_cryptolib( ).
      WHEN /apmg/if_apm_env=>is_64bit.
        result = xsdbool( get_kernel_release( )-version = 64 ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD get_kernel_release.

    " Kernel Info retrival copied from function group SHSY get_kinfo
    TYPES:
      BEGIN OF ty_kernel_info,
        key  TYPE c LENGTH 21,
        data TYPE c LENGTH 400,
      END OF ty_kernel_info.

    DATA:
      kernel_infos      TYPE STANDARD TABLE OF ty_kernel_info WITH KEY key,
      kern_rel          TYPE thllines-thline,
      kern_make_variant TYPE thllines-thline,
      kern_dblib        TYPE thllines-thline,
      kern_patchlevel   TYPE thllines-thline.

    CALL FUNCTION 'TH_SAPREL4'
      IMPORTING
        kern_rel          = kern_rel
        kern_make_variant = kern_make_variant
        kern_dblib        = kern_dblib
        kern_patchlevel   = kern_patchlevel.

    result-platform = kern_make_variant.
    result-release  = kern_rel.
    result-patch    = kern_patchlevel.

    " Kernel Release Information
    CALL 'SAPCORE'
      ID 'ID'    FIELD 'VERSION'
      ID 'TABLE' FIELD kernel_infos.                      "#EC CI_CCALL

    " Machine type: 32- or 64-bit Kernel
    READ TABLE kernel_infos REFERENCE INTO DATA(kernel_info) INDEX 3.
    IF sy-subrc = 0.
      result-other = to_lower( kernel_info->data ).
      REPLACE 'amd64' IN result-other WITH 'x86_64'.
      IF kernel_info->data CS '64'.
        result-version = 64.
      ELSE.
        result-version = 32.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_kernel_cryptolib.

    DATA version TYPE shlib_vers.

    CALL FUNCTION 'GET_SH_LIBRARY_VERSION'
      EXPORTING
        lib_id         = 'CCL'
      IMPORTING
        version        = version
      EXCEPTIONS
        lib_id_unknown = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      SPLIT version AT space INTO result DATA(rest) ##NEEDED.
    ENDIF.

  ENDMETHOD.

  METHOD get_database.

    DATA rest TYPE string.

    CASE name.
      WHEN /apmg/if_apm_env=>database.
        result = get_database_release( )-srvrel.
      WHEN /apmg/if_apm_env=>database_schema.
        result = get_database_release( )-dbschema.
      WHEN /apmg/if_apm_env=>database_host.
        result = get_database_release( )-dbhost.
      WHEN /apmg/if_apm_env=>database_name.
        result = get_database_release( )-dbname.
      WHEN /apmg/if_apm_env=>database_charset.
        result = get_database_release( )-charset.
      WHEN /apmg/if_apm_env=>database_release.
        FIND FIRST OCCURRENCE OF REGEX '(\d+)\.\d+\.*' IN get_database_release( )-srvrel
          SUBMATCHES result ##SUBRC_OK ##REGEX_POSIX.
        result = format( result ).
      WHEN /apmg/if_apm_env=>database_patch.
        FIND FIRST OCCURRENCE OF REGEX '\d+\.(\d+)\.*' IN get_database_release( )-srvrel
          SUBMATCHES result ##SUBRC_OK ##REGEX_POSIX.
        result = format( result ).
      WHEN /apmg/if_apm_env=>dbsl.
        DATA(release) = get_database_release( )-dbsl_vers.
        SPLIT release AT '.' INTO release DATA(patch).
        result = |{ release(1) }.{ release+1 }.{ patch }|.
      WHEN /apmg/if_apm_env=>dbsl_release.
        SPLIT get_database_release( )-dbsl_vers AT '.' INTO result rest.
        result = format( result ).
      WHEN /apmg/if_apm_env=>dbsl_patch.
        SPLIT get_database_release( )-dbsl_vers AT '.' INTO rest result.
        result = format( result ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD get_database_release.

    CALL FUNCTION 'DB_DBRELINFO'
      IMPORTING
        dbinfo = result.

  ENDMETHOD.

  METHOD get_hana.

    IF sy-dbsys = 'HDB'.
      DATA(adbc) = NEW cl_hdb_adbc( 'DEFAULT' ).
    ENDIF.

    CASE name.
      WHEN /apmg/if_apm_env=>is_hana.
        result = xsdbool( sy-dbsys = 'HDB' ).
      WHEN /apmg/if_apm_env=>hana_release.
        result = format( get_hana_release( )-release DIV 100 ).
      WHEN /apmg/if_apm_env=>hana_sp.
        result = format( get_hana_release( )-release MOD 100 ).
      WHEN /apmg/if_apm_env=>hana_revision.
        result = format( get_hana_release( )-version ).
      WHEN /apmg/if_apm_env=>hana_patch.
        result = format( get_hana_release( )-patch ).
      WHEN /apmg/if_apm_env=>hana_platform.
        TRY.
            adbc->get_host_information( IMPORTING platform = result ).
          CATCH cx_dba_adbc INTO DATA(error).
            result = error->get_text( ).
        ENDTRY.
      WHEN /apmg/if_apm_env=>hana_hardware.
        TRY.
            adbc->get_host_information( IMPORTING hw_manufacturer = result ).
          CATCH cx_dba_adbc INTO error.
            result = error->get_text( ).
        ENDTRY.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD get_hana_release.

    DATA dbinfo TYPE dbrelinfo.

    CALL FUNCTION 'DB_DBRELINFO'
      IMPORTING
        dbinfo = dbinfo.

    IF dbinfo-dbsys = 'HDB'.
      " First number in version is release, third one is revision level
      FIND FIRST OCCURRENCE OF REGEX '(\d+)\.(\d+)\.(\d+)\.(\d*)\.\d*' IN dbinfo-srvrel
        SUBMATCHES DATA(text_1) DATA(text_2) result-version result-patch ##REGEX_POSIX.
      IF sy-subrc = 0.
        DATA(hana_rel) = CONV i( text_1 ).
        DATA(hana_sps) = CONV i( text_2 ). "= 0 (except SAP-internally)

        CASE hana_rel.
          WHEN 1.
            IF result-version = 0.
              hana_sps = 0.
            ELSEIF result-version BETWEEN 1 AND 10 ##NUMBER_OK.
              hana_sps = 1.
            ELSEIF result-version BETWEEN 11 AND 18 ##NUMBER_OK.
              hana_sps = 2.
            ELSEIF result-version BETWEEN 19 AND 27 ##NUMBER_OK.
              hana_sps = 3.
            ELSEIF result-version BETWEEN 28 AND 44 ##NUMBER_OK.
              hana_sps = 4.
            ELSEIF result-version BETWEEN 45 AND 59 ##NUMBER_OK.
              hana_sps = 5.
            ELSE.
              hana_sps = result-version DIV 10.
            ENDIF.
          WHEN OTHERS.
            hana_sps = result-version DIV 10.
        ENDCASE.

        DATA(release) = 100 * hana_rel + hana_sps.
        result-release = release.
        IF result-patch > 1000. " it's the changelog for old revisions
          result-patch = 0.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_host.

    DATA instance_number TYPE n LENGTH 2.

    DATA(server) = get_host_details( ).

    CASE name.
      WHEN /apmg/if_apm_env=>host_instance.
        result = server-name.
      WHEN /apmg/if_apm_env=>host_instance_no.
        CALL FUNCTION 'GET_SYSTEM_NUMBER'
          IMPORTING
            instancenumber = instance_number.

        result = instance_number.
      WHEN /apmg/if_apm_env=>host_name.
        result = server-host.
      WHEN /apmg/if_apm_env=>host_state.
        CASE server-state.
          WHEN 1.
            result = 'RUNNING'.
          WHEN 2.
            result = 'HIBERNATE'.
          WHEN 3.
            result = 'SHUTDOWN'.
          WHEN 4.
            result = 'STOP'.
          WHEN 5.
            result = 'STARTING'.
          WHEN 6.
            result = 'INIT'.
          WHEN OTHERS.
            result = /apmg/if_apm_env=>c_unknown.
        ENDCASE.
      WHEN OTHERS.
        result = /apmg/if_apm_env=>c_todo.
        " ASSERT 0 = 1
    ENDCASE.

  ENDMETHOD.

  METHOD get_host_details.

    DATA:
      subrc       TYPE i,
      server_name TYPE tpfet-pvalue,
      server_list TYPE STANDARD TABLE OF msxxlist WITH KEY name.

    CALL FUNCTION 'TH_GET_PARAMETER'
      EXPORTING
        parameter_name  = 'rdisp/myname'
      IMPORTING
        parameter_value = server_name
        rc              = subrc
      EXCEPTIONS
        not_authorized  = 1
        OTHERS          = 2.
    CHECK sy-subrc = 0 AND subrc = 0.

    CALL FUNCTION 'TH_SERVER_LIST'
      TABLES
        list           = server_list
      EXCEPTIONS
        no_server_list = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      READ TABLE server_list INTO result WITH KEY name = server_name ##SUBRC_OK.
    ENDIF.

  ENDMETHOD.

  METHOD get_connection.

    DATA:
      pname_appl TYPE rfcdessecu-pname_appl,
      subrc      TYPE sy-subrc,
      secure     TYPE abap_bool.

    CASE name.
      WHEN /apmg/if_apm_env=>conn_snc_name.
        CALL FUNCTION 'SNC_GET_MY_INFO'
          IMPORTING
            pname_appl     = pname_appl
            rc             = subrc
          EXCEPTIONS
            internal_error = 1
            snc_not_active = 2
            OTHERS         = 3.
        IF sy-subrc = 0 AND subrc = 0.
          result = pname_appl.
        ENDIF.
      WHEN /apmg/if_apm_env=>is_secure_conn.
        CALL FUNCTION 'MS_SECURE_COMMUNICATION'
          IMPORTING
            secure = secure.

        result = secure.
      WHEN OTHERS.
        result = /apmg/if_apm_env=>c_todo.
        " ASSERT 0 = 1
    ENDCASE.

  ENDMETHOD.

  METHOD get_license.

    DATA:
      license_date   TYPE sy-datum,
      license_number TYPE c LENGTH 10.

    CASE name.
      WHEN /apmg/if_apm_env=>license_date.
        CALL FUNCTION 'SLIC_GET_LICENCE_DATE'
          IMPORTING
            licence_date = license_date.

        result = license_date.
      WHEN /apmg/if_apm_env=>license_number.
        CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
          IMPORTING
            license_number = license_number.

        result = license_number.
      WHEN OTHERS.
        result = /apmg/if_apm_env=>c_todo.
        " ASSERT 0 = 1
    ENDCASE.

  ENDMETHOD.

  METHOD get_language.

    DATA langu TYPE sy-langu.
    CASE name.
      WHEN /apmg/if_apm_env=>language_primary.
        cl_suid_tools=>get_language( IMPORTING ev_primary_language = langu ).
        result = langu.
      WHEN /apmg/if_apm_env=>language_secondary.
        cl_suid_tools=>get_language( IMPORTING ev_secondary_language = langu ).
        result = langu.
      WHEN OTHERS.
        result = /apmg/if_apm_env=>c_todo.
        " ASSERT 0 = 1
    ENDCASE.

  ENDMETHOD.

  METHOD is_gui.

    DATA:
      is_gui_running TYPE c LENGTH 1,
      has_activex    TYPE c LENGTH 1,
      has_javabeans  TYPE c LENGTH 1,
      is_its         TYPE c LENGTH 1.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = is_gui_running.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = has_activex.

    IF has_activex IS INITIAL.
      CALL FUNCTION 'GUI_HAS_JAVABEANS'
        IMPORTING
          return = has_javabeans.
    ELSE.
      CALL FUNCTION 'GUI_IS_ITS'
        IMPORTING
          return = is_its.
    ENDIF.

    CASE name.
      WHEN /apmg/if_apm_env=>is_gui_windows.
        result = xsdbool( is_gui_running = abap_true AND has_javabeans = abap_false AND is_its = abap_false ).
      WHEN /apmg/if_apm_env=>is_gui_activex.
        result = has_activex.
      WHEN /apmg/if_apm_env=>is_gui_java.
        result = has_javabeans.
      WHEN /apmg/if_apm_env=>is_gui_web.
        result = is_its.
    ENDCASE.

  ENDMETHOD.

  METHOD is_ecatt.

    DATA is_active TYPE c LENGTH 1.

    CASE name.
      WHEN /apmg/if_apm_env=>is_ecatt_active.
        CALL FUNCTION 'CAT_IS_ACTIVE'
          IMPORTING
            active = is_active.
      WHEN /apmg/if_apm_env=>is_ecatt_recording.
        CALL FUNCTION 'CAT_IS_ACTIVE'
          IMPORTING
            recording = is_active.
      WHEN /apmg/if_apm_env=>is_ecatt_playback.
        CALL FUNCTION 'CAT_IS_ACTIVE'
          IMPORTING
            playback = is_active.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    result = is_active.

  ENDMETHOD.

  METHOD get_client.

    SELECT SINGLE cccategory FROM t000 INTO @DATA(client_type) WHERE mandt = @sy-mandt.
    ASSERT sy-subrc = 0.

    CASE name.
      WHEN /apmg/if_apm_env=>abap_env.
        CASE client_type.
          WHEN 'P'.
            result = /apmg/if_apm_env=>c_client-production.
          WHEN 'T'.
            result = /apmg/if_apm_env=>c_client-test.
          WHEN 'D'.
            result = /apmg/if_apm_env=>c_client-demo.
          WHEN 'E'.
            result = /apmg/if_apm_env=>c_client-training.
          WHEN 'S'.
            result = /apmg/if_apm_env=>c_client-sap.
          WHEN 'C'.
            result = /apmg/if_apm_env=>c_client-customizing.
          WHEN OTHERS.
            result = /apmg/if_apm_env=>c_client-unknown.
        ENDCASE.
      WHEN /apmg/if_apm_env=>is_production_client.
        result = xsdbool( client_type = 'P' ).
      WHEN /apmg/if_apm_env=>is_test_client.
        result = xsdbool( client_type = 'T' ).
      WHEN /apmg/if_apm_env=>is_demo_client.
        result = xsdbool( client_type = 'D' ).
      WHEN /apmg/if_apm_env=>is_training_client.
        result = xsdbool( client_type = 'E' ).
      WHEN /apmg/if_apm_env=>is_sap_client.
        result = xsdbool( client_type = 'S' ).
      WHEN /apmg/if_apm_env=>is_customzing_client.
        result = xsdbool( client_type = 'C' ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD get_timezone.

    CASE name.
      WHEN /apmg/if_apm_env=>timezone_system.
        result = cl_suid_tools=>get_system_time_zone( ).
      WHEN /apmg/if_apm_env=>timezone_user.
        SELECT SINGLE tzone FROM usr02 INTO @result WHERE bname = @sy-uname.
        IF sy-subrc <> 0.
          result = cl_suid_tools=>get_system_time_zone( ).
        ENDIF.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD get_spam.

    CASE name.
      WHEN /apmg/if_apm_env=>spam_release.
        result = format( get_spam_release( )-release ).
      WHEN /apmg/if_apm_env=>spam_version.
        result = format( get_spam_release( )-version ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD get_spam_release.

    DATA spam_vers TYPE n LENGTH 4.

    TRY.
        CALL FUNCTION 'SPAM_VERSION'
          IMPORTING
            version = spam_vers.

        result-release = sy-saprl.                        "#EC SAPRL_OK
        result-version = spam_vers.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD has_process.

    DATA:
      th_bool      TYPE x LENGTH 1,
      is_a_dialog  LIKE th_bool,
      is_a_batch   LIKE th_bool,
      is_a_update  LIKE th_bool,
      is_a_update2 LIKE th_bool,
      is_a_spool   LIKE th_bool,
      is_a_enqueue LIKE th_bool,
      is_a_atp     LIKE th_bool,
      is_a_icman   LIKE th_bool,
      is_a_vmc     LIKE th_bool,
      is_a_j2ee    LIKE th_bool.

    CONSTANTS th_true LIKE th_bool VALUE 1.

    DATA(server) = get_host_details( ).

    CALL FUNCTION 'TH_SERVER_TYPE'
      EXPORTING
        services     = server-msgtypes
      IMPORTING
        is_a_dialog  = is_a_dialog
        is_a_batch   = is_a_batch
        is_a_update  = is_a_update
        is_a_update2 = is_a_update2
        is_a_spool   = is_a_spool
        is_a_enqueue = is_a_enqueue
        is_a_atp     = is_a_atp
        is_a_icman   = is_a_icman
        is_a_vmc     = is_a_vmc
        is_a_j2ee    = is_a_j2ee.

    CASE name.
      WHEN /apmg/if_apm_env=>has_dialog_proc.
        result = xsdbool( is_a_dialog = th_true ).
      WHEN /apmg/if_apm_env=>has_batch_proc.
        result = xsdbool( is_a_batch = th_true ).
      WHEN /apmg/if_apm_env=>has_update_proc.
        result = xsdbool( is_a_update = th_true ).
      WHEN /apmg/if_apm_env=>has_update2_proc.
        result = xsdbool( is_a_update2 = th_true ).
      WHEN /apmg/if_apm_env=>has_spool_proc.
        result = xsdbool( is_a_spool = th_true ).
      WHEN /apmg/if_apm_env=>has_enqueue_proc.
        result = xsdbool( is_a_enqueue = th_true ).
      WHEN /apmg/if_apm_env=>has_atp_proc.
        result = xsdbool( is_a_atp = th_true ).
      WHEN /apmg/if_apm_env=>has_icman_proc.
        result = xsdbool( is_a_icman = th_true ).
      WHEN /apmg/if_apm_env=>has_vmc_proc.
        result = xsdbool( is_a_vmc = th_true ).
      WHEN /apmg/if_apm_env=>has_j2ee_proc.
        result = xsdbool( is_a_j2ee = th_true ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD is_process.

    DATA(work_process) = get_work_process( ).

    CASE name.
      WHEN /apmg/if_apm_env=>is_dialog_proc.
        result = xsdbool( work_process-wp_typ = 'DIA' ).
      WHEN /apmg/if_apm_env=>is_batch_proc.
        result = xsdbool( work_process-wp_typ = 'BTC' ).
      WHEN /apmg/if_apm_env=>is_update_proc.
        result = xsdbool( work_process-wp_typ = 'UPD' ).
      WHEN /apmg/if_apm_env=>is_update2_proc.
        result = xsdbool( work_process-wp_typ = 'UP2' ).
      WHEN /apmg/if_apm_env=>is_spool_proc.
        result = xsdbool( work_process-wp_typ = 'SPO' ).
      WHEN /apmg/if_apm_env=>is_enqueue_proc.
        result = xsdbool( work_process-wp_typ = 'ENQ' ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.

  METHOD get_work_process.

    DATA:
      subrc   TYPE sy-subrc,
      wp_no   TYPE wpinfo-wp_no,
      wp_list TYPE STANDARD TABLE OF wpinfo WITH KEY wp_no.

    CALL FUNCTION 'TH_GET_OWN_WP_NO'
      IMPORTING
        subrc = subrc
        wp_no = wp_no.
    CHECK subrc = 0.

    CALL FUNCTION 'TH_WPINFO'
      TABLES
        wplist     = wp_list
      EXCEPTIONS
        send_error = 1
        OTHERS     = 2.
    CHECK sy-subrc = 0.

    READ TABLE wp_list INTO result WITH TABLE KEY wp_no = wp_no ##SUBRC_OK.

  ENDMETHOD.


  METHOD get_other.

    DATA codepage TYPE cpcodepage.

    CASE name.
      WHEN /apmg/if_apm_env=>endian.
        result = cl_abap_char_utilities=>endian.
      WHEN /apmg/if_apm_env=>is_unicode.
        result = xsdbool( cl_abap_char_utilities=>charsize <> 1 ).
      WHEN /apmg/if_apm_env=>codepage.
        CALL FUNCTION 'SCP_GET_CODEPAGE_NUMBER'
          EXPORTING
            database_also = space
          IMPORTING
            appl_codepage = codepage.

        result = codepage.
      WHEN OTHERS.
        WRITE: / '>>>', name, '(NOT IMPLEMENTED)'.
        " ASSERT 0 = 1
    ENDCASE.

  ENDMETHOD.

  METHOD format.

    result = number.

    SHIFT result LEFT DELETING LEADING '0'.
    IF result IS INITIAL.
      result = '0'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
