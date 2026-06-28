INTERFACE /apmg/if_apm_env PUBLIC.

************************************************************************
* Environment
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: Adding constants not related to environment variables with
* require adjusting the select in cl_env->constructor
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.0'.

  CONSTANTS:
    BEGIN OF c_env,
      abap    TYPE string VALUE 'ABAP',
      os      TYPE string VALUE 'OS',
      profile TYPE string VALUE 'PROFILE',
    END OF c_env.

  " Note: When adding or changing constants, ...
  " - names must not match SAP software components
  " - add value to cl_env=>init_abap
  " - check mapping in lcl_abap_environment=>get
  " - implement getter method in lcl_abap_environment
  CONSTANTS:
    abap_env                   TYPE string VALUE 'ABAP_ENV',
    " Kernel
    kernel                     TYPE string VALUE 'KERNEL',
    kernel_platform            TYPE string VALUE 'KERNEL_PLATFORM',
    kernel_type                TYPE string VALUE 'KERNEL_TYPE',
    kernel_arch                TYPE string VALUE 'KERNEL_ARCH',
    kernel_release             TYPE string VALUE 'KERNEL_RELEASE',
    kernel_patch               TYPE string VALUE 'KERNEL_PATCH',
    kernel_cryptolib           TYPE string VALUE 'KERNEL_CRYPTOLIB',
    endian                     TYPE string VALUE 'ENDIAN',
    is_64bit                   TYPE string VALUE 'IS_64BIT',
    " Host
    host_name                  TYPE string VALUE 'HOST_NAME',
    host_instance              TYPE string VALUE 'HOST_INSTANCE',
    host_instance_no           TYPE string VALUE 'HOST_INSTANCE_NO',
    host_state                 TYPE string VALUE 'HOST_STATE',
    " Connection
    conn_snc_name              TYPE string VALUE 'CONN_SNC_NAME',
    is_secure_conn             TYPE string VALUE 'IS_SECURE_CONN',
    " License
    hardware_key               TYPE string VALUE 'HARDWARE_KEY',
    license_number             TYPE string VALUE 'LICENSE_NUMBER',
    license_exp_date           TYPE string VALUE 'LICENSE_EXP_DATE',
    " Database
    database                   TYPE string VALUE 'DATABASE',
    database_platform          TYPE string VALUE 'DATABASE_PLATFORM',
    database_release           TYPE string VALUE 'DATABASE_RELEASE',
    database_patch             TYPE string VALUE 'DATABASE_PATCH',
    database_schema            TYPE string VALUE 'DATABASE_SCHEMA',
    database_host              TYPE string VALUE 'DATABASE_HOST',
    database_name              TYPE string VALUE 'DATABASE_NAME',
    database_charset           TYPE string VALUE 'DATABASE_CHARSET',
    " Database Library
    dbsl                       TYPE string VALUE 'DBSL',
    dbsl_release               TYPE string VALUE 'DBSL_RELEASE',
    dbsl_patch                 TYPE string VALUE 'DBSL_PATCH',
    " SAP HANA
    hana_release               TYPE string VALUE 'HANA_RELEASE',
    hana_sp                    TYPE string VALUE 'HANA_SP',
    hana_revision              TYPE string VALUE 'HANA_REVISION',
    hana_patch                 TYPE string VALUE 'HANA_PATCH',
    hana_platform              TYPE string VALUE 'HANA_PLATFORM',
    hana_hardware              TYPE string VALUE 'HANA_HARDWARE',
    hana_hardware_key          TYPE string VALUE 'HANA_HARDWARE_KEY',
    hana_license_number        TYPE string VALUE 'HANA_LICENSE_NUMBER',
    hana_license_exp_date      TYPE string VALUE 'HANA_LICENSE_EXP_DATE',
    is_hana                    TYPE string VALUE 'IS_HANA',
    " System
    system_type                TYPE string VALUE 'SYSTEM_TYPE',
    is_system_changeable       TYPE string VALUE 'IS_SYSTEM_CHANGEABLE',
    is_cloud_system            TYPE string VALUE 'IS_CLOUD_SYSTEM',
    is_s4hana_system           TYPE string VALUE 'IS_S4HANA_SYSTEM',
    is_shadow_system           TYPE string VALUE 'IS_SHADOW_SYSTEM',
    " Client
    client                     TYPE string VALUE 'CLIENT',
    is_client_changeable       TYPE string VALUE 'IS_CLIENT_CHANGEABLE',
    is_cross_client_changeable TYPE string VALUE 'IS_CROSS_CLIENT_CHANGEABLE',
    is_repository_changeable   TYPE string VALUE 'IS_REPOSITORY_CHANGEABLE',
    is_customizing_client      TYPE string VALUE 'IS_CLIENT_CUSTOMIZING',
    is_demo_client             TYPE string VALUE 'IS_CLIENT_DEMO',
    is_production_client       TYPE string VALUE 'IS_CLIENT_PRODUTION',
    is_sap_client              TYPE string VALUE 'IS_CLIENT_SAP',
    is_test_client             TYPE string VALUE 'IS_CLIENT_TEST',
    is_training_client         TYPE string VALUE 'IS_CLIENT_TRAINING',
    " Timezones
    timezone_system            TYPE string VALUE 'TIMEZONE_SYSTEM',
    timezone_user              TYPE string VALUE 'TIMEZONE_USER',
    " Support Package Manager
    spam_release               TYPE string VALUE 'SPAM_RELEASE',
    spam_version               TYPE string VALUE 'SPAM_VERSION',
    is_spam_locked             TYPE string VALUE 'IS_SPAM_LOCKED',
    " Language
    language_primary           TYPE string VALUE 'LANGUAGE_PRIMARY',
    language_secondary         TYPE string VALUE 'LANGUAGE_SECONDARY',
    " Characters
    codepage                   TYPE string VALUE 'CODEPAGE',
    is_unicode                 TYPE string VALUE 'IS_UNICODE',
    " Process
    has_dialog_proc            TYPE string VALUE 'HAS_PROCESS_DIALOG',
    has_batch_proc             TYPE string VALUE 'HAS_PROCESS_BATCH',
    has_update_proc            TYPE string VALUE 'HAS_PROCESS_UPDATE',
    has_update2_proc           TYPE string VALUE 'HAS_PROCESS_UPDATE2',
    has_spool_proc             TYPE string VALUE 'HAS_PROCESS_SPOOL',
    has_enqueue_proc           TYPE string VALUE 'HAS_PROCESS_ENQUEUE',
    has_atp_proc               TYPE string VALUE 'HAS_PROCESS_ATP',
    has_icman_proc             TYPE string VALUE 'HAS_PROCESS_ICMAN',
    has_vmc_proc               TYPE string VALUE 'HAS_PROCESS_VMC',
    has_j2ee_proc              TYPE string VALUE 'HAS_PROCESS_J2EE',
    is_dialog_proc             TYPE string VALUE 'IS_PROCESS_DIALOG',
    is_batch_proc              TYPE string VALUE 'IS_PROCESS_BATCH',
    is_update_proc             TYPE string VALUE 'IS_PROCESS_UPDATE',
    is_update2_proc            TYPE string VALUE 'IS_PROCESS_UPDATE2',
    is_spool_proc              TYPE string VALUE 'IS_PROCESS_SPOOL',
    is_enqueue_proc            TYPE string VALUE 'IS_PROCESS_ENQUEUE',
    " GUI
    is_gui_windows             TYPE string VALUE 'IS_GUI_WINDOWS',
    is_gui_activex             TYPE string VALUE 'IS_GUI_ACTIVEX',
    is_gui_java                TYPE string VALUE 'IS_GUI_JAVA',
    is_gui_web                 TYPE string VALUE 'IS_GUI_WEB',
    " ECATT
    is_ecatt_active            TYPE string VALUE 'IS_ECATT_ACTIVE',
    is_ecatt_recording         TYPE string VALUE 'IS_ECATT_RECORDING',
    is_ecatt_playback          TYPE string VALUE 'IS_ECATT_PLAYBACK',
    " Status
    is_upgrade_running         TYPE string VALUE 'IS_UPGRADE_RUNNING'.

  CONSTANTS:
    BEGIN OF c_client_role,
      production  TYPE string VALUE 'PRODUCTION',
      test        TYPE string VALUE 'TEST',
      demo        TYPE string VALUE 'DEMO',
      training    TYPE string VALUE 'TRAINING',
      sap         TYPE string VALUE 'SAP',
      customizing TYPE string VALUE 'CUSTOMIZING',
      unknown     TYPE string VALUE '<unknown>',
    END OF c_client_role.

  CONSTANTS:
    BEGIN OF c_system,
      sap      TYPE string VALUE 'SAP',
      customer TYPE string VALUE 'CUSTOMER',
    END OF c_system.

  CONSTANTS:
    c_undefined      TYPE string VALUE '<undefined>',
    c_unknown        TYPE string VALUE '<unknown>',
    c_todo           TYPE string VALUE '<todo>',
    c_system_defined TYPE string VALUE '<system>', " read-only
    c_user_defined   TYPE string VALUE '<user>'. " read/write

  TYPES:
    BEGIN OF ty_env,
      name  TYPE string,
      value TYPE string,
      type  TYPE string,
    END OF ty_env,
    ty_environment TYPE HASHED TABLE OF ty_env WITH UNIQUE KEY name.

  METHODS get
    IMPORTING
      !name         TYPE string
    RETURNING
      VALUE(result) TYPE string.

  METHODS get_all
    RETURNING
      VALUE(result) TYPE string_table.

  METHODS set
    IMPORTING
      !name         TYPE string
      !value        TYPE string OPTIONAL
      !type         TYPE string DEFAULT c_user_defined
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_env
    RAISING
      /apmg/cx_apm_error.

  METHODS delete
    IMPORTING
      !name         TYPE string
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_env
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
