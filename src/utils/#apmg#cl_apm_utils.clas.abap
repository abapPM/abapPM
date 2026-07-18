CLASS /apmg/cl_apm_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    "! Remove trailing slash
    CLASS-METHODS remove_trailing_slash
      IMPORTING
        val           TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    "! Return semantic version of SAP Basis
    CLASS-METHODS get_abap_version
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    "! Return semantic version of SAP Kernel
    CLASS-METHODS get_kernel_version
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    "! Return database platform
    CLASS-METHODS get_database_platform
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    "! Return semantic version of database
    CLASS-METHODS get_database_version
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    "! Return user agent for registry interactions
    CLASS-METHODS get_user_agent
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA env TYPE REF TO /apmg/if_apm_env.

ENDCLASS.



CLASS /apmg/cl_apm_utils IMPLEMENTATION.


  METHOD class_constructor.
    env = /apmg/cl_apm_env=>create( ).
  ENDMETHOD.


  METHOD get_abap_version.

    TRY.
        result = NEW /apmg/cl_apm_semver_sap( )->sap_component_to_semver( 'SAP_BASIS' ).
      CATCH cx_abap_invalid_value INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_database_platform.

    DATA(db) = env->get( /apmg/if_apm_env=>database_platform ).

    CASE db.
      WHEN 'DB2'.
        result = /apmg/if_apm_types=>c_db-db2.
      WHEN 'DB4'.
        result = /apmg/if_apm_types=>c_db-db400.
      WHEN 'DB6'.
        result = /apmg/if_apm_types=>c_db-db6.
      WHEN 'HDB'.
        result = /apmg/if_apm_types=>c_db-hdb.
      WHEN 'INF'.
        result = /apmg/if_apm_types=>c_db-informix.
      WHEN 'MSS'.
        result = /apmg/if_apm_types=>c_db-mssql.
      WHEN 'ORA'.
        result = /apmg/if_apm_types=>c_db-oracle.
      WHEN 'SDB'.
        result = /apmg/if_apm_types=>c_db-sap_db.
      WHEN 'SYB'.
        result = /apmg/if_apm_types=>c_db-sybase.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Unknown DB platform'.
    ENDCASE.

  ENDMETHOD.


  METHOD get_database_version.

    DATA(db) = env->get( /apmg/if_apm_env=>database ).

    result = /apmg/cl_apm_semver_functions=>coerce(
      version = db
      loose   = abap_true )->to_string( ).

  ENDMETHOD.


  METHOD get_kernel_version.

    DATA(kernel) = env->get( /apmg/if_apm_env=>kernel_release ).

    TRY.
        result = NEW /apmg/cl_apm_semver_sap( )->sap_release_to_semver( |{ kernel }| ).
      CATCH cx_abap_invalid_value INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_user_agent.

    DATA(os) = env->get( /apmg/if_apm_env=>kernel_platform ) && '@' && get_kernel_version( ).
    DATA(db) = env->get( /apmg/if_apm_env=>database_platform ) && '@' && get_database_version( ).

    result = to_lower( |apm/{ /apmg/if_apm_version=>c_version } abap/{ get_abap_version( ) } os/{ os } db/{ db }| ).

  ENDMETHOD.


  METHOD remove_trailing_slash.

    result = replace(
      val   = val
      regex = '(.*)/$'
      with  = '$1' ) ##REGEX_POSIX.

  ENDMETHOD.
ENDCLASS.
