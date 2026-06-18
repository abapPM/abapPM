CLASS /apmg/cl_apm_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

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
ENDCLASS.



CLASS /apmg/cl_apm_utils IMPLEMENTATION.


  METHOD get_abap_version.

    TRY.
        result = NEW /apmg/cl_apm_semver_sap( )->sap_component_to_semver( 'SAP_BASIS' ).
      CATCH cx_abap_invalid_value INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_database_version.

    DATA(db) = /apmg/cl_apm_env=>create( )->get( /apmg/if_apm_env=>database ).

    result = /apmg/cl_apm_semver_functions=>coerce(
      version = db
      loose   = abap_true )->to_string( ).

  ENDMETHOD.


  METHOD get_kernel_version.

    DATA(kernel) = /apmg/cl_apm_env=>create( )->get( /apmg/if_apm_env=>kernel_release ).

    TRY.
        result = NEW /apmg/cl_apm_semver_sap( )->sap_release_to_semver( |{ kernel }| ).
      CATCH cx_abap_invalid_value INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_user_agent.

    DATA(env) = /apmg/cl_apm_env=>create( ).

    DATA(os) = env->get( /apmg/if_apm_env=>kernel_platform ) && '@' && get_kernel_version( ).
    DATA(db) = env->get( /apmg/if_apm_env=>database_platform ) && '@' && get_database_version( ).

    result = to_lower( |apm/{ /apmg/if_apm_version=>c_version } abap/{ get_abap_version( ) } os/{ os } db/{ db }| ).

  ENDMETHOD.
ENDCLASS.
