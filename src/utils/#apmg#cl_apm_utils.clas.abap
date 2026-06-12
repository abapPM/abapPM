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


  METHOD get_user_agent.

    " TODO: add platform (os) and arch (db) from /apmg/cl_env
    result = |apm/{ /apmg/if_apm_version=>c_version } abap/{ get_abap_version( ) }|.

  ENDMETHOD.
ENDCLASS.
