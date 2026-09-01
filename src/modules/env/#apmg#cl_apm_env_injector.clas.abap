CLASS /apmg/cl_apm_env_injector DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PRIVATE.

************************************************************************
* Environment Injector
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS set
      IMPORTING
        !env_type TYPE string DEFAULT /apmg/if_apm_env=>c_env-abap
        !env      TYPE REF TO /apmg/if_apm_env OPTIONAL
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_env_injector IMPLEMENTATION.


  METHOD set.

    IF env_type <> /apmg/if_apm_env=>c_env-abap AND
      env_type <> /apmg/if_apm_env=>c_env-os AND
      env_type <> /apmg/if_apm_env=>c_env-profile.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Unknown environment type'.
    ENDIF.

    IF env IS INITIAL.
      DELETE /apmg/cl_apm_env_factory=>envs WHERE env_type = env_type.
    ELSE.
      READ TABLE /apmg/cl_apm_env_factory=>envs ASSIGNING FIELD-SYMBOL(<env>) WITH TABLE KEY env_type = env_type.
      IF sy-subrc = 0.
        <env>-env = env.
      ELSE.
        INSERT VALUE #( env_type = env_type env = env ) INTO TABLE /apmg/cl_apm_env_factory=>envs.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
