CLASS /apmg/cl_apm_env_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PRIVATE
  GLOBAL FRIENDS /apmg/cl_apm_env_injector.

************************************************************************
* Environment Factory
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !env_type     TYPE string DEFAULT /apmg/if_apm_env=>c_env-abap
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_env
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        env_type TYPE string,
        env      TYPE REF TO /apmg/if_apm_env,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY env_type.

    CLASS-DATA envs TYPE ty_instances.

ENDCLASS.



CLASS /apmg/cl_apm_env_factory IMPLEMENTATION.


  METHOD get.

    READ TABLE envs ASSIGNING FIELD-SYMBOL(<env>) WITH TABLE KEY env_type = env_type.
    IF sy-subrc = 0.
      result = <env>-env.
    ELSE.
      result = /apmg/cl_apm_env=>create( env_type ).
      INSERT VALUE #( env_type = env_type env = result ) INTO TABLE envs.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
