CLASS /apmg/cl_apm_command_init DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Init Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package      TYPE devclass
        !package_json TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS execute
      IMPORTING
        !package      TYPE devclass
        !package_json TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_init IMPLEMENTATION.


  METHOD execute.

    " Package JSON
    DATA(package_json_service) = /apmg/cl_apm_package_json=>factory(
      package = package
      name    = package_json-name
      version = package_json-version ).

    IF package_json_service->exists( ) = abap_true.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = |Package { package } is already initialized|.
    ENDIF.

    " Remove readme which is stored separately
    DATA(package_json_wo_readme) = package_json.
    CLEAR package_json_wo_readme-readme.

    package_json_service->set( package_json_wo_readme )->save( ).

    " Readme
    IF package_json-readme IS INITIAL.
      DATA(markdown) = |# { package_json-name } - { package_json-description }|.
    ELSE.
      markdown = package_json-readme.
      markdown = replace(
        val  = markdown
        sub  = '\n'
        with = cl_abap_char_utilities=>newline
        occ  = 0 ).
    ENDIF.

    DATA(readme_service) = /apmg/cl_apm_readme=>factory(
      package  = package
      markdown = markdown ).

    readme_service->save( ).

    MESSAGE 'Package successfully initialized' TYPE 'S'.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_init( ).

    command->execute(
      package      = package
      package_json = package_json ).

  ENDMETHOD.
ENDCLASS.
