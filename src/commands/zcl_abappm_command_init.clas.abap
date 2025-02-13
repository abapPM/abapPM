CLASS zcl_abappm_command_init DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Init Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package      TYPE devclass
        !package_json TYPE zif_abappm_types=>ty_package_json
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_command_init IMPLEMENTATION.


  METHOD run.

    " Package JSON
    DATA(package_json_service) = zcl_abappm_package_json=>factory(
      package = package
      name    = package_json-name
      version = package_json-version ).

    IF package_json_service->exists( ) = abap_true.
      zcx_abappm_error=>raise( |Package { package } is already initialized| ).
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
        val   = markdown
        sub   = '\n'
        with  = cl_abap_char_utilities=>newline
        occ   = 0 ).
    ENDIF.

    DATA(readme_service) = zcl_abappm_readme=>factory(
      package  = package
      markdown = markdown ).

    readme_service->save( ).

    MESSAGE 'Package successfully initialized' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
