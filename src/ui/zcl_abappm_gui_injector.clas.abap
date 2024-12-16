CLASS zcl_abappm_gui_injector DEFINITION
  PUBLIC
  CREATE PRIVATE
  FOR TESTING.

************************************************************************
* apm GUI Injector
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS set_gui_services
      IMPORTING
        !gui_service TYPE REF TO zif_abapgit_gui_services.

    CLASS-METHODS get_dummy_gui_services
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_services.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_gui_injector IMPLEMENTATION.


  METHOD get_dummy_gui_services.
    result = lcl_gui_services_dummy=>create( ).
  ENDMETHOD.


  METHOD set_gui_services.
    zcl_abappm_gui_factory=>gui_service = gui_service.
  ENDMETHOD.
ENDCLASS.
