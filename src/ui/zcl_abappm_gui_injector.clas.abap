CLASS zcl_abappm_gui_injector DEFINITION
  PUBLIC
  CREATE PRIVATE
  FOR TESTING.

************************************************************************
* apm GUI Injector
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS set_gui_services
      IMPORTING
        !ii_gui_services TYPE REF TO zif_abapgit_gui_services.

    CLASS-METHODS get_dummy_gui_services
      RETURNING
        VALUE(ri_gui_services) TYPE REF TO zif_abapgit_gui_services.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_gui_injector IMPLEMENTATION.


  METHOD get_dummy_gui_services.
    ri_gui_services = lcl_gui_services_dummy=>create( ).
  ENDMETHOD.


  METHOD set_gui_services.
    zcl_abappm_gui_factory=>gi_gui_services = ii_gui_services.
  ENDMETHOD.
ENDCLASS.
