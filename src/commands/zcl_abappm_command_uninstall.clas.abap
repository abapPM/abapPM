CLASS zcl_abappm_command_uninstall DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Uninstall Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_package_json
      RAISING
        zcx_abappm_error.

    CLASS-METHODS confirm_popup
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_uninstall IMPLEMENTATION.


  METHOD check_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      zcx_abappm_error=>raise( |Package { package } not found| ).
    ENDIF.

    result = package_json_service->get( ).

  ENDMETHOD.


  METHOD confirm_popup.

    DATA answer TYPE c LENGTH 1.

    DATA(question) = |This will DELETE all objects in package { package } | &&
                     |including subpackages from the system|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Uninstall'
        text_question         = question
        text_button_1         = 'Delete'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
        popup_type            = 'ICON_MESSAGE_WARNING'
*       start_column          = ms_position-start_column
*       start_row             = ms_position-start_row
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    ASSERT sy-subrc = 0.

    IF answer = '2'.
      MESSAGE 'Uninstall cancelled' TYPE 'S'.
      RETURN.
    ENDIF.

    result = abap_true.

  ENDMETHOD.


  METHOD run.

    DATA(package_json) = check_package( package ).

    IF confirm_popup( package ) = abap_false.
      RETURN.
    ENDIF.

    zcl_abappm_command_utils=>uninstall_package(
      name    = package_json-name
      version = package_json-version
      package = package ).

    MESSAGE 'Package successfully uninstalled' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
