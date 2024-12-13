CLASS zcl_abappm_command_uninstall DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

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
        !package TYPE devclass
      RAISING
        zcx_abappm_error.

    CLASS-METHODS confirm_popup
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall_package
      IMPORTING
        !package TYPE devclass
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_uninstall IMPLEMENTATION.


  METHOD check_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_false.
      zcx_abappm_error=>raise( |Package { package } does not exist| ).
    ENDIF.

  ENDMETHOD.


  METHOD confirm_popup.

    TRY.
        DATA(question) = |This will DELETE all objects in package { package } | &&
                         |including subpackages from the system|.

        DATA(answer) = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
          iv_titlebar              = 'Uninstall'
          iv_text_question         = question
          iv_text_button_1         = 'Delete'
          iv_icon_button_1         = 'ICON_DELETE'
          iv_text_button_2         = 'Cancel'
          iv_icon_button_2         = 'ICON_CANCEL'
          iv_default_button        = '2'
          iv_popup_type            = 'ICON_MESSAGE_WARNING'
          iv_display_cancel_button = abap_false ).

        IF answer = '2'.
          MESSAGE 'Uninstall cancelled' TYPE 'S'.
          result = abap_false.
          RETURN.
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    result = abap_true.

  ENDMETHOD.


  METHOD run.

    check_package( package ).

    confirm_popup( package ).

    uninstall_package( package ).

    MESSAGE 'Package successfully uninstalled' TYPE 'S'.

  ENDMETHOD.


  METHOD uninstall_package.

    zcl_abappm_installer=>uninstall(
      iv_apm  = abap_true
      iv_pack = package ).

  ENDMETHOD.
ENDCLASS.
