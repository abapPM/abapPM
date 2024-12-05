CLASS zcl_abappm_command_uninstall DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_package
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abappm_error.

    CLASS-METHODS confirm_popup
      IMPORTING
        !iv_package   TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall_package
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_uninstall IMPLEMENTATION.


  METHOD check_package.

    DATA li_package_json TYPE REF TO zif_abappm_package_json.

    li_package_json = zcl_abappm_package_json=>factory( iv_package ).

    IF li_package_json->exists( ) = abap_false.
      zcx_abappm_error=>raise( |Package { iv_package } does not exist| ).
    ENDIF.

  ENDMETHOD.


  METHOD confirm_popup.

    DATA:
      lv_question TYPE string,
      lv_answer   TYPE c LENGTH 1,
      lx_error    TYPE REF TO zcx_abapgit_exception.

    TRY.
        lv_question = |This will DELETE all objects in package { iv_package
          } including subpackages from the system|.

        lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
          iv_titlebar              = 'Uninstall'
          iv_text_question         = lv_question
          iv_text_button_1         = 'Delete'
          iv_icon_button_1         = 'ICON_DELETE'
          iv_text_button_2         = 'Cancel'
          iv_icon_button_2         = 'ICON_CANCEL'
          iv_default_button        = '2'
          iv_popup_type            = 'ICON_MESSAGE_WARNING'
          iv_display_cancel_button = abap_false ).

        IF lv_answer = '2'.
          MESSAGE 'Uninstall cancelled' TYPE 'S'.
          result = abap_false.
          RETURN.
        ENDIF.

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    result = abap_true.

  ENDMETHOD.


  METHOD run.

    check_package( iv_package ).

    confirm_popup( iv_package ).

    uninstall_package( iv_package ).

    MESSAGE 'Package successfully uninstalled' TYPE 'S'.

  ENDMETHOD.


  METHOD uninstall_package.

    zcl_abappm_installer=>uninstall(
      iv_apm  = abap_true
      iv_pack = iv_package ).

  ENDMETHOD.
ENDCLASS.
