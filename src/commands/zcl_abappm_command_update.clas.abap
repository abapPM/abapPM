CLASS zcl_abappm_command_update DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  " Note: This is a stateless class. Do not add any attributes!
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry TYPE string
        !package  TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_update IMPLEMENTATION.


  METHOD get_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_true.
      result = package_json_service->get( ).
    ELSE.
      zcx_abappm_error=>raise( |{ package } does not contain any installed package| ).
    ENDIF.

  ENDMETHOD.


  METHOD run.

    " 1. Check package is installed and get version details
    DATA(package_json) = get_package( package ).

    " 2. Get updated manifest
    DATA(manifest) = zcl_abappm_command_utils=>get_manifest_from_registry(
      registry     = registry
      package_json = package_json ).


    " TODO...
    zcl_abappm_roadmap=>planned( ).



    MESSAGE 'Package successfully updated' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
