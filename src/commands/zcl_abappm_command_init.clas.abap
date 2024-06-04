CLASS zcl_abappm_command_init DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_package      TYPE devclass
        !is_package_json TYPE zif_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_command_init IMPLEMENTATION.


  METHOD run.

    DATA:
      lx_error        TYPE REF TO zcx_abappm_package_json,
      li_package_json TYPE REF TO zif_abappm_package_json.

    TRY.
        li_package_json = zcl_abappm_package_json=>factory(
          iv_package = iv_package
          iv_name    = is_package_json-name
          iv_version = is_package_json-version ).

        IF li_package_json->exists( ) = abap_true.
          zcx_abappm_error=>raise( |Package { iv_package } is already initialized| ).
        ENDIF.

        li_package_json->set( is_package_json )->save( ).
      CATCH zcx_abappm_package_json INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    MESSAGE 'Package successfully initialized' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
