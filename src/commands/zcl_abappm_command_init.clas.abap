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
      lv_markdown           TYPE string,
      lx_package_json_error TYPE REF TO zcx_abappm_package_json,
      li_package_json       TYPE REF TO zif_abappm_package_json,
      lx_readme_error       TYPE REF TO zcx_abappm_readme,
      li_readme             TYPE REF TO zif_abappm_readme.

    TRY.
        li_package_json = zcl_abappm_package_json=>factory(
          iv_package = iv_package
          iv_name    = is_package_json-name
          iv_version = is_package_json-version ).

        IF li_package_json->exists( ) = abap_true.
          zcx_abappm_error=>raise( |Package { iv_package } is already initialized| ).
        ENDIF.

        li_package_json->set( is_package_json )->save( ).

      CATCH zcx_abappm_package_json INTO lx_package_json_error.
        zcx_abappm_error=>raise_with_text( lx_package_json_error ).
    ENDTRY.

    TRY.
        lv_markdown = |# { is_package_json-name } - { is_package_json-description }|.

        li_readme = zcl_abappm_readme=>factory(
          iv_package  = iv_package
          iv_markdown = lv_markdown ).

        li_readme->save( ).

      CATCH zcx_abappm_readme INTO lx_readme_error.
        zcx_abappm_error=>raise_with_text( lx_readme_error ).
    ENDTRY.

    MESSAGE 'Package successfully initialized' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
