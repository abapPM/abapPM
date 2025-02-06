CLASS zcl_abappm_popup_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create_package
      IMPORTING
        !package      TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_popup_utils IMPLEMENTATION.


  METHOD create_package.

    DATA:
      package_data TYPE zif_abapgit_sap_package=>ty_create,
      create_it    TYPE abap_bool.

    package_data-devclass = condense( to_upper( package ) ).

    IF package IS NOT INITIAL AND zcl_abapgit_factory=>get_sap_package( package_data-devclass )->exists( ) = abap_true.
      zcx_abappm_error=>raise( |Package { package_data-devclass } already exists| ).
    ENDIF.

    zcl_abappm_gui_factory=>get_popups( )->popup_to_create_package(
      EXPORTING
        is_package_data = package_data
      IMPORTING
        es_package_data = package_data
        ev_create       = create_it ).

    IF create_it = abap_true.
      TRY.
          zcl_abappm_factory=>get_sap_package( package_data-devclass )->create( package_data ).
          result = package_data-devclass.
          COMMIT WORK AND WAIT.
        CATCH zcx_abapgit_exception INTO DATA(error).
          zcx_abappm_error=>raise_with_text( error ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
