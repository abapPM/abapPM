CLASS zcl_abappm_injector DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS set_package_json
      IMPORTING
        !iv_package TYPE devclass
        !ii_mock    TYPE REF TO zif_abappm_package_json
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_injector IMPLEMENTATION.


  METHOD set_package_json.

    DATA ls_instance TYPE zcl_abappm_factory=>ty_instance.

    READ TABLE zcl_abappm_factory=>gt_instances TRANSPORTING  NO FIELDS WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.
      ls_instance-package  = iv_package.
      ls_instance-instance = ii_mock.
      INSERT ls_instance INTO TABLE zcl_abappm_factory=>gt_instances.
    ELSE.
      zcx_abappm_error=>raise( 'Package instance already exists' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
