CLASS zcl_abappm_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_abappm_injector.

  PUBLIC SECTION.

    CLASS-METHODS get_package_json
      IMPORTING
        !iv_package   TYPE devclass
        !iv_name      TYPE string OPTIONAL
        !iv_version   TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_package_json.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        package  TYPE devclass,
        instance TYPE REF TO zif_abappm_package_json,
      END OF ty_instance,
      ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY package.

    CLASS-DATA:
      gt_instances TYPE ty_instances.

ENDCLASS.



CLASS zcl_abappm_factory IMPLEMENTATION.


  METHOD get_package_json.

    DATA ls_instance TYPE ty_instance.

    FIELD-SYMBOLS <ls_instance> TYPE ty_instance.

    READ TABLE gt_instances ASSIGNING <ls_instance> WITH TABLE KEY package = iv_package.
    IF sy-subrc = 0.
      result = <ls_instance>-instance.
    ELSE.
      CREATE OBJECT result TYPE zcl_abappm_package_json
        EXPORTING
          package = iv_package
          name    = iv_name
          version = iv_version.

      ls_instance-package  = iv_package.
      ls_instance-instance = result.
      INSERT ls_instance INTO TABLE gt_instances.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
