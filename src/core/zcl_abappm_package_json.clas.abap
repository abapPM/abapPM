CLASS zcl_abappm_package_json DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abappm_factory.

  PUBLIC SECTION.

    INTERFACES zif_abappm_package_json.

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass
        !iv_name    TYPE string OPTIONAL
        !iv_version TYPE string OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mv_package TYPE devclass,
      ms_json    TYPE zif_abappm_package_json=>ty_package_json.

ENDCLASS.



CLASS zcl_abappm_package_json IMPLEMENTATION.


  METHOD constructor.
    mv_package = iv_package.
    IF iv_name IS NOT INITIAL.
      ms_json-name = iv_name.
    ENDIF.
    IF iv_version IS NOT INITIAL.
      ms_json-version = iv_version.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abappm_package_json~delete.

  ENDMETHOD.


  METHOD zif_abappm_package_json~get.

  ENDMETHOD.


  METHOD zif_abappm_package_json~get_string.

  ENDMETHOD.


  METHOD zif_abappm_package_json~load.

  ENDMETHOD.


  METHOD zif_abappm_package_json~save.

  ENDMETHOD.


  METHOD zif_abappm_package_json~set.

  ENDMETHOD.


  METHOD zif_abappm_package_json~set_string.

  ENDMETHOD.
ENDCLASS.
