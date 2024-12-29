CLASS zcl_abappm_command_semver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  " FUTURE: Class become obsolete once zcx_abappm_semver_error is replaced by zcx_abappm_error
  PUBLIC SECTION.

    CLASS-METHODS gt
      IMPORTING
        !a            TYPE string
        !b            TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS satisfies
      IMPORTING
        !version      TYPE string
        !range        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS max_satisfying
      IMPORTING
        !versions     TYPE string_table
        !range        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_command_semver IMPLEMENTATION.


  METHOD gt.

    TRY.
        result = zcl_abappm_semver_functions=>gt(
          a = a
          b = b ).
      CATCH zcx_abappm_semver_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD max_satisfying.

    TRY.
        result = zcl_abappm_semver_ranges=>max_satisfying(
         versions = versions
         range    = range ).
      CATCH zcx_abappm_semver_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD satisfies.

    TRY.
        result = zcl_abappm_semver_functions=>satisfies(
          version = version
          range   = range ).
      CATCH zcx_abappm_semver_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
