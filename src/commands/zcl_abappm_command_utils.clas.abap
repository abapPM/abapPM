CLASS zcl_abappm_command_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS get_manifest_from_registry
      IMPORTING
        !registry     TYPE string
        !package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result) TYPE zif_abappm_package_json_types=>ty_manifest
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tarball_from_registry
      IMPORTING
        !registry     TYPE string
        !manifest     TYPE zif_abappm_package_json_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_command_utils IMPLEMENTATION.


  METHOD get_manifest_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(manifest) = zcl_abappm_pacote=>factory(
      iv_registry = registry
      iv_name     = package_json-name )->manifest( package_json-version ).

    TRY.
        zcl_abappm_ajson=>parse( manifest )->to_abap_corresponding_only( )->to_abap( IMPORTING ev_container = result ).
      CATCH zcx_abappm_ajson_error INTO DATA(ajson_error).
        zcx_abappm_error=>raise_with_text( ajson_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tarball_from_registry.

    result = zcl_abappm_pacote=>factory(
      iv_registry = registry
      iv_name     = manifest-name )->tarball( manifest-dist-tarball ).

  ENDMETHOD.
ENDCLASS.
