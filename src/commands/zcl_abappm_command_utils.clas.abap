CLASS zcl_abappm_command_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  " Note: This is a stateless class. Do not add any attributes!
  PUBLIC SECTION.

    CLASS-METHODS get_packument_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_pacote=>ty_packument
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_manifest_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_types=>ty_manifest
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tarball_from_registry
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !tarball      TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS install_package
      IMPORTING
        !registry     TYPE string
        !manifest     TYPE zif_abappm_types=>ty_manifest
        !package      TYPE devclass
        !name         TYPE string
        !version      TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall_package
      IMPORTING
        !package TYPE devclass
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
      iv_name     = name )->manifest( version ).

    result = zcl_abappm_package_json=>convert_json_to_manifest( manifest ).

  ENDMETHOD.


  METHOD get_packument_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(packument) = zcl_abappm_pacote=>factory(
      iv_registry = registry
      iv_name     = name )->get( ).

  ENDMETHOD.


  METHOD get_tarball_from_registry.

    result = zcl_abappm_pacote=>factory(
      iv_registry = registry
      iv_name     = name )->tarball( tarball ).

  ENDMETHOD.


  METHOD install_package.

    DATA(tarball) = get_tarball_from_registry(
      registry = registry
      name     = manifest-name
      tarball  = manifest-dist-tarball ).

    " TODO: Currently hardcoded to local packages (no transport)
    " FUTURE: Allow other folder logic than prefix
    zcl_abappm_installer=>install(
      iv_apm_name          = name
      iv_apm_version       = version
      iv_enum_zip          = zcl_abappm_installer=>c_enum_zip-registry
      iv_name              = |{ name }|
      iv_data              = tarball
      iv_enum_package      = zcl_abappm_installer=>c_enum_package-local
      iv_package           = package
      iv_enum_transport    = zcl_abappm_installer=>c_enum_transport-prompt
      iv_enum_folder_logic = zcl_abappm_installer=>c_enum_folder_logic-prefix ).

  ENDMETHOD.


  METHOD uninstall_package.

    zcl_abappm_installer=>uninstall(
      iv_apm  = abap_true
      iv_pack = package ).

  ENDMETHOD.
ENDCLASS.
