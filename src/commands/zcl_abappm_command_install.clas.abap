CLASS zcl_abappm_command_install DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry          TYPE string
        !package           TYPE devclass
        !package_json      TYPE zif_abappm_package_json_types=>ty_package_json
        !only_dependencies TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_package
      IMPORTING
        !package TYPE devclass
        !name    TYPE string
      RAISING
        zcx_abappm_error.

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

    CLASS-METHODS install_package
      IMPORTING
        !package      TYPE devclass
        !package_json TYPE zif_abappm_package_json_types=>ty_package_json
        !tarball      TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS install_dependencies
      IMPORTING
        !package      TYPE devclass
        !package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_install IMPLEMENTATION.


  METHOD check_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_true.
      DATA(existing_name) = package_json_service->get( )-name.
      IF existing_name <> name.
        zcx_abappm_error=>raise( |{ package } already contains a package { existing_name }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


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


  METHOD install_dependencies.

    " TODO: pass production flag
    zcl_abappm_importer=>run(
       package       = package
       is_logging    = abap_false
       is_dryrun     = abap_false
       is_production = abap_true ).

  ENDMETHOD.


  METHOD install_package.

    " TODO: Currently hardcoded to local packages (no transport)
    " FUTURE: Allow other folder logic than prefix
    zcl_abappm_installer=>install(
      iv_apm_name          = package_json-name
      iv_apm_version       = package_json-version
      iv_enum_zip          = zcl_abappm_installer=>c_enum_zip-registry
      iv_name              = |{ package_json-name }|
      iv_data              = tarball
      iv_enum_package      = zcl_abappm_installer=>c_enum_package-local
      iv_package           = package
      iv_enum_transport    = zcl_abappm_installer=>c_enum_transport-prompt
      iv_enum_folder_logic = zcl_abappm_installer=>c_enum_folder_logic-prefix ).

  ENDMETHOD.


  METHOD run.

    DATA:
      manifest          TYPE zif_abappm_package_json_types=>ty_manifest,
      package_json_init TYPE zif_abappm_package_json_types=>ty_package_json,
      packument         TYPE zif_abappm_pacote=>ty_packument,
      tarball           TYPE xstring.

    IF only_dependencies = abap_false.
      " 1. Check if something else is already installed
      check_package(
        package = package
        name    = package_json-name ).

      " 2. Get manifest
      manifest = get_manifest_from_registry(
        registry     = registry
        package_json = package_json ).

      " 3. Get tarball
      tarball = get_tarball_from_registry(
        registry = registry
        manifest = manifest ).

      " 4. Pass tarball to installer
      install_package(
        package      = package
        package_json = package_json
        tarball      = tarball ).
    ENDIF.

    " 5. The real magic... dependencies
    " Warning: Currently requires dependcies to be installed globally already
    " They are then copied and renamed into the target package
    install_dependencies(
      package      = package
      package_json = package_json ).

    IF only_dependencies = abap_false.
      " 6. Save package.abap.json and readme
      package_json_init = package_json.
      MOVE-CORRESPONDING manifest TO package_json_init.

      zcl_abappm_command_init=>run(
        package      = package
        package_json = package_json_init ).
    ENDIF.

    MESSAGE 'Package successfully installed' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
