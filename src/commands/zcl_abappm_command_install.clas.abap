CLASS zcl_abappm_command_install DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_registry          TYPE string
        !iv_package           TYPE devclass
        !is_package_json      TYPE zif_abappm_package_json_types=>ty_package_json
        !iv_only_dependencies TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_package
      IMPORTING
        !iv_package TYPE devclass
        !iv_name    TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_manifest_from_registry
      IMPORTING
        !iv_registry     TYPE string
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result)    TYPE zif_abappm_package_json_types=>ty_manifest
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tarball_from_registry
      IMPORTING
        !iv_registry  TYPE string
        !is_manifest  TYPE zif_abappm_package_json_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS install_package
      IMPORTING
        !iv_package      TYPE devclass
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
        !iv_tarball      TYPE xstring
      RAISING
        zcx_abappm_error.

    CLASS-METHODS install_dependencies
      IMPORTING
        !iv_package      TYPE devclass
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_install IMPLEMENTATION.


  METHOD check_package.

    DATA:
      lv_name         TYPE string,
      li_package_json TYPE REF TO zif_abappm_package_json.

    li_package_json = zcl_abappm_package_json=>factory( iv_package ).

    IF li_package_json->exists( ) = abap_true.
      lv_name = li_package_json->get( )-name.
      IF lv_name <> iv_name.
        zcx_abappm_error=>raise( |{ iv_package } already contains a package { lv_name }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_manifest_from_registry.

    DATA:
      lx_ajson_error TYPE REF TO zcx_abappm_ajson_error,
      lv_manifest    TYPE string.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    lv_manifest = zcl_abappm_pacote=>factory(
      iv_registry = iv_registry
      iv_name     = is_package_json-name )->manifest( is_package_json-version ).

    TRY.
        zcl_abappm_ajson=>parse( lv_manifest )->to_abap_corresponding_only( )->to_abap( IMPORTING ev_container = result ).
      CATCH zcx_abappm_ajson_error INTO lx_ajson_error.
        zcx_abappm_error=>raise_with_text( lx_ajson_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tarball_from_registry.
    result = zcl_abappm_pacote=>factory(
      iv_registry = iv_registry
      iv_name     = is_manifest-name )->tarball( is_manifest-dist-tarball ).
  ENDMETHOD.


  METHOD install_dependencies.

    SUBMIT zapm_import_two
      WITH p_pack = iv_package
      WITH p_log  = abap_false
      WITH p_test = abap_false
      AND RETURN.

*            WITH p_defrul ...
*            WITH p_notest ...
*            WITH p_pretty ...
*            WITH so_name ...
*            WITH so_type ...

  ENDMETHOD.


  METHOD install_package.

    " TODO: Currently hardcoded to $-packages and prefix folder logic
    zcl_abappm_installer=>install(
      iv_apm_name          = is_package_json-name
      iv_apm_version       = is_package_json-version
      iv_enum_zip          = zcl_abappm_installer=>c_enum_zip-registry
      iv_name              = |{ is_package_json-name }|
      iv_data              = iv_tarball
      iv_enum_package      = zcl_abappm_installer=>c_enum_package-local
      iv_package           = iv_package
      iv_enum_transport    = zcl_abappm_installer=>c_enum_transport-prompt
      iv_enum_folder_logic = zcl_abappm_installer=>c_enum_folder_logic-prefix ).

  ENDMETHOD.


  METHOD run.

    DATA:
      ls_manifest     TYPE zif_abappm_package_json_types=>ty_manifest,
      ls_package_json TYPE zif_abappm_package_json_types=>ty_package_json,
      ls_packument    TYPE zif_abappm_pacote=>ty_packument,
      lv_tarball      TYPE xstring.

    IF iv_only_dependencies = abap_false.
      " 1. Check if something else is already installed
      check_package(
        iv_package = iv_package
        iv_name    = is_package_json-name ).

      " 2. Get manifest
      ls_manifest = get_manifest_from_registry(
        iv_registry     = iv_registry
        is_package_json = is_package_json ).

      " 3. Get tarball
      lv_tarball = get_tarball_from_registry(
        iv_registry = iv_registry
        is_manifest = ls_manifest ).

      " 4. Pass tarball to installer
      install_package(
        iv_package      = iv_package
        is_package_json = is_package_json
        iv_tarball      = lv_tarball ).
    ENDIF.

    " 5. The real magic... dependencies
    " Warning: Currently requires dependcies to be installed globally already
    " They are then copied and renamed into the target package
    install_dependencies(
      iv_package      = iv_package
      is_package_json = is_package_json ).

    IF iv_only_dependencies = abap_false.
      " 6. Save package.abap.json and readme
      ls_package_json = is_package_json.
      MOVE-CORRESPONDING ls_manifest TO ls_package_json.

      zcl_abappm_command_init=>run(
        iv_package      = iv_package
        is_package_json = ls_package_json ).
    ENDIF.

    MESSAGE 'Package successfully installed' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
