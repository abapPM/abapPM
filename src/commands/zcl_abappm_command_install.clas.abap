CLASS zcl_abappm_command_install DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_registry     TYPE string
        !iv_package      TYPE devclass
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
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

    CLASS-METHODS get_packument_from_registry
      IMPORTING
        !iv_registry     TYPE string
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RETURNING
        VALUE(result)    TYPE zif_abappm_pacote=>ty_packument
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

ENDCLASS.



CLASS zcl_abappm_command_install IMPLEMENTATION.


  METHOD check_package.

    DATA:
      lv_name         TYPE string,
      li_package_json TYPE REF TO zif_abappm_package_json,
      lx_error        TYPE REF TO zcx_abappm_package_json.

    TRY.
        li_package_json = zcl_abappm_package_json=>factory( iv_package ).

        IF li_package_json->exists( ) = abap_true.
          lv_name = li_package_json->get( )-name.
          IF lv_name <> iv_name.
            zcx_abappm_error=>raise( |{ iv_package } already contains a package { lv_name }| ).
          ENDIF.
        ENDIF.
      CATCH zcx_abappm_package_json INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_manifest_from_registry.

    DATA:
      lx_pacote_error TYPE REF TO zcx_abappm_pacote,
      lx_ajson_error  TYPE REF TO zcx_abappm_ajson_error,
      lv_manifest     TYPE string.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    TRY.
        lv_manifest = zcl_abappm_pacote=>factory(
          iv_registry = iv_registry
          iv_name     = is_package_json-name )->manifest( is_package_json-version ).
      CATCH zcx_abappm_pacote INTO lx_pacote_error.
        zcx_abappm_error=>raise_with_text( lx_pacote_error ).
    ENDTRY.

    TRY.
        zcl_abappm_ajson=>parse( lv_manifest )->to_abap_corresponding_only( )->to_abap( IMPORTING ev_container = result ).
      CATCH zcx_abappm_ajson_error INTO lx_ajson_error.
        zcx_abappm_error=>raise_with_text( lx_ajson_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_packument_from_registry.

    DATA:
      lx_pacote_error TYPE REF TO zcx_abappm_pacote,
      lx_ajson_error  TYPE REF TO zcx_abappm_ajson_error,
      lv_packument    TYPE string.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    TRY.
        lv_packument = zcl_abappm_pacote=>factory(
          iv_registry = iv_registry
          iv_name     = is_package_json-name )->packument( ).
      CATCH zcx_abappm_pacote INTO lx_pacote_error.
        zcx_abappm_error=>raise_with_text( lx_pacote_error ).
    ENDTRY.

    TRY.
        zcl_abappm_ajson=>parse( lv_packument )->to_abap_corresponding_only( )->to_abap( IMPORTING ev_container = result ).
      CATCH zcx_abappm_ajson_error INTO lx_ajson_error.
        zcx_abappm_error=>raise_with_text( lx_ajson_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tarball_from_registry.

    DATA lx_error TYPE REF TO zcx_abappm_pacote.

    TRY.
        result = zcl_abappm_pacote=>factory(
          iv_registry = iv_registry
          iv_name     = is_manifest-name )->tarball( is_manifest-dist-tarball ).
      CATCH zcx_abappm_pacote INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD install_package.

    DATA lx_error TYPE REF TO zcx_abapinst_exception.

    " TODO: The installed needs to be moved to the ZCL_ABAPPM_ namespace
    " TODO: Currently hardcoded to $-packages and prefix folder logic
    TRY.
        zcl_abapinst_installer=>install(
          iv_apm_name          = is_package_json-name
          iv_apm_version       = is_package_json-version
          iv_enum_zip          = zcl_abapinst_installer=>c_enum_zip-registry
          iv_name              = |{ is_package_json-name }|
          iv_data              = iv_tarball
          iv_enum_package      = zcl_abapinst_installer=>c_enum_package-local
          iv_package           = iv_package
          iv_enum_transport    = zcl_abapinst_installer=>c_enum_transport-prompt
          iv_enum_folder_logic = zcl_abapinst_installer=>c_enum_folder_logic-prefix ).
      CATCH zcx_abapinst_exception INTO lx_error.
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD run.

    DATA:
      ls_manifest     TYPE zif_abappm_package_json_types=>ty_manifest,
      ls_package_json TYPE zif_abappm_package_json_types=>ty_package_json,
      ls_packument    TYPE zif_abappm_pacote=>ty_packument,
      lv_tarball      TYPE xstring.

    " 1. Check if something else is already installed
    check_package(
      iv_package = iv_package
      iv_name    = is_package_json-name ).

    " 2. Get manifest
    ls_manifest = get_manifest_from_registry(
      iv_registry     = iv_registry
      is_package_json = is_package_json ).

    " 2b. Registry currently returns readme only on packument level (not in manifest)
    ls_packument = get_packument_from_registry(
      iv_registry     = iv_registry
      is_package_json = is_package_json ).
    ls_manifest-readme = ls_packument-readme.

    " 3. Get tarball
    lv_tarball = get_tarball_from_registry(
      iv_registry = iv_registry
      is_manifest = ls_manifest ).

    " 4. Pass tarball to installer
    install_package(
      iv_package      = iv_package
      is_package_json = is_package_json
      iv_tarball      = lv_tarball ).

    " 5. Save package.abap.json and readme
    ls_package_json = is_package_json.
    MOVE-CORRESPONDING ls_manifest TO ls_package_json.

    zcl_abappm_command_init=>run(
      iv_package      = iv_package
      is_package_json = ls_package_json ).

    MESSAGE 'Package successfully installed' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
