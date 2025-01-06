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
        VALUE(result) TYPE zif_abappm_types=>ty_packument
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
        !registry      TYPE string
        !manifest      TYPE zif_abappm_types=>ty_manifest
        !package       TYPE devclass
        !name          TYPE string
        !version       TYPE string
        !is_production TYPE abap_bool
      RAISING
        zcx_abappm_error.

    CLASS-METHODS uninstall_package
      IMPORTING
        !package TYPE devclass
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS check_integrity
      IMPORTING
        !tarball TYPE xstring
        !dist    TYPE zif_abappm_types=>ty_dist
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_utils IMPLEMENTATION.


  METHOD check_integrity.

    DATA key TYPE xstring.
    DATA shasum TYPE string.

    " TODO: Is this correct? or convert tarball to base64 first?
    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
            if_algorithm  = 'SHA512'
            if_key        = key
            if_data       = tarball
          IMPORTING
            ef_hmacstring = shasum ).
      CATCH cx_abap_message_digest INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    IF to_lower( shasum ) <> to_lower( dist-shasum ).
      zcx_abappm_error=>raise( 'Checksum error for tarball' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_manifest_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    DATA(manifest) = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name )->manifest( version ).

    result = zcl_abappm_package_json=>convert_json_to_manifest( manifest ).

  ENDMETHOD.


  METHOD get_packument_from_registry.

    " The abbreviated manifest would be sufficient for installer
    " however we also want to get the description and readme
    result = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name )->get( ).

  ENDMETHOD.


  METHOD get_tarball_from_registry.

    result = zcl_abappm_pacote=>factory(
      registry = registry
      name     = name )->tarball( tarball ).

  ENDMETHOD.


  METHOD install_package.

    DATA(tarball) = get_tarball_from_registry(
      registry = registry
      name     = manifest-name
      tarball  = manifest-dist-tarball ).

    check_integrity(
      tarball = tarball
      dist    = manifest-dist ).

    " TODO: Currently hardcoded to local packages (no transport)
    " FUTURE: Allow other folder logic than prefix
    zcl_abappm_installer=>install(
      apm_name          = name
      apm_version       = version
      enum_zip          = zcl_abappm_installer=>c_enum_zip-registry
      name              = |{ name }|
      data              = tarball
      enum_package      = zcl_abappm_installer=>c_enum_package-local
      package           = package
      enum_transport    = zcl_abappm_installer=>c_enum_transport-prompt
      enum_folder_logic = zcl_abappm_installer=>c_enum_folder_logic-prefix
      is_production     = is_production ).

  ENDMETHOD.


  METHOD uninstall_package.

    zcl_abappm_installer=>uninstall(
      apm  = abap_true
      pack = package ).

  ENDMETHOD.
ENDCLASS.
