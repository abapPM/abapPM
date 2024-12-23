CLASS zcl_abappm_command_install DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  " Note: This is a stateless class. Do not add any attributes!
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry      TYPE string
        !package       TYPE devclass
        !package_json  TYPE zif_abappm_types=>ty_package_json
        !is_production TYPE abap_bool DEFAULT abap_false
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

    CLASS-METHODS check_prerequisites
      IMPORTING
        !manifest TYPE zif_abappm_types=>ty_manifest
      RAISING
        zcx_abappm_error.

    CLASS-METHODS check_dependencies
      IMPORTING
        !manifest      TYPE zif_abappm_types=>ty_manifest
        !is_production TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS check_dependency
      IMPORTING
        !list        TYPE zif_abappm_package_json=>ty_packages
        !dependency  TYPE zif_abappm_types=>ty_dependency
        !category    TYPE string
        !is_optional TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

    CLASS-METHODS check_semver
      IMPORTING
        !name        TYPE string
        !version     TYPE string
        !range       TYPE string
        !category    TYPE string
        !is_optional TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_command_install IMPLEMENTATION.


  METHOD check_dependencies.

    " Get all installed packages
    DATA(list) = zcl_abappm_package_json=>list( ).

    LOOP AT manifest-dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
      READ TABLE manifest-bundle_dependencies TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = <dependency>-name.
      IF sy-subrc <> 0.
        check_dependency(
          list       = list
          dependency = <dependency>
          category   = 'Dependency' ).
      ENDIF.
    ENDLOOP.

    IF is_production = abap_false.
      LOOP AT manifest-dev_dependencies ASSIGNING <dependency>.
        check_dependency(
          list       = list
          dependency = <dependency>
          category   = 'devDependency' ).
      ENDLOOP.
    ENDIF.

    LOOP AT manifest-optional_dependencies ASSIGNING <dependency>.
      check_dependency(
        list        = list
        dependency  = <dependency>
        category    = 'optionalDependency'
        is_optional = abap_true ).
    ENDLOOP.

    LOOP AT manifest-peer_dependencies ASSIGNING <dependency>.
      check_dependency(
        list       = list
        dependency = <dependency>
        category   = 'peerDependency'
        is_optional = abap_true ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check_dependency.

    " TODO: Log the issues instead of failing
    READ TABLE list ASSIGNING FIELD-SYMBOL(<package>)
      WITH KEY name COMPONENTS name = dependency-name.
    IF sy-subrc = 0.
      TRY.
          DATA(satisfies) = zcl_abappm_semver_functions=>satisfies(
            version = <package>-version
            range   = dependency-range ).
        CATCH zcx_abappm_semver_error INTO DATA(error).
          zcx_abappm_error=>raise_with_text( error ).
      ENDTRY.

      IF satisfies = abap_false.
        IF is_optional = abap_true.
          " TODO: Log warning
        ELSE.
          zcx_abappm_error=>raise(
            |{ category } { dependency-name } is installed in version { <package>-version } | &&
            |but does not satisfy { dependency-range }| ).
        ENDIF.
      ENDIF.
    ELSE.
      IF is_optional = abap_true.
        " TODO: Log warning
      ELSE.
        zcx_abappm_error=>raise( |{ category } { dependency-name } is not installed| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_package.

    DATA(package_json_service) = zcl_abappm_package_json=>factory( package ).

    IF package_json_service->exists( ) = abap_true.
      DATA(existing_name) = package_json_service->get( )-name.
      IF existing_name <> name.
        zcx_abappm_error=>raise( |{ package } already contains a package { existing_name }| ).
      ENDIF.
    ENDIF.

    SELECT COUNT(*) FROM tadir INTO @DATA(count) WHERE devclass = @package.
    IF count > 1.
      zcx_abappm_error=>raise( |{ package } already contains { count } objects| ).
    ENDIF.

  ENDMETHOD.


  METHOD check_prerequisites.

    " apm version
    READ TABLE manifest-engines ASSIGNING FIELD-SYMBOL(<dependency>)
      WITH KEY name = 'apm'.
    IF sy-subrc = 0.
      check_semver(
        name     = 'apm'
        version  = zif_abappm_version=>c_version
        range    = <dependency>-range
        category = 'Engine' ).
    ENDIF.

    " abap release
    READ TABLE manifest-engines ASSIGNING <dependency>
      WITH KEY name = 'abap'.
    IF sy-subrc = 0.
      TRY.
          DATA(abap_version) = NEW zcl_abappm_semver_sap( )->sap_component_to_semver( 'SAP_BASIS' ).
        CATCH cx_abap_invalid_value INTO DATA(error).
          zcx_abappm_error=>raise_with_text( error ).
      ENDTRY.

      check_semver(
        name     = 'ABAP'
        version  = abap_version
        range    = <dependency>-range
        category = 'Engine' ).
    ENDIF.

    " TODO: Check os & cpu (requires "env" package which is =WIP=)

  ENDMETHOD.


  METHOD check_semver.

    DATA(satisfies) = zcl_abappm_command_semver=>satisfies(
      version = version
      range   = range ).

    IF satisfies = abap_false.
      IF is_optional = abap_true.
        " TODO: Log warning
      ELSE.
        zcx_abappm_error=>raise(
          |{ category } { name } is installed in version { version } | &&
          |but does not satisfy { range }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    DATA package_json_init TYPE zif_abappm_types=>ty_package_json.

    " 1. Check if something else is already installed
    check_package(
      package = package
      name    = package_json-name ).

    " 2. Get manifest
    DATA(manifest) = zcl_abappm_command_utils=>get_manifest_from_registry(
      registry = registry
      name     = package_json-name
      version  = package_json-version ).

    " 3. Check prerequisites (os, cpu, engines)
    check_prerequisites( manifest ).

    " TODO!: Instead of checking if dependencies are installed, it should install them.
    " For that to happen, we need arborist to build the dependency tree and pass it here.
    " This needs to include the target SAP package for each dependency :-)

    " 4. Check dependencies
    check_dependencies( manifest ).

    " 6. Get tarball from registry and install it into package
    zcl_abappm_command_utils=>install_package(
      registry     = registry
      manifest     = manifest
      package      = package
      name         = package_json-name
      version      = package_json-version ).

    " 7. Save package.abap.json and readme
    package_json_init = package_json.
    MOVE-CORRESPONDING manifest TO package_json_init.

    zcl_abappm_command_init=>run(
      package      = package
      package_json = package_json_init ).

    MESSAGE 'Package successfully installed' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
