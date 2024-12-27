CLASS zcl_abappm_importer DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    " TODO: replace logging with ABAP Logger (wait for v2 of it)

    CLASS-METHODS run
      IMPORTING
        !package       TYPE devclass
        !dependencies  TYPE zif_abappm_importer=>ty_dependencies OPTIONAL
        !object_types  TYPE zif_abappm_importer=>ty_object_types OPTIONAL
        !object_names  TYPE zif_abappm_importer=>ty_object_names OPTIONAL
        !default_rule  TYPE string DEFAULT zif_abappm_importer=>c_default_import_rule
        !is_dryrun     TYPE abap_bool DEFAULT abap_true
        !is_production TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_width TYPE i VALUE 150.

    CLASS-DATA is_logging TYPE abap_bool VALUE 'X'.

    CLASS-METHODS get_programs
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE zif_abappm_importer=>ty_programs
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_rules
      IMPORTING
        !programs     TYPE zif_abappm_importer=>ty_programs
        !default_rule TYPE string
      RETURNING
        VALUE(result) TYPE zif_abappm_importer=>ty_rules
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_packages
      IMPORTING
        !rules        TYPE zif_abappm_importer=>ty_rules
        !dependencies TYPE zif_abappm_importer=>ty_dependencies
      RETURNING
        VALUE(result) TYPE zif_abappm_importer=>ty_packages
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_map
      IMPORTING
        !rules        TYPE zif_abappm_importer=>ty_rules
        !packages     TYPE zif_abappm_importer=>ty_packages
        !object_types TYPE zif_abappm_importer=>ty_object_types
        !object_names TYPE zif_abappm_importer=>ty_object_names
      RETURNING
        VALUE(result) TYPE zif_abappm_importer=>ty_map
      RAISING
        zcx_abappm_error.

    CLASS-METHODS import_objects
      IMPORTING
        !map           TYPE zif_abappm_importer=>ty_map
        !is_dryrun     TYPE abap_bool DEFAULT abap_true
        !is_production TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_importer IMPLEMENTATION.


  METHOD get_map.

    IF is_logging = abap_true.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'Object Mapping:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
    ENDIF.

    LOOP AT packages ASSIGNING FIELD-SYMBOL(<package>).
      DATA(map) = zcl_abappm_code_mapper=>get(
        source_package = <package>-source_package
        target_package = <package>-target_package
        rules          = rules
        object_types   = object_types
        object_names   = object_names
        is_logging     = is_logging ).

      INSERT LINES OF map INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_packages.

    DATA(list) = zcl_abappm_package_json=>list( instanciate = abap_true ).

    IF is_logging = abap_true.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'Packages:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
    ENDIF.

    LOOP AT rules ASSIGNING FIELD-SYMBOL(<rule>).
      IF dependencies IS NOT INITIAL.
        READ TABLE dependencies ASSIGNING FIELD-SYMBOL(<dependency>)
          WITH KEY name = <rule>-name.
        IF sy-subrc <> 0.
          zcx_abappm_error=>raise( |Package { <rule>-name } used in IMPORT statement but not a dependency| ).
        ENDIF.
      ENDIF.

      " TODO!: Instead of using the globally installed package as a source,
      " this needs to be retrieved from the registry
      READ TABLE list ASSIGNING FIELD-SYMBOL(<package_json>)
        WITH KEY name COMPONENTS name = <rule>-name.
      IF sy-subrc = 0.
        DATA(package) = VALUE zif_abappm_importer=>ty_package(
          name           = <rule>-name
          version        = <rule>-version
          source_package = <package_json>-package
          target_package = <rule>-target_package ).

        CASE <rule>-version.
          WHEN 'latest'.
            IF dependencies IS NOT INITIAL.
              package-version = <dependency>-version.
            ELSE.
              package-version = <package_json>-version.
            ENDIF.
          WHEN <package_json>-version.
            " TODO: better error
            zcx_abappm_error=>raise( 'Version mismatch' ).
        ENDCASE.

        INSERT package INTO TABLE result.

        IF is_logging = abap_true.
          FORMAT COLOR COL_POSITIVE.
          WRITE: AT /5 package-name, AT 30 package-version,
            AT 40 package-source_package, AT 72 package-target_package, AT c_width space.
        ENDIF.

      ELSE.
        IF is_logging = abap_true.
          FORMAT COLOR COL_NEGATIVE.
          WRITE: AT /5 <rule>-name , 'not found in global namespace', AT c_width ''.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF is_logging = abap_true.
      FORMAT COLOR OFF.
      SKIP.
    ENDIF.

    SORT result.
    DELETE ADJACENT DUPLICATES FROM result.

  ENDMETHOD.


  METHOD get_programs.

    DATA programs TYPE zif_abappm_importer=>ty_programs.

    DATA(packages) = zcl_abapgit_factory=>get_sap_package( package )->list_subpackages( ).

    IF is_logging = abap_true.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'Searching for IMPORTs:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
    ENDIF.

    LOOP AT packages INTO DATA(sub_package) WHERE table_line <> package.

      " Find INCLUDEs containing IMPORT statements
      " FUTURE: includes are not available in BTP so this could be the source of an interface
      CLEAR programs.
      SELECT a~obj_name AS program, a~devclass AS package
        INTO CORRESPONDING FIELDS OF TABLE @programs
        FROM tadir AS a
        JOIN trdir AS b
        ON a~obj_name = b~name
        WHERE a~pgmid = 'R3TR' AND a~object = 'PROG' AND a~devclass = @sub_package AND b~subc = 'I' ##SUBRC_OK.

      IF is_logging = abap_true AND programs IS NOT INITIAL.
        WRITE: / 'PACKAGE', sub_package, AT c_width space.
      ENDIF.

      LOOP AT programs ASSIGNING FIELD-SYMBOL(<program>).
        IF is_logging = abap_true.
          WRITE: AT /5 'INCLUDE', <program>-program, AT c_width space.
        ENDIF.

        DATA(source_code) = zcl_abappm_code_importer=>read( <program>-program ).

        DATA(found) = abap_false.
        LOOP AT source_code ASSIGNING FIELD-SYMBOL(<code>).
          " TODO?: Check for multi-line statements
          FIND REGEX 'IMPORT\s+.*\s+TO\s+.*\s+FROM\s+''.*''\s*\.' IN <code> IGNORING CASE.
          IF sy-subrc = 0.
            IF is_logging = abap_true.
              FORMAT COLOR COL_POSITIVE.
              WRITE: AT /10 <code>, AT c_width space.
              FORMAT COLOR OFF.
            ENDIF.
            found = abap_true.
          ENDIF.
        ENDLOOP.

        IF found = abap_true.
          INSERT <program> INTO TABLE result.
        ENDIF.
      ENDLOOP. " programs
    ENDLOOP. " packages

    IF result IS INITIAL AND is_logging = abap_true.
      WRITE AT /5 'No includes with IMPORT statements found' COLOR COL_TOTAL.
    ELSE.
      SKIP.
    ENDIF.

    SORT result.

  ENDMETHOD.


  METHOD get_rules.

    result = zcl_abappm_code_import_rules=>get(
      programs     = programs
      is_logging   = is_logging
      default_rule = default_rule ).

  ENDMETHOD.


  METHOD import_objects.

    IF is_logging = abap_true.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'Importing:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
    ENDIF.

    LOOP AT map INTO DATA(mapping).

      IF is_logging = abap_true.
        WRITE: /
          'IMPORT', mapping-object_type, mapping-old_object,
          'TO', mapping-new_object,
          'IN PACKAGE', mapping-target_package.
      ENDIF.

      DATA(item) = VALUE zif_abappm_object=>ty_item(
        obj_type = mapping-object_type
        obj_name = mapping-old_object ).

      DATA(new_package) = mapping-target_package.
      DATA(new_object)  = map[ old_object = mapping-old_object ]-new_object.

      ASSERT new_object IS NOT INITIAL AND new_object <> mapping-old_object.

      " TODO: make this dynamic like in abapGit
      " TODO: pass FILES
      CASE mapping-object_type.
        WHEN 'CLAS'.
          DATA(class_handler) = NEW zcl_abappm_object_clas( item ).

          class_handler->zif_abappm_object~import(
            new_package   = new_package
            new_object    = new_object
            map           = map
            is_dryrun     = is_dryrun
            is_production = is_production ).

        WHEN 'INTF'.
          DATA(interface_handler) = NEW zcl_abappm_object_intf( item ).

          interface_handler->zif_abappm_object~import(
            new_package   = new_package
            new_object    = new_object
            map           = map
            is_dryrun     = is_dryrun
            is_production = is_production ).

        WHEN OTHERS.
          zcx_abappm_error=>raise( |Unsupported object type { mapping-object_type }| ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    " 1. Get all programs that contain IMPORT statements
    DATA(programs) = get_programs( package ).

    " 2. Get the import rules from the programs
    DATA(rules) = get_rules(
      programs     = programs
      default_rule = default_rule ).

    " 3. Get name/version from the rules
    DATA(packages) = get_packages(
      rules        = rules
      dependencies = dependencies ).

    " TODO: 4. Download the tarballs for all packages

    " 5. Get the object mapping
    DATA(map) = get_map(
      rules        = rules
      packages     = packages
      object_types = object_types
      object_names = object_names ).

    " 6. Import the tarballs using the mapping
    import_objects(
      map           = map
      is_dryrun     = is_dryrun
      is_production = is_production ).

  ENDMETHOD.
ENDCLASS.
