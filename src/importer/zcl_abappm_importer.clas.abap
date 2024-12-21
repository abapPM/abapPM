CLASS zcl_abappm_importer DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package       TYPE devclass
        !object_types  TYPE zif_abappm_code_importer=>ty_object_types OPTIONAL
        !object_names  TYPE zif_abappm_code_importer=>ty_object_names OPTIONAL
        !default_rule  TYPE string DEFAULT zif_abappm_code_importer=>c_default_import_rule
        !is_logging    TYPE abap_bool DEFAULT abap_true
        !is_dryrun     TYPE abap_bool DEFAULT abap_true
        !is_production TYPE abap_bool DEFAULT abap_true
        !from_registry TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    " TODO: replace with logger
    CONSTANTS c_width TYPE i VALUE 150.

    CLASS-METHODS get_programs
      IMPORTING
        !package      TYPE devclass
        !is_logging   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE zif_abappm_code_importer=>ty_programs
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_rules
      IMPORTING
        !programs     TYPE zif_abappm_code_importer=>ty_programs
        !default_rule TYPE string
        !is_logging   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE zcl_abappm_code_import_rules=>ty_rules
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_map
      IMPORTING
        !rules        TYPE zcl_abappm_code_import_rules=>ty_rules
        !object_types TYPE zif_abappm_code_importer=>ty_object_types
        !object_names TYPE zif_abappm_code_importer=>ty_object_names
      RETURNING
        VALUE(result) TYPE zif_abappm_code_importer=>ty_map
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_metadata
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE zabappm.

    CLASS-METHODS import_objects
      IMPORTING
        !map           TYPE zif_abappm_code_importer=>ty_map
        !is_logging    TYPE abap_bool
        !is_dryrun     TYPE abap_bool DEFAULT abap_true
        !is_production TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_importer IMPLEMENTATION.


  METHOD get_map.

  ENDMETHOD.


  METHOD get_metadata.

    " For now, check if package is installed with apm
    DATA(key)  = 'PACKAGE:%:PACKAGE_JSON'.
    DATA(name_regex) = '"name":\s*"' && name && '"'.

    SELECT * FROM zabappm INTO result WHERE keys LIKE key.
      FIND REGEX name_regex IN result-value IGNORING CASE.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.
    ENDSELECT.

    " not found
    CLEAR result.

  ENDMETHOD.


  METHOD get_programs.

    DATA programs TYPE zif_abappm_code_importer=>ty_programs.

    DATA(packages) = zcl_abapgit_factory=>get_sap_package( package )->list_subpackages( ).

    LOOP AT packages INTO DATA(sub_package) WHERE table_line <> package.

      IF is_logging = abap_true.
        WRITE: / 'PACKAGE', sub_package.
      ENDIF.

      " Find INCLUDEs containing IMPORT statements
      " FUTURE: includes are not available in BTP so this could be the source of an interface
      CLEAR programs.
      SELECT a~obj_name AS program a~devclass AS package
        INTO CORRESPONDING FIELDS OF TABLE programs
        FROM tadir AS a
        JOIN trdir AS b
        ON a~obj_name = b~name
        WHERE a~pgmid = 'R3TR' AND a~object = 'PROG' AND a~devclass = sub_package AND b~subc = 'I'.

      LOOP AT programs ASSIGNING FIELD-SYMBOL(<program>).
        IF is_logging = abap_true.
          WRITE: AT /5 'INCLUDE', <program>-program.
        ENDIF.

        DATA(source_code) = zcl_abappm_code_importer=>read( <program>-program ).

        DATA(found) = abap_false.
        LOOP AT source_code ASSIGNING FIELD-SYMBOL(<code>).
          " TODO?: Check for multi-line statements
          FIND REGEX 'IMPORT\s+.*\s+TO\s+.*\s+FROM\s+''.*''\s*\.' IN <code> IGNORING CASE.
          IF sy-subrc = 0.
            IF is_logging = abap_true.
              WRITE: AT /10 <code> COLOR COL_POSITIVE.
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

***      item-obj_type = mapping-object_type.
***      item-obj_name = mapping-old_object.
***      new_package   = mapping-target_package.
***      new_object    = map[ old_object = mapping-old_object ]-new_object.
*
*      ASSERT new_object IS NOT INITIAL AND new_object <> mapping-old_object.

      " TODO: make this dynamic like in abapGit
      " TODO: pass FILES
***      CASE mapping-object_type.
***        WHEN 'CLAS'.
***          CREATE OBJECT class_handler
***            EXPORTING
***              item = item.
***
***          class_handler->zif_abappm_object~import(
***            new_package   = new_package
***            new_object    = new_object
***            map           = map
***            is_dryrun     = is_dryrun
***            is_production = is_production ).
***
***        WHEN 'INTF'.
***          CREATE OBJECT interface_handler
***            EXPORTING
***              item = item.
***
***          interface_handler->zif_abappm_object~import(
***            new_package   = new_package
***            new_object    = new_object
***            map           = map
***            is_dryrun     = is_dryrun
***            is_production = is_production ).
***
***        WHEN OTHERS.
***          zcx_abappm_error=>raise( |Unknow type { mapping-object_type }| ).
***      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA:
      item              TYPE zif_abappm_object=>ty_item,
      new_package       TYPE devclass,
      new_object        TYPE tadir-obj_name,
      class_handler     TYPE REF TO zcl_abappm_object_clas,
      interface_handler TYPE REF TO zcl_abappm_object_intf.

    " TODO: replace with logger
    IF is_logging = abap_true.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'Searching for IMPORTs:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
    ENDIF.

    " 1. Get all programs that contain IMPORT statements
    DATA(programs) = get_programs(
      package    = package
      is_logging = is_logging ).

    " 2. Get the import rules from the programs
    DATA(rules) = get_rules(
      programs     = programs
      default_rule = default_rule
      is_logging   = is_logging ).

    " 3. Get name/version to be installed from the rules

    " 4. Download the tarballs for name/version

    " 5. Get the mapping for the objects in  the tarballs

    " 6. Import the tarballs using the mapping

  ENDMETHOD.
ENDCLASS.
