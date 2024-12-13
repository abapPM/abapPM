CLASS zcl_abappm_importer DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !package       TYPE devclass
        !object_types  TYPE zif_abappm_code_importer=>ty_object_types OPTIONAL
        !object_names  TYPE zif_abappm_code_importer=>ty_object_names OPTIONAL
        !is_logging    TYPE abap_bool DEFAULT abap_true
        !is_dryrun     TYPE abap_bool DEFAULT abap_true
        !is_production TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    " TODO: replace with logger
    CONSTANTS c_width TYPE i VALUE 150.

    CLASS-METHODS get_programs
      IMPORTING
        !package      TYPE devclass
        !is_logging   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE zif_abappm_code_importer=>ty_programs
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_map
      IMPORTING
        !programs     TYPE zif_abappm_code_importer=>ty_programs
        !object_types TYPE zif_abappm_code_importer=>ty_object_types
        !object_names TYPE zif_abappm_code_importer=>ty_object_names
      RETURNING
        VALUE(result) TYPE zif_abappm_code_importer=>ty_map
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_apm_metadata
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE zabappm.

ENDCLASS.



CLASS zcl_abappm_importer IMPLEMENTATION.


  METHOD get_apm_metadata.

    " For now, check if package is installed with apm
    DATA(apm_key)  = 'PACKAGE:%:PACKAGE_JSON'.
    DATA(apm_name) = |"name":*"{ name }"|.

    SELECT * FROM zabappm INTO result WHERE keys LIKE apm_key.
      IF result-value CP apm_name.
        RETURN.
      ENDIF.
    ENDSELECT.

    " not found
    CLEAR result.

  ENDMETHOD.


  METHOD get_map.

    LOOP AT programs ASSIGNING FIELD-SYMBOL(<program>).
      DATA(map) = zcl_abappm_code_mapper=>get(
        program      = <program>
        object_types = object_types[]
        object_names = object_names[] ).

      APPEND LINES OF map TO result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_programs.

    DATA programs TYPE zif_abappm_code_importer=>ty_programs.

    DATA(packages) = zcl_abapgit_factory=>get_sap_package( package )->list_subpackages( ).

    LOOP AT packages INTO DATA(sub_package) WHERE table_line <> package.

      IF is_logging = abap_true.
        WRITE: / 'PACKAGE', sub_package.
      ENDIF.

      " Find INCLUDEs containing IMPORT statements
      " FUTURE: includes are not available in BTP so this could be an interface source instead
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

        FIND FIRST OCCURRENCE OF REGEX |IMPORT.*TO.*FROM '(.*)'| IN TABLE source_code SUBMATCHES DATA(name).
        IF sy-subrc = 0.
          IF is_logging = abap_true.
            WRITE: AT /10 'IMPORT', name COLOR COL_POSITIVE.
          ENDIF.

          " TODO: Big change... instead of looking for the package in the global namespace
          " and copying it from there, the package (tarball) needs to be read from the
          " registry (using pacote)
          DATA(apm_metadata) = get_apm_metadata( name ).

          IF apm_metadata IS NOT INITIAL.
            SPLIT apm_metadata-keys AT ':' INTO DATA(rest1) DATA(source_package) DATA(rest2).
            ASSERT sy-subrc = 0.

            IF is_logging = abap_true.
              WRITE: AT /15 'FOUND IN', source_package COLOR COL_POSITIVE.
            ENDIF.

            <program>-source_package = source_package.
            INSERT <program> INTO TABLE result.
          ELSE.
            IF is_logging = abap_true.
              WRITE: AT /15 'NOT FOUND' COLOR COL_NEGATIVE.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDLOOP. " programs
    ENDLOOP. " packages

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

    DATA(programs) = get_programs(
      package    = package
      is_logging = is_logging ).

    DATA(map) = get_map(
      programs     = programs
      object_types = object_types
      object_names = object_names ).

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

      item-obj_type = mapping-object_type.
      item-obj_name = mapping-old_object.
      new_package   = mapping-target_package.
      new_object    = map[ old_object = mapping-old_object ]-new_object.

      ASSERT new_object IS NOT INITIAL AND new_object <> mapping-old_object.

      " TODO: make this dynamic like in abapGit
      CASE mapping-object_type.
        WHEN 'CLAS'.
          CREATE OBJECT class_handler
            EXPORTING
              item = item.

          class_handler->zif_abappm_object~import(
            new_package   = new_package
            new_object    = new_object
            map           = map
            is_dryrun     = is_dryrun
            is_production = is_production ).

        WHEN 'INTF'.
          CREATE OBJECT interface_handler
            EXPORTING
              item = item.

          interface_handler->zif_abappm_object~import(
            new_package   = new_package
            new_object    = new_object
            map           = map
            is_dryrun     = is_dryrun
            is_production = is_production ).

        WHEN OTHERS.
          zcx_abappm_error=>raise( |Unknow type { mapping-object_type }| ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
