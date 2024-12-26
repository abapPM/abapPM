CLASS zcl_abappm_code_mapper DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    " TODO: replace logging with ABAP Logger (wait for v2 of it)

    CLASS-METHODS get
      IMPORTING
        !source_package TYPE devclass
        !target_package TYPE devclass
        !rules          TYPE zif_abappm_importer=>ty_rules
        !object_types   TYPE zif_abappm_importer=>ty_object_types
        !object_names   TYPE zif_abappm_importer=>ty_object_names
        !is_logging     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE zif_abappm_importer=>ty_map
      RAISING
        zcx_abappm_error.

  PRIVATE SECTION.

    CONSTANTS c_width TYPE i VALUE 150.

    CLASS-METHODS get_tadir_objects
      IMPORTING
        !source_package TYPE devclass
        !object_types   TYPE zif_abappm_importer=>ty_object_types
        !object_names   TYPE zif_abappm_importer=>ty_object_names
        !is_logging     TYPE abap_bool
      RETURNING
        VALUE(result)   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_code_mapper IMPLEMENTATION.


  METHOD get.

    " Get list of original objects
    DATA(tadir) = get_tadir_objects(
      source_package = source_package
      object_types   = object_types
      object_names   = object_names
      is_logging     = is_logging ).

    " Process all objects and apply rules
    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
      DATA(map) = VALUE zif_abappm_importer=>ty_map_item(
        source_package = source_package
        target_package = target_package
        object_type    = <tadir>-object
        old_object     = <tadir>-obj_name ).

      " Collect first matching rule
      DATA(found)      = abap_false.
      DATA(import_all) = abap_false.
      LOOP AT rules ASSIGNING FIELD-SYMBOL(<rule>) WHERE target_package = target_package.
        IF <rule>-old_object = zif_abappm_importer=>c_default_import_rule.
          import_all = abap_true.
        ENDIF.

        map-new_object = <tadir>-obj_name.

        IF <rule>-old_object = <tadir>-obj_name.
          " Direct mapping
          map-new_object = <rule>-new_object.
        ELSE.
          " Regex replacement
          map-new_object = replace(
            val   = <tadir>-obj_name
            regex = <rule>-old_object
            with  = <rule>-new_object ).
        ENDIF.
        IF map-new_object <> <tadir>-obj_name AND strlen( map-new_object ) <= 30.
          found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF found = abap_true.
        INSERT map INTO TABLE result.
      ELSEIF import_all = abap_true.
        " With IMPORT '*' we expect all objects to be mapped
        " Most likely, the object name became too long requiring an additional rule
        zcx_abappm_error=>raise( |No mapping rule found for { <tadir>-obj_name }| ).
      ENDIF.
    ENDLOOP.

    IF is_logging = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Mapping:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<map>).
        WRITE: / <map>-source_package, <map>-target_package, <map>-old_object, <map>-new_object.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.


  METHOD get_tadir_objects.

    " Get list of original objects
    TRY.
        result = zcl_abapgit_factory=>get_tadir( )->read( source_package ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

    " Only classes and interfaces
    " FUTURE: support other object types
    DELETE result WHERE NOT ( object = 'CLAS' OR object = 'INTF' ).

    " Filter objects (for testing)
    DELETE result WHERE NOT ( object IN object_types AND obj_name IN object_names ).

    IF is_logging = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Objects:' COLOR COL_NORMAL, AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<tadir>).
        WRITE: / <tadir>-object, <tadir>-obj_name.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
