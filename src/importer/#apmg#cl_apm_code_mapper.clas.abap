CLASS /apmg/cl_apm_code_mapper DEFINITION PUBLIC FINAL CREATE PUBLIC.

************************************************************************
* apm Code Mapper
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* TODO: replace logging with ABAP Logger (wait for v2 of it)
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !source_package TYPE devclass
        !target_package TYPE devclass
        !rules          TYPE /apmg/if_apm_importer=>ty_rules
        !object_types   TYPE /apmg/if_apm_importer=>ty_object_types
        !object_names   TYPE /apmg/if_apm_importer=>ty_object_names
        !is_logging     TYPE abap_bool DEFAULT abap_false
        !is_production  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE /apmg/if_apm_importer=>ty_map
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_width TYPE i VALUE 150.

    CLASS-METHODS get_tadir_objects
      IMPORTING
        !source_package TYPE devclass
        !object_types   TYPE /apmg/if_apm_importer=>ty_object_types
        !object_names   TYPE /apmg/if_apm_importer=>ty_object_names
        !is_logging     TYPE abap_bool
      RETURNING
        VALUE(result)   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_code_mapper IMPLEMENTATION.


  METHOD get.

    " Get list of original objects
    DATA(tadir) = get_tadir_objects(
      source_package = source_package
      object_types   = object_types
      object_names   = object_names
      is_logging     = is_logging ).

    " Process all objects and apply rules
    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
      " No programs for production (just classes and interfaces)
      IF is_production = abap_true AND <tadir>-object = 'PROG'.
        CONTINUE.
      ENDIF.

      DATA(map) = VALUE /apmg/if_apm_importer=>ty_map_item(
        source_package = source_package
        target_package = target_package
        object_type    = <tadir>-object
        old_object     = <tadir>-obj_name ).

      " Collect first matching rule
      DATA(found)      = abap_false.
      DATA(import_all) = abap_false.
      LOOP AT rules ASSIGNING FIELD-SYMBOL(<rule>) WHERE target_package = target_package.
        IF <rule>-old_object = /apmg/if_apm_importer=>c_default_import_rule.
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

          " Remove first underscore after namespace:
          " PROG Z_TEST becomes /NAMESPACE/TEST (instead of /NAMESPACE/_TEST)
          map-new_object = replace(
            val   = map-new_object
            regex = '(/.+/)_'
            with  = '$1' ).
        ENDIF.
        IF map-new_object <> <tadir>-obj_name.
          IF strlen( map-new_object ) > 30.
            map-new_object = map-new_object(30).
            IF is_logging = abap_true.
              WRITE: / <tadir>-object, <tadir>-obj_name, map-new_object, 'object name was > 30 chars' COLOR COL_TOTAL.
            ENDIF.
          ENDIF.
          found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF found = abap_true.
        INSERT map INTO TABLE result.
      ELSEIF import_all = abap_true.
        " With IMPORT '*' we expect all objects to be mapped
        " Most likely, the object name became too long requiring an additional rule
        WRITE: / <tadir>-object, <tadir>-obj_name, 'no mapping rule found' COLOR COL_TOTAL.
        DATA(missing_rule) = abap_true.
      ENDIF.
    ENDLOOP.

    IF is_logging = abap_true AND result IS NOT INITIAL.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Mapping:', AT c_width space.
      SKIP.
      FORMAT COLOR COL_POSITIVE.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<map>).
        WRITE: / <map>-source_package, <map>-target_package, <map>-old_object, <map>-new_object, AT c_width space.
      ENDLOOP.
      FORMAT COLOR OFF.
      SKIP.
    ENDIF.

    IF missing_rule = abap_true.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'No mapping rule for some objects'.
    ENDIF.

  ENDMETHOD.


  METHOD get_tadir_objects.

    " Get list of original objects
    TRY.
        result = zcl_abapgit_factory=>get_tadir( )->read( source_package ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

    " Only classes and interfaces
    " FUTURE: support other object types
    DELETE result WHERE NOT ( object = 'CLAS' OR object = 'INTF' OR object = 'PROG' ).

    " Filter objects (for testing)
    DELETE result WHERE NOT ( object IN object_types AND obj_name IN object_names ).

    IF is_logging = abap_true AND result IS NOT INITIAL.
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
