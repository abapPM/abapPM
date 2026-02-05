CLASS /apmg/cl_apm_object_clas DEFINITION PUBLIC FINAL CREATE PUBLIC
  INHERITING FROM zcl_abapgit_oo_class.

************************************************************************
* apm CLAS Importer
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_object.

    METHODS constructor
      IMPORTING
        !item TYPE /apmg/if_apm_object=>ty_item.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA class_name TYPE seoclsname.

    METHODS source
      RETURNING
        VALUE(result) TYPE seop_source_string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_object_clas IMPLEMENTATION.


  METHOD /apmg/if_apm_object~import.

    DATA(is_pretty) = xsdbool( is_dry_run = abap_false ).

    TRY.
        DATA(class_key) = VALUE seoclskey( clsname = class_name ).

        " TODO: Make files mandatory
        IF files IS INITIAL.
          " Copy globally installed interface
          DATA(class_metadata)   = zif_abapgit_oo_object_fnc~get_class_properties( class_key ).
          DATA(class_attributes) = zif_abapgit_oo_object_fnc~read_attributes( class_name ).
          DATA(class_text_pool) = zif_abapgit_oo_object_fnc~read_text_pool(
            iv_class_name = class_name
            iv_language   = sy-langu ).

          IF class_metadata IS INITIAL.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Not found'.
          ENDIF.

          DATA(orig_class_code) = source( ).
        ELSE.
          orig_class_code = files->get_abap( ).

          DATA(xml) = files->get_xml_parsed( ).

          xml->read(
            EXPORTING
              iv_name = 'VSEOCLASS'
            CHANGING
              cg_data = class_metadata ).
          xml->read(
            EXPORTING
              iv_name = 'ATTRIBUTES'
            CHANGING
              cg_data = class_attributes ).
          xml->read(
            EXPORTING
              iv_name = 'TPOOL'
            CHANGING
              cg_data = class_text_pool ).
        ENDIF.

        " Rename and create new class
        class_key-clsname      = new_object.
        class_metadata-clsname = new_object.

        IF is_production = abap_true.
          CLEAR class_metadata-with_unit_tests.
        ENDIF.

        DATA(class_code) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_classpool_name( class_name )
          program_source = orig_class_code
          map            = map
          is_pretty      = is_pretty ).

        IF is_dry_run IS INITIAL AND class_code <> orig_class_code.
          zif_abapgit_oo_object_fnc~create(
            EXPORTING
              iv_check      = abap_false
              iv_package    = new_package
              it_attributes = class_attributes
            CHANGING
              cg_properties = class_metadata ).

          zif_abapgit_oo_object_fnc~deserialize_source(
            iv_package = new_package
            iv_version = class_metadata-unicode
            is_key     = class_key
            it_source  = class_code ).
        ENDIF.

        " TODO: Make files mandatory
        IF files IS NOT INITIAL.
          DATA(local_definitions)     = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-locals_def ).
          DATA(local_implementations) = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-locals_imp ).
          DATA(local_macros)          = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-macros ).
          DATA(test_classes)          = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-testclasses ).
        ENDIF.

        local_definitions = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_ccdef_name( class_name )
          program_source = local_definitions
          map            = map
          is_pretty      = is_pretty ).

        local_implementations = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_ccimp_name( class_name )
          program_source = local_implementations
          map            = map
          is_pretty      = is_pretty ).

        local_macros = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_ccmac_name( class_name )
          program_source = local_macros
          map            = map
          is_pretty      = is_pretty ).

        IF is_production = abap_true.
          CLEAR test_classes.
        ELSE.
          test_classes = /apmg/cl_apm_code_importer=>import(
            program_name   = cl_oo_classname_service=>get_ccau_name( class_name )
            program_source = test_classes
            map            = map
            is_pretty      = is_pretty ).
        ENDIF.

        IF is_dry_run IS INITIAL.
          zif_abapgit_oo_object_fnc~generate_locals(
            iv_package               = new_package
            iv_version               = class_metadata-unicode
            is_key                   = class_key
            it_local_definitions     = local_definitions
            it_local_implementations = local_implementations
            it_local_macros          = local_macros
            it_local_test_classes    = test_classes ).

          zif_abapgit_oo_object_fnc~insert_text_pool(
            iv_class_name = class_name
            it_text_pool  = class_text_pool
            iv_language   = sy-langu ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    class_name = item-obj_name.

  ENDMETHOD.


  METHOD source.

    TRY.
        DATA(instance) = cl_oo_factory=>create_instance( ).

        DATA(source_handler) = instance->create_clif_source(
          clif_name = class_name
          version   = 'A' ).

        source_handler->get_source( IMPORTING source = result ).
      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
