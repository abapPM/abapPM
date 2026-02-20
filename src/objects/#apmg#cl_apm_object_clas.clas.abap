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

    DATA original_class_name TYPE seoclsname.
    DATA original_class_key TYPE seoclskey.

    METHODS source
      IMPORTING
        class_name    TYPE seoclsname
      RETURNING
        VALUE(result) TYPE seop_source_string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_object_clas IMPLEMENTATION.


  METHOD /apmg/if_apm_object~import.

    DATA(is_pretty) = xsdbool( is_dry_run = abap_false ).

    TRY.
        " TODO: Make files mandatory
        IF files IS INITIAL.
          " Copy globally installed class
          DATA(import_metadata)   = zif_abapgit_oo_object_fnc~get_class_properties( original_class_key ).
          DATA(import_attributes) = zif_abapgit_oo_object_fnc~read_attributes( original_class_name ).
          DATA(import_text_pool) = zif_abapgit_oo_object_fnc~read_text_pool(
            iv_class_name = original_class_name
            iv_language   = sy-langu ).

          IF import_metadata IS INITIAL.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Not found'.
          ENDIF.

          DATA(class_code) = source( original_class_name ).
        ELSE.
          " Import class from registry
          DATA(xml) = files->get_xml_parsed( ).

          xml->read(
            EXPORTING
              iv_name = 'VSEOCLASS'
            CHANGING
              cg_data = import_metadata ).
          xml->read(
            EXPORTING
              iv_name = 'ATTRIBUTES'
            CHANGING
              cg_data = import_attributes ).
          xml->read(
            EXPORTING
              iv_name = 'TPOOL'
            CHANGING
              cg_data = import_text_pool ).

          class_code = files->get_abap( ).

          DATA(local_definitions)     = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-locals_def ).
          DATA(local_implementations) = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-locals_imp ).
          DATA(local_macros)          = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-macros ).
          DATA(test_classes)          = files->get_abap( zif_abapgit_oo_object_fnc=>c_parts-testclasses ).
        ENDIF.

        " Rename and create new class
        DATA(import_key)        = original_class_key.
        import_key-clsname      = new_object.
        import_metadata-clsname = new_object.

        " No test classes in production
        IF is_production = abap_true.
          CLEAR import_metadata-with_unit_tests.
        ENDIF.

        " Get existing and to be imported code
        IF zif_abapgit_oo_object_fnc~exists( import_key-clsname ).
          DATA(existing_class_code) = source( import_key-clsname ).
        ENDIF.

        DATA(import_class_code) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_classpool_name( original_class_name )
          program_source = class_code
          map            = map
          is_pretty      = is_pretty ).

        " Compare existing code to newly imported code to avoid slow REPOSRC updates
        IF is_dry_run IS INITIAL AND import_class_code <> existing_class_code.
          zif_abapgit_oo_object_fnc~create(
            EXPORTING
              iv_check      = abap_false
              iv_package    = new_package
              it_attributes = import_attributes
            CHANGING
              cg_properties = import_metadata ).

          zif_abapgit_oo_object_fnc~deserialize_source(
            iv_package = new_package
            iv_version = import_metadata-unicode
            is_key     = import_key
            it_source  = import_class_code ).
        ENDIF.

        DATA(import_local_definitions) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_ccdef_name( original_class_name )
          program_source = local_definitions
          map            = map
          is_pretty      = is_pretty ).

        DATA(import_local_implementations) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_ccimp_name( original_class_name )
          program_source = local_implementations
          map            = map
          is_pretty      = is_pretty ).

        DATA(import_local_macros) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_ccmac_name( original_class_name )
          program_source = local_macros
          map            = map
          is_pretty      = is_pretty ).

        " No test classes in production
        IF is_production = abap_false.
          DATA(import_test_classes) = /apmg/cl_apm_code_importer=>import(
            program_name   = cl_oo_classname_service=>get_ccau_name( original_class_name )
            program_source = test_classes
            map            = map
            is_pretty      = is_pretty ).
        ELSE.
          CLEAR import_test_classes.
        ENDIF.

        IF is_dry_run IS INITIAL.
          zif_abapgit_oo_object_fnc~generate_locals(
            iv_package               = new_package
            iv_version               = import_metadata-unicode
            is_key                   = import_key
            it_local_definitions     = import_local_definitions
            it_local_implementations = import_local_implementations
            it_local_macros          = import_local_macros
            it_local_test_classes    = import_test_classes ).

          zif_abapgit_oo_object_fnc~insert_text_pool(
            iv_class_name = import_key-clsname
            it_text_pool  = import_text_pool
            iv_language   = sy-langu ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    original_class_name = item-obj_name.
    original_class_key  = VALUE seoclskey( clsname = original_class_name ).

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
