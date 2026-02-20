CLASS /apmg/cl_apm_object_intf DEFINITION PUBLIC FINAL CREATE PUBLIC
  INHERITING FROM zcl_abapgit_oo_interface.

************************************************************************
* apm INTF Importer
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

    DATA original_interface_name TYPE seoclsname.
    DATA original_interface_key TYPE seoclskey.

    METHODS source
      IMPORTING
        interface_name TYPE seoclsname
      RETURNING
        VALUE(result)  TYPE seop_source_string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_object_intf IMPLEMENTATION.


  METHOD /apmg/if_apm_object~import.

    DATA(is_pretty) = xsdbool( is_dry_run = abap_false ).

    TRY.
        " TODO: Make files mandatory
        IF files IS INITIAL.
          " Copy globally installed interface
          DATA(import_metadata)  = zif_abapgit_oo_object_fnc~get_interface_properties( original_interface_key ).

          IF import_metadata IS INITIAL.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Not found'.
          ENDIF.

          DATA(interface_code) = source( original_interface_name ).
        ELSE.
          " Import interface from registry
          DATA(xml) = files->get_xml_parsed( ).

          xml->read(
            EXPORTING
              iv_name = 'VSEOINTERF'
            CHANGING
              cg_data = import_metadata ).

          interface_code = files->get_abap( ).
        ENDIF.

        " Rename and create new interface
        DATA(import_key)        = original_interface_key.
        import_key-clsname      = new_object.
        import_metadata-clsname = new_object.

        " Get existing and to be imported code
        IF zif_abapgit_oo_object_fnc~exists( import_key-clsname ).
          DATA(existing_interface_code) = source( import_key-clsname ).
        ENDIF.

        DATA(import_interface_code) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_intfsec_name( original_interface_name )
          program_source = interface_code
          map            = map
          is_pretty      = is_pretty ).

        " Compare existing code to newly imported code to avoid slow REPOSRC updates
        IF is_dry_run IS INITIAL AND import_interface_code <> existing_interface_code.
          zif_abapgit_oo_object_fnc~create(
            EXPORTING
              iv_check      = abap_false
              iv_package    = new_package
            CHANGING
              cg_properties = import_metadata ).

          zif_abapgit_oo_object_fnc~deserialize_source(
            iv_package = new_package
            iv_version = import_metadata-unicode
            is_key     = import_key
            it_source  = import_interface_code ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    original_interface_name = item-obj_name.
    original_interface_key  = VALUE seoclskey( clsname = original_interface_name ).

  ENDMETHOD.


  METHOD source.

    TRY.
        DATA(instance) = cl_oo_factory=>create_instance( ).

        DATA(source_handler) = instance->create_clif_source(
          clif_name = interface_name
          version   = 'A' ).

        source_handler->get_source( IMPORTING source = result ).
      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
