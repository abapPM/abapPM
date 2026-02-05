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

    "! Original interface name
    DATA interface_name TYPE seoclsname.

    METHODS source
      RETURNING
        VALUE(result) TYPE seop_source_string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_object_intf IMPLEMENTATION.


  METHOD /apmg/if_apm_object~import.

    DATA(is_pretty) = xsdbool( is_dry_run = abap_false ).

    TRY.
        " Copy globally installed interface
        DATA(interface_key) = VALUE seoclskey( clsname = interface_name ).

        " TODO: Make files mandatory
        IF files IS INITIAL.
          DATA(interface_metadata)  = zif_abapgit_oo_object_fnc~get_interface_properties( interface_key ).

          IF interface_metadata IS INITIAL.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Not found'.
          ENDIF.

          DATA(orig_interface_code) = source( ).
        ELSE.
          orig_interface_code = files->get_abap( ).

          DATA(xml) = files->get_xml_parsed( ).

          xml->read(
            EXPORTING
              iv_name = 'VSEOINTERF'
            CHANGING
              cg_data = interface_metadata ).
        ENDIF.

        " Rename and create new interface
        interface_key-clsname      = new_object.
        interface_metadata-clsname = new_object.

        DATA(interface_code) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_intfsec_name( interface_name )
          program_source = orig_interface_code
          map            = map
          is_pretty      = is_pretty ).

        IF is_dry_run IS INITIAL AND interface_code <> orig_interface_code.
          zif_abapgit_oo_object_fnc~create(
            EXPORTING
              iv_check      = abap_false
              iv_package    = new_package
            CHANGING
              cg_properties = interface_metadata ).

          zif_abapgit_oo_object_fnc~deserialize_source(
            iv_package = new_package
            iv_version = interface_metadata-unicode
            is_key     = interface_key
            it_source  = interface_code ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    interface_name = item-obj_name.

  ENDMETHOD.


  METHOD source.

    " or /apmg/cl_apm_code_importer=>read( cl_oo_classname_service=>get_intfsec_name( interface_name ) ) ?
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
