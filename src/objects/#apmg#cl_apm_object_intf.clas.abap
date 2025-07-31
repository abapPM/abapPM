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

    DATA interface_name TYPE seoclsname.

ENDCLASS.



CLASS /apmg/cl_apm_object_intf IMPLEMENTATION.


  METHOD /apmg/if_apm_object~import.

    DATA(is_pretty) = xsdbool( is_dryrun = abap_false ).

    TRY.
        " Get old interface
        DATA(interface_key) = VALUE seoclskey( clsname = interface_name ).

        DATA(interface_metadata) = zif_abapgit_oo_object_fnc~get_interface_properties( interface_key ).

        IF interface_metadata IS INITIAL.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Not found'.
        ENDIF.

        " Rename and create new interface
        interface_key-clsname      = new_object.
        interface_metadata-clsname = new_object.

        " TODO: Make files mandatory
        IF files IS INITIAL.
          DATA(orig_interface_code) = /apmg/cl_apm_code_importer=>read(
            cl_oo_classname_service=>get_intfsec_name( interface_name ) ).
        ELSE.
          orig_interface_code = files->get_abap( ).
        ENDIF.

        DATA(interface_code) = /apmg/cl_apm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_intfsec_name( interface_name )
          program_source = orig_interface_code
          map            = map
          is_pretty      = is_pretty ).

        IF is_dryrun IS INITIAL AND interface_code <> orig_interface_code.
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
ENDCLASS.
