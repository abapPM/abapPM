CLASS zcl_abappm_object_intf DEFINITION PUBLIC FINAL CREATE PUBLIC
  INHERITING FROM zcl_abapgit_oo_interface.

  PUBLIC SECTION.

    INTERFACES zif_abappm_object.

    METHODS constructor
      IMPORTING
        !item TYPE zif_abappm_object=>ty_item.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA interface_name TYPE seoclsname.

ENDCLASS.



CLASS zcl_abappm_object_intf IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    interface_name = item-obj_name.

  ENDMETHOD.


  METHOD zif_abappm_object~import.

    DATA:
      interface_key      TYPE seoclskey,
      interface_metadata TYPE vseointerf,
      interface_code     TYPE seop_source_string.

    TRY.
        " Get old interface
        interface_key-clsname = interface_name.
        interface_metadata    = zif_abapgit_oo_object_fnc~get_interface_properties( interface_key ).

        IF interface_metadata IS INITIAL.
          zcx_abappm_error=>raise( 'Not found' ).
        ENDIF.

        " Rename and create new interface
        interface_key-clsname      = new_object.
        interface_metadata-clsname = new_object.

        zif_abapgit_oo_object_fnc~create(
          EXPORTING
            iv_check      = abap_false
            iv_package    = new_package
          CHANGING
            cg_properties = interface_metadata ).

        IF files IS NOT INITIAL.
          interface_code = files->get_abap( ).
        ENDIF.

        DATA(is_pretty) = xsdbool( is_dryrun = abap_false ).

        interface_code = zcl_abappm_code_importer=>import(
          program_name   = cl_oo_classname_service=>get_intfsec_name( interface_name )
          program_source = interface_code
          map            = map
          is_pretty      = is_pretty ).

        IF is_dryrun IS INITIAL.
          zif_abapgit_oo_object_fnc~deserialize_source(
            iv_package = new_package
            iv_version = interface_metadata-unicode
            is_key     = interface_key
            it_source  = interface_code ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
