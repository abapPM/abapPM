CLASS zcl_abapgit_object_zapm DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_language    TYPE spras
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_package TYPE devclass.

ENDCLASS.



CLASS zcl_abapgit_object_zapm IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    mv_package = is_item-obj_name.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA lx_error TYPE REF TO zcx_package_json.

    TRY.
        zcl_package_json=>factory( mv_package )->delete( ).
      CATCH zcx_package_json INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      lv_json  TYPE string,
      lx_error TYPE REF TO zcx_package_json.

    TRY.
        lv_json = mo_files->read_string(
          iv_ext = |{ zif_package_json_types=>c_package_file-extension }| ).
      CATCH zcx_abapgit_exception.
        " Most probably file not found -> ignore
        RETURN.
    ENDTRY.

    zcl_abapgit_utils=>check_eol( lv_json ).

    TRY.
        zcl_package_json=>factory( mv_package )->set_json( lv_json )->save( ).
      CATCH zcx_package_json INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        zcl_package_json=>factory( mv_package )->load( ).
        rv_bool = abap_true.
      CATCH zcx_package_json.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-early TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.

    IF iv_filename <> zif_package_json_types=>c_package_file.
      zcx_abapgit_exception=>raise( |Unexpected filename for apm package: { iv_filename }| ).
    ENDIF.

    " Try to get a unique package name by using the path
    cs_item-obj_name = zcl_abapgit_folder_logic=>get_instance( )->path_to_package(
      iv_top                  = iv_package
      io_dot                  = io_dot
      iv_create_if_not_exists = abap_false
      iv_path                 = iv_path ).

  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.

    " Packages have a fixed filename so that the repository can be installed to a different
    " package(-hierarchy) on the client and not show up as a different package in the repo.
    cv_filename = zif_package_json_types=>c_package_file.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA li_package_json TYPE REF TO zif_package_json.

    TRY.
        li_package_json = zcl_package_json=>factory( mv_package )->load( ).
      CATCH zcx_package_json.
        RETURN. " ignore errors
    ENDTRY.

    mo_files->add_string(
      iv_ext    = |{ zif_package_json_types=>c_package_file-extension }|
      iv_string = li_package_json->get_json( ) ).

  ENDMETHOD.
ENDCLASS.
