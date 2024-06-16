CLASS zcl_abapgit_object_zapm DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Object Type
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* This is a virtual object type used by abapGit to serialize and
* deserialize apm package metadata i.e. the package.abap.json file
*
* This virtual object type must be added to abapGit via exit!
************************************************************************
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

    CONSTANTS:
      " Must match zcl_apm_abapgit_ext but we don't want a dependency between classes
      " which could cause problems when merging into standalone version
      c_int_package_json TYPE string VALUE 'package.zapm.json',
      c_int_readme       TYPE string VALUE 'package.zapm.readme.md'.

    CONSTANTS:
      c_key_type TYPE string VALUE 'PACKAGE',
      BEGIN OF c_key_extra,
        package_json TYPE string VALUE 'PACKAGE_JSON',
        readme       TYPE string VALUE 'README',
      END OF c_key_extra.

    DATA mv_is_installed TYPE abap_bool.
    DATA mv_package TYPE devclass.

    METHODS get_package_key
      RETURNING
        VALUE(result) TYPE zif_persist_apm=>ty_key.

    METHODS get_readme_key
      RETURNING
        VALUE(result) TYPE zif_persist_apm=>ty_key.

ENDCLASS.



CLASS zcl_abapgit_object_zapm IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      is_item        = is_item
      iv_language    = iv_language
      io_files       = io_files
      io_i18n_params = io_i18n_params ).

    mv_package = replace(
      val  = is_item-obj_name
      sub  = '%'
      with = '$'
      occ  = 0 ).

    mv_is_installed = lcl_persist_apm=>is_installed( ).

  ENDMETHOD.


  METHOD get_package_key.
    result = |{ c_key_type }:{ mv_package }:{ c_key_extra-package_json }|.
  ENDMETHOD.


  METHOD get_readme_key.
    result = |{ c_key_type }:{ mv_package }:{ c_key_extra-readme }|.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    CHECK mv_is_installed = abap_true.

    rv_user = lcl_persist_apm=>get_instance( )->load( get_package_key( ) )-luser.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CHECK mv_is_installed = abap_true.

    lcl_persist_apm=>get_instance( )->delete( get_package_key( ) ).
    lcl_persist_apm=>get_instance( )->delete( get_readme_key( ) ).

    tadir_delete( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lv_data TYPE string.

    CHECK mv_is_installed = abap_true.

    " Package JSON
    lv_data = mo_files->read_string(
      iv_ext = 'json' ).

    lcl_persist_apm=>get_instance( )->save(
      iv_key   = get_package_key( )
      iv_value = lv_data ).

    " Readme
    TRY.
        lv_data = mo_files->read_string(
          iv_ext   = 'md'
          iv_extra = 'readme' ).

        lcl_persist_apm=>get_instance( )->save(
          iv_key   = get_readme_key( )
          iv_value = lv_data ).

      CATCH zcx_abapgit_exception ##NO_HANDLER.
        " Most probably file not found -> ignore
    ENDTRY.

    tadir_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CHECK mv_is_installed = abap_true.

    rv_bool = boolc( lcl_persist_apm=>get_instance( )->load( get_package_key( ) )-value IS NOT INITIAL ).

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
    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = |{ lif_persist_apm=>c_lock }|
      iv_argument    = |{ get_package_key( ) }| ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " TODO: open apm package view
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.

    IF iv_filename <> c_int_package_json AND iv_filename <> c_int_readme.
      zcx_abapgit_exception=>raise( |Unexpected filename for apm package: { iv_filename }| ).
    ENDIF.

    " Try to get a unique package name by using the path
    cs_item-obj_name = zcl_abapgit_folder_logic=>get_instance( )->path_to_package(
      iv_top                  = iv_package
      io_dot                  = io_dot
      iv_create_if_not_exists = abap_false
      iv_path                 = iv_path ).

    " Logical transport objects don't allow $ so we replace them with %
    cs_item-obj_name = replace(
      val  = cs_item-obj_name
      sub  = '$'
      with = '%'
      occ  = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.

    IF iv_ext = '*'.
      cv_filename = 'package.zapm.*'.
    ELSEIF iv_extra IS INITIAL.
      cv_filename = c_int_package_json.
    ELSEIF iv_extra = 'readme'.
      cv_filename = c_int_readme.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA lv_data TYPE string.

    CHECK mv_is_installed = abap_true.

    " Package JSON
    lv_data = lcl_persist_apm=>get_instance( )->load( get_package_key( ) )-value.
    IF lv_data IS NOT INITIAL.
      mo_files->add_string(
        iv_ext    = 'json'
        iv_string = lv_data ).
    ENDIF.

    " Readme
    lv_data = lcl_persist_apm=>get_instance( )->load( get_readme_key( ) )-value.
    IF lv_data IS NOT INITIAL.
      mo_files->add_string(
        iv_ext    = 'md'
        iv_extra  = 'readme'
        iv_string = lv_data ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
