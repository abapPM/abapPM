CLASS /apmg/cl_apm_abapgit_serialize DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* apm abapGit Serializer
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
* This is a replacement for ZCL_ABAPGIT_SERIALIZE
*
* Using ZCL_ABAPGIT_SERIALIZE would include APACK and TABUs which are
* not supported in apm
************************************************************************
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_dot_abapgit    TYPE REF TO zcl_abapgit_dot_abapgit OPTIONAL
        !is_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings OPTIONAL
      RAISING
        zcx_abapgit_exception.

    METHODS serialize
      IMPORTING
        !iv_package          TYPE devclass OPTIONAL
        !it_tadir            TYPE zif_abapgit_definitions=>ty_tadir_tt
        !ii_log              TYPE REF TO zif_abapgit_log OPTIONAL
        !iv_force_sequential TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_files)      TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

    METHODS files_local
      IMPORTING
        !iv_package     TYPE devclass
        !ii_log         TYPE REF TO zif_abapgit_log
        !it_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_unsupported_count,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        count    TYPE i,
      END OF ty_unsupported_count.
    TYPES:
      ty_unsupported_count_tt TYPE HASHED TABLE OF ty_unsupported_count WITH UNIQUE KEY obj_type.
    TYPES:
      ty_char32 TYPE c LENGTH 32.

    DATA mt_files TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA mi_log TYPE REF TO zif_abapgit_log.
    DATA mo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA ms_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings.
    DATA ms_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params.
    DATA mo_abap_language_version TYPE REF TO zcl_abapgit_abap_language_vers.

    METHODS add_dot_abapgit
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

    METHODS add_to_return
      IMPORTING
        iv_path      TYPE string
        is_file_item TYPE zif_abapgit_objects=>ty_serialization.

    METHODS run_sequential
      IMPORTING
        !is_tadir TYPE zif_abapgit_definitions=>ty_tadir
      RAISING
        zcx_abapgit_exception.

    METHODS add_objects
      IMPORTING
        !iv_package     TYPE devclass
        !ii_log         TYPE REF TO zif_abapgit_log
        !it_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      CHANGING
        VALUE(ct_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception.

    METHODS filter_unsupported_objects
      CHANGING
        !ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    METHODS filter_ignored_objects
      IMPORTING
        !iv_package TYPE devclass
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

ENDCLASS.



CLASS /apmg/cl_apm_abapgit_serialize IMPLEMENTATION.


  METHOD add_dot_abapgit.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.

    APPEND INITIAL LINE TO ct_files ASSIGNING <ls_file>.
    <ls_file>-file = mo_dot_abapgit->to_file( ).

  ENDMETHOD.


  METHOD add_objects.

    DATA: lo_filter TYPE REF TO zcl_abapgit_repo_filter,
          lv_force  TYPE abap_bool,
          lt_found  LIKE ct_files,
          lt_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = iv_package
      iv_ignore_subpackages = ms_local_settings-ignore_subpackages
      iv_only_local_objects = ms_local_settings-only_local_objects
      io_dot                = mo_dot_abapgit
      ii_log                = ii_log
      it_filter             = it_filter ).

    CREATE OBJECT lo_filter.

    lo_filter->apply( EXPORTING it_filter = it_filter
                      CHANGING  ct_tadir  = lt_tadir ).

* if there are less than 10 objects run in single thread
* this helps a lot when debugging, plus performance gain
* with low number of objects does not matter much
    lv_force = boolc( lines( lt_tadir ) < 10 ).

    lt_found = serialize(
      iv_package          = iv_package
      it_tadir            = lt_tadir
      ii_log              = ii_log
      iv_force_sequential = lv_force ).
    APPEND LINES OF lt_found TO ct_files.

  ENDMETHOD.


  METHOD add_to_return.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF is_file_item-files,
                   <ls_return> LIKE LINE OF mt_files.


    LOOP AT is_file_item-files ASSIGNING <ls_file>.
      APPEND INITIAL LINE TO mt_files ASSIGNING <ls_return>.
      <ls_return>-file = <ls_file>.
      <ls_return>-file-path = iv_path.
      <ls_return>-item = is_file_item-item.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mo_dot_abapgit = io_dot_abapgit.
    ms_local_settings = is_local_settings.

    IF io_dot_abapgit IS BOUND.
      ms_i18n_params = io_dot_abapgit->determine_i18n_parameters( is_local_settings-main_language_only ).
    ELSE.
      ms_i18n_params-main_language      = sy-langu.
      ms_i18n_params-main_language_only = is_local_settings-main_language_only.
    ENDIF.

    CREATE OBJECT mo_abap_language_version
      EXPORTING
        io_dot_abapgit = mo_dot_abapgit.

  ENDMETHOD.


  METHOD files_local.

    add_dot_abapgit( CHANGING ct_files = rt_files ).

    add_objects(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
        it_filter  = it_filter
      CHANGING
        ct_files   = rt_files ).

  ENDMETHOD.


  METHOD filter_ignored_objects.

    DATA:
      ls_ignored_count TYPE ty_unsupported_count,
      lt_ignored_count TYPE ty_unsupported_count_tt,
      lo_folder_logic  TYPE REF TO zcl_abapgit_folder_logic,
      ls_item          TYPE zif_abapgit_definitions=>ty_item,
      lv_path          TYPE string,
      lv_filename      TYPE string.

    FIELD-SYMBOLS:
      <ls_tadir>         LIKE LINE OF ct_tadir,
      <ls_ignored_count> TYPE ty_unsupported_count.

    " Ignore logic requires .abapGit.xml
    IF mo_dot_abapgit IS INITIAL OR iv_package IS INITIAL OR mi_log IS INITIAL.
      RETURN.
    ENDIF.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_ignored_count.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      IF <ls_tadir>-devclass IS NOT INITIAL.
        lv_path = lo_folder_logic->package_to_path(
          iv_top     = iv_package
          io_dot     = mo_dot_abapgit
          iv_package = <ls_tadir>-devclass ).
      ELSE.
        lv_path = mo_dot_abapgit->get_starting_folder( ).
      ENDIF.

      lv_filename = zcl_abapgit_filename_logic=>object_to_file(
        is_item  = ls_item
        iv_ext   = '*' ).

      IF mo_dot_abapgit->is_ignored(
        iv_path     = lv_path
        iv_filename = lv_filename ) = abap_false.
        CONTINUE.
      ENDIF.

      READ TABLE lt_ignored_count ASSIGNING <ls_ignored_count> WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_ignored_count-obj_type = <ls_tadir>-object.
        ls_ignored_count-count    = 1.
        ls_ignored_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_ignored_count INTO TABLE lt_ignored_count ASSIGNING <ls_ignored_count>.
      ELSE.
        CLEAR: <ls_ignored_count>-obj_name.
        <ls_ignored_count>-count = <ls_ignored_count>-count + 1.
      ENDIF.
      " init object so we can remove these entries afterward
      CLEAR <ls_tadir>-object.
    ENDLOOP.
    IF lt_ignored_count IS INITIAL.
      RETURN.
    ENDIF.

    " remove ignored objects
    DELETE ct_tadir WHERE object IS INITIAL.

    LOOP AT lt_ignored_count ASSIGNING <ls_ignored_count>.
      IF <ls_ignored_count>-count = 1.
        mi_log->add_warning( |Object { <ls_ignored_count>-obj_type } { <ls_ignored_count>-obj_name } ignored| ).
      ELSE.
        mi_log->add_warning( |Object type { <ls_ignored_count>-obj_type } with | &&
                             |{ <ls_ignored_count>-count } objects ignored| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_unsupported_objects.

    DATA: ls_unsupported_count TYPE ty_unsupported_count,
          lt_supported_types   TYPE zif_abapgit_objects=>ty_types_tt,
          lt_unsupported_count TYPE ty_unsupported_count_tt.

    FIELD-SYMBOLS: <ls_tadir>             LIKE LINE OF ct_tadir,
                   <ls_unsupported_count> TYPE ty_unsupported_count.

    lt_supported_types = zcl_abapgit_objects=>supported_list( ).
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_unsupported_count.
      READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>
                                      WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_unsupported_count-obj_type = <ls_tadir>-object.
        ls_unsupported_count-count    = 1.
        ls_unsupported_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_unsupported_count INTO TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>.
      ELSE.
        CLEAR: <ls_unsupported_count>-obj_name.
        <ls_unsupported_count>-count = <ls_unsupported_count>-count + 1.
      ENDIF.
      CLEAR: <ls_tadir>-object.
    ENDLOOP.
    IF lt_unsupported_count IS INITIAL.
      RETURN.
    ENDIF.

    DELETE ct_tadir WHERE object IS INITIAL.
    IF mi_log IS BOUND.
      LOOP AT lt_unsupported_count ASSIGNING <ls_unsupported_count>.
        IF <ls_unsupported_count>-count = 1.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-obj_name } ignored| ).
        ELSE.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-count } objects ignored| ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD run_sequential.

    DATA: lx_error     TYPE REF TO zcx_abapgit_exception,
          ls_file_item TYPE zif_abapgit_objects=>ty_serialization.

    ls_file_item-item-obj_type  = is_tadir-object.
    ls_file_item-item-obj_name  = is_tadir-obj_name.
    ls_file_item-item-devclass  = is_tadir-devclass.
    ls_file_item-item-srcsystem = is_tadir-srcsystem.
    ls_file_item-item-abap_language_version = mo_abap_language_version->get_repo_abap_language_version( ).

    TRY.
        ls_file_item = /apmg/cl_apm_abapgit_objects=>serialize(
          is_item        = ls_file_item-item
          io_i18n_params = zcl_abapgit_i18n_params=>new( is_params = ms_i18n_params ) ).

        add_to_return( is_file_item = ls_file_item
                       iv_path      = is_tadir-path ).
      CATCH zcx_abapgit_exception INTO lx_error.
        IF NOT mi_log IS INITIAL.
          mi_log->add_exception(
              ix_exc  = lx_error
              is_item = ls_file_item-item ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD serialize.

* serializes only objects

    DATA: lv_max      TYPE i,
          lv_count    TYPE i,
          li_progress TYPE REF TO zif_abapgit_progress,
          li_exit     TYPE REF TO zif_abapgit_exit,
          lo_timer    TYPE REF TO zcl_abapgit_timer,
          lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    CLEAR mt_files.

    mi_log = ii_log.

    lt_tadir = it_tadir.
    filter_unsupported_objects( CHANGING ct_tadir = lt_tadir ).

    filter_ignored_objects(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = lt_tadir ).

    lv_count = lines( lt_tadir ).

    li_progress = zcl_abapgit_progress=>get_instance( lv_count ).

    lo_timer = zcl_abapgit_timer=>create(
      iv_text  = 'Serialize:'
      iv_count = lv_count )->start( ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Serialize { <ls_tadir>-obj_name }, { lv_max } thread| ).
      run_sequential( <ls_tadir> ).
    ENDLOOP.

    li_progress->off( ).

    rt_files = mt_files.
    FREE mt_files.

    lo_timer->end( abap_true ).

  ENDMETHOD.
ENDCLASS.
