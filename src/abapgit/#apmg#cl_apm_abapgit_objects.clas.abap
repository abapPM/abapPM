CLASS /apmg/cl_apm_abapgit_objects DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* apm abapGit Objects
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
* This is a replacement for ZCL_ABAPGIT_OBJECTS
*
* Using ZCL_ABAPGIT_OBJECTS would drag in many other dependencies
* which are unnecessary for apm (like the abapGit repo layer).
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING
        !is_item                 TYPE zif_abapgit_definitions=>ty_item
        !io_i18n_params          TYPE REF TO zcl_abapgit_i18n_params
      RETURNING
        VALUE(rs_files_and_item) TYPE zif_abapgit_objects=>ty_serialization
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS deserialize
      IMPORTING
        !iv_package              TYPE devclass
        !iv_language             TYPE spras
        !iv_transport            TYPE trkorr OPTIONAL
        !it_local                TYPE zif_abapgit_definitions=>ty_files_item_tt OPTIONAL
        !it_local_checksums      TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt OPTIONAL
        !it_remote               TYPE zif_abapgit_git_definitions=>ty_files_tt
        !io_dot                  TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_log                  TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rt_accessed_files) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS delete
      IMPORTING
        !it_tadir     TYPE zif_abapgit_definitions=>ty_tadir_tt
        !iv_transport TYPE trkorr OPTIONAL
        !ii_log       TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS jump
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !is_sub_item    TYPE zif_abapgit_definitions=>ty_item OPTIONAL
        !iv_filename    TYPE string OPTIONAL
        !iv_line_number TYPE i OPTIONAL
        !iv_new_window  TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS is_supported
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_bool)  TYPE abap_bool.

    CLASS-METHODS is_type_supported
      IMPORTING
        !iv_obj_type   TYPE zif_abapgit_definitions=>ty_item-obj_type
      RETURNING
        VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS exists
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS supported_list
      RETURNING
        VALUE(rt_types) TYPE zif_abapgit_objects=>ty_types_tt.

    CLASS-METHODS is_active
      IMPORTING
        !is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_supported_types,
        obj_type  TYPE tadir-object,
        supported TYPE abap_bool,
      END OF ty_supported_types.

    TYPES: ty_supported_types_tt TYPE SORTED TABLE OF ty_supported_types WITH UNIQUE KEY obj_type.

    TYPES:
      BEGIN OF ty_obj_serializer_item,
        item     TYPE zif_abapgit_definitions=>ty_item,
        metadata TYPE zif_abapgit_definitions=>ty_metadata,
      END OF ty_obj_serializer_item.
    TYPES:
      ty_obj_serializer_map TYPE SORTED TABLE OF ty_obj_serializer_item WITH UNIQUE KEY item.

    CLASS-DATA gt_obj_serializer_map TYPE ty_obj_serializer_map.
    CLASS-DATA gt_supported_obj_types TYPE ty_supported_types_tt.
    CLASS-DATA gv_supported_obj_types_loaded TYPE abap_bool.

    CLASS-METHODS files_to_deserialize
      IMPORTING
        !iv_package         TYPE devclass
        !it_local           TYPE zif_abapgit_definitions=>ty_files_item_tt
        !it_local_checksums TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
        !it_remote          TYPE zif_abapgit_git_definitions=>ty_files_tt
        !io_dot             TYPE REF TO zcl_abapgit_dot_abapgit
        !ii_log             TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results)   TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS prioritize_deser
      IMPORTING
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.

    CLASS-METHODS class_name
      IMPORTING
        !is_item             TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_class_name) TYPE string.

    CLASS-METHODS update_package_tree
      IMPORTING
        !iv_package TYPE devclass.

    CLASS-METHODS delete_object
      IMPORTING
        !iv_package   TYPE devclass
        !is_item      TYPE zif_abapgit_definitions=>ty_item
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS deserialize_steps
      IMPORTING
        !it_steps       TYPE zif_abapgit_objects=>ty_step_data_tt
        !ii_log         TYPE REF TO zif_abapgit_log
        !iv_transport   TYPE trkorr
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params
      CHANGING
        !ct_files       TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS deserialize_step
      IMPORTING
        !is_step      TYPE zif_abapgit_objects=>ty_step_data
        !ii_log       TYPE REF TO zif_abapgit_log
        !iv_transport TYPE trkorr
      CHANGING
        !ct_files     TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS deserialize_lxe
      IMPORTING
        !is_step        TYPE zif_abapgit_objects=>ty_step_data
        !ii_log         TYPE REF TO zif_abapgit_log
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params
      CHANGING
        !ct_files       TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS update_original_system
      IMPORTING
        !it_items     TYPE zif_abapgit_definitions=>ty_items_tt
        !ii_log       TYPE REF TO zif_abapgit_log
        !io_dot       TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS check_objects_locked
      IMPORTING
        !it_items TYPE zif_abapgit_definitions=>ty_items_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS create_object
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        !io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
        !is_metadata    TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
        !iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_obj)   TYPE REF TO zif_abapgit_object
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS map_tadir_to_items
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt.

    CLASS-METHODS map_results_to_items
      IMPORTING
        !it_results     TYPE zif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(rt_items) TYPE zif_abapgit_definitions=>ty_items_tt.

    CLASS-METHODS filter_files_to_deserialize
      IMPORTING
        !it_results       TYPE zif_abapgit_definitions=>ty_results_tt
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.

    CLASS-METHODS get_deserialize_steps
      RETURNING
        VALUE(rt_steps) TYPE zif_abapgit_objects=>ty_step_data_tt.

    CLASS-METHODS check_main_package
      IMPORTING
        !iv_package  TYPE devclass
        !iv_obj_type TYPE tadir-object
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS change_package_assignments
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item
        !ii_log  TYPE REF TO zif_abapgit_log.

    CLASS-METHODS determine_i18n_params
      IMPORTING
        !io_dot                TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_main_language_only TYPE abap_bool
      RETURNING
        VALUE(rs_i18n_params)  TYPE zif_abapgit_definitions=>ty_i18n_params
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_extra_from_filename
      IMPORTING
        !iv_filename    TYPE string
      RETURNING
        VALUE(rv_extra) TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_abapgit_objects IMPLEMENTATION.


  METHOD change_package_assignments.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = is_item-obj_type
        wi_tadir_obj_name = is_item-obj_name
        wi_tadir_devclass = is_item-devclass
        wi_test_modus     = abap_false
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc = 0.
      ii_log->add_success( iv_msg  = |Object { is_item-obj_name } assigned to package { is_item-devclass }|
                           is_item = is_item ).
    ELSE.
      ii_log->add_error( iv_msg  = |Package change of object { is_item-obj_name } failed|
                         is_item = is_item ).
    ENDIF.

  ENDMETHOD.


  METHOD check_main_package.

    " check package restrictions, closed package, descriptive or
    " functional package
    cl_pak_object_types=>check_object_type(
      EXPORTING
        i_working_mode         = 'I'
        i_package_name         = iv_package
        i_pgmid                = 'R3TR'
        i_object_type          = iv_obj_type
      EXCEPTIONS
        wrong_object_type      = 1
        package_not_extensible = 2
        package_not_loaded     = 3
        OTHERS                 = 4 ).
    CASE sy-subrc.
      WHEN 0.
        RETURN.
      WHEN 2.
        zcx_abapgit_exception=>raise( |Object type { iv_obj_type } not allowed for package { iv_package }| ).
      WHEN OTHERS.
        zcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_objects_locked.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    FIELD-SYMBOLS: <ls_item> LIKE LINE OF it_items.

    LOOP AT it_items ASSIGNING <ls_item>.

      " You should remember that we ignore not supported objects here,
      " because otherwise the process aborts which is not desired
      IF is_type_supported( <ls_item>-obj_type ) = abap_false.
        CONTINUE.
      ENDIF.

      li_obj = create_object( <ls_item> ).

      IF li_obj->is_locked( ) = abap_true.
        zcx_abapgit_exception=>raise( |Object { <ls_item>-obj_type } { <ls_item>-obj_name } |
                                   && |is locked. Action not possible.| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD class_name.

    CONCATENATE 'ZCL_ABAPGIT_OBJECT_' is_item-obj_type INTO rv_class_name.

  ENDMETHOD.


  METHOD create_object.

    DATA: lv_message            TYPE string,
          lv_class_name         TYPE string,
          ls_obj_serializer_map LIKE LINE OF gt_obj_serializer_map.

    " serialize & deserialize require files and i18n parameters,
    " other calls are good without them
    ASSERT io_files IS BOUND AND io_i18n_params IS BOUND OR
           io_files IS NOT BOUND AND io_i18n_params IS NOT BOUND.

    READ TABLE gt_obj_serializer_map
      INTO ls_obj_serializer_map WITH KEY item = is_item.
    IF sy-subrc = 0.
      lv_class_name = ls_obj_serializer_map-metadata-class.
    ELSEIF is_metadata IS NOT INITIAL.
*        Metadata is provided only on deserialization
*        Once this has been triggered, the same deserializer shall be used
*        for subsequent processes.
*        Thus, buffer the metadata afterwards
      ls_obj_serializer_map-item      = is_item.
      ls_obj_serializer_map-metadata  = is_metadata.
      INSERT ls_obj_serializer_map INTO TABLE gt_obj_serializer_map.
      lv_class_name = is_metadata-class.
    ELSE.
      lv_class_name = class_name( is_item ).
    ENDIF.

    REPLACE FIRST OCCURRENCE OF 'LCL' IN lv_class_name WITH 'ZCL_ABAPGIT'.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of object handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    TRY.
        IF io_files IS BOUND AND io_i18n_params IS BOUND.
          CREATE OBJECT ri_obj TYPE (lv_class_name)
            EXPORTING
              is_item        = is_item
              iv_language    = io_i18n_params->ms_params-main_language
              io_files       = io_files
              io_i18n_params = io_i18n_params.
        ELSE.
          CREATE OBJECT ri_obj TYPE (lv_class_name)
            EXPORTING
              is_item     = is_item
              iv_language = zif_abapgit_definitions=>c_english.
        ENDIF.
      CATCH cx_sy_create_object_error.
        lv_message = |Object type { is_item-obj_type } is not supported by this system|.
        " No support for bridge in apm
        zcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.


  METHOD delete.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          li_progress TYPE REF TO zif_abapgit_progress,
          lt_tadir    LIKE it_tadir,
          lt_deleted  LIKE it_tadir,
          lt_items    TYPE zif_abapgit_definitions=>ty_items_tt,
          lx_error    TYPE REF TO zcx_abapgit_exception,
          lv_count    TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.

    IF it_tadir IS INITIAL.
      RETURN.
    ENDIF.

    lt_tadir = it_tadir.

    IF ii_log IS BOUND.
      IF lines( lt_tadir ) = 1.
        ii_log->add_info( |>>> Deleting 1 object| ).
      ELSE.
        ii_log->add_info( |>>> Deleting { lines( lt_tadir ) } objects| ).
      ENDIF.
    ENDIF.

    IF iv_transport IS NOT INITIAL.
      zcl_abapgit_factory=>get_default_transport( )->set( iv_transport ).
    ENDIF.

    TRY.
        zcl_abapgit_dependencies=>resolve( CHANGING ct_tadir = lt_tadir ).

        li_progress = zcl_abapgit_progress=>get_instance( lines( lt_tadir ) ).

        lt_items = map_tadir_to_items( lt_tadir ).

        check_objects_locked( lt_items ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcl_abapgit_factory=>get_default_transport( )->reset( ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    lv_count = 1.
    DO.
      CLEAR lt_deleted.
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        li_progress->show( iv_current = lv_count
                           iv_text    = |Delete { <ls_tadir>-obj_name }| ).

        CLEAR ls_item.
        ls_item-obj_type = <ls_tadir>-object.
        ls_item-obj_name = <ls_tadir>-obj_name.

        TRY.
            delete_object(
              iv_package   = <ls_tadir>-devclass
              is_item      = ls_item
              iv_transport = iv_transport ).

            INSERT <ls_tadir> INTO TABLE lt_deleted.
            DELETE lt_tadir.
            lv_count = lv_count + 1.

            " make sure to save object deletions
            COMMIT WORK.

            IF ii_log IS BOUND.
              ii_log->add_info( iv_msg  = |Object { ls_item-obj_type } { ls_item-obj_name } deleted|
                                is_item = ls_item ).
            ENDIF.

          CATCH zcx_abapgit_exception INTO lx_error.
            IF ii_log IS BOUND.
              ii_log->add_exception( ix_exc  = lx_error
                                     is_item = ls_item ).
              ii_log->add_error( iv_msg = |Deletion of object { ls_item-obj_name } failed|
                                 is_item = ls_item ).
            ENDIF.
        ENDTRY.

      ENDLOOP.

      " Exit if done or nothing else was deleted
      IF lines( lt_tadir ) = 0 OR lines( lt_deleted ) = 0.
        EXIT.
      ENDIF.
    ENDDO.

    zcl_abapgit_factory=>get_default_transport( )->reset( ).

    IF lx_error IS BOUND AND lines( lt_tadir ) > 0.
      zcx_abapgit_exception=>raise( 'Error during uninstall. Check the log.' ).
    ENDIF.

    li_progress->off( ).

  ENDMETHOD.


  METHOD delete_object.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    " Nothing to do for unsupported objects
    IF is_type_supported( is_item-obj_type ) = abap_false.
      RETURN.
    ENDIF.

    li_obj = create_object( is_item ).
    li_obj->delete( iv_package   = iv_package
                    iv_transport = iv_transport ).

  ENDMETHOD.


  METHOD deserialize.

    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
          li_obj      TYPE REF TO zif_abapgit_object,
          lv_package  TYPE devclass,
          lo_files    TYPE REF TO zcl_abapgit_objects_files,
          ls_metadata TYPE zif_abapgit_definitions=>ty_metadata,
          lo_xml      TYPE REF TO zif_abapgit_xml_input,
          lt_results  TYPE zif_abapgit_definitions=>ty_results_tt,
          li_progress TYPE REF TO zif_abapgit_progress,
          lv_path     TYPE string,
          lt_items    TYPE zif_abapgit_definitions=>ty_items_tt,
          lt_steps_id TYPE zif_abapgit_objects=>ty_deserialization_step_tt,
          lt_steps    TYPE zif_abapgit_objects=>ty_step_data_tt,
          lx_exc      TYPE REF TO zcx_abapgit_exception.
    DATA lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.
    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.
    DATA lo_timer TYPE REF TO zcl_abapgit_timer.
    DATA lo_abap_language_vers TYPE REF TO zcl_abapgit_abap_language_vers.

    FIELD-SYMBOLS: <ls_result>  TYPE zif_abapgit_definitions=>ty_result,
                   <lv_step_id> TYPE LINE OF zif_abapgit_objects=>ty_deserialization_step_tt,
                   <ls_step>    TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt,
                   <ls_deser>   TYPE LINE OF zif_abapgit_objects=>ty_deserialization_tt.

    lt_steps = get_deserialize_steps( ).

    IF iv_transport IS NOT INITIAL.
      zcl_abapgit_factory=>get_default_transport( )->set( iv_transport ).
    ENDIF.

    zcl_abapgit_objects_activation=>clear( ).

    lt_results = files_to_deserialize(
      iv_package         = iv_package
      it_local           = it_local
      it_local_checksums = it_local_checksums
      it_remote          = it_remote
      io_dot             = io_dot
      ii_log             = ii_log ).

    IF lt_results IS INITIAL.
      RETURN.
    ENDIF.

    li_progress = zcl_abapgit_progress=>get_instance( lines( lt_results ) ).

    lt_items = map_results_to_items( lt_results ).

    lo_timer = zcl_abapgit_timer=>create(
      iv_text  = 'Deserialize:'
      iv_count = lines( lt_items ) )->start( ).

    zcl_abapgit_factory=>get_cts_api( )->confirm_transport_messages( ).

    check_objects_locked( lt_items ).

    lo_i18n_params = zcl_abapgit_i18n_params=>new( is_params = determine_i18n_params(
      io_dot                = io_dot
      iv_main_language_only = abap_false ) ). "io_repo->get_local_settings( )-main_language_only

    IF lines( lt_items ) = 1.
      ii_log->add_info( |>>> Deserializing 1 object| ).
    ELSE.
      ii_log->add_info( |>>> Deserializing { lines( lt_items ) } objects| ).
    ENDIF.

    CREATE OBJECT lo_abap_language_vers
      EXPORTING
        io_dot_abapgit = io_dot.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT lt_results ASSIGNING <ls_result>.
      li_progress->show( iv_current = sy-tabix
                         iv_text    = |Prepare Deserialize: { <ls_result>-obj_type } { <ls_result>-obj_name }| ).

      CLEAR ls_item.
      CLEAR: lv_path, lv_package.

      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      "error handling & logging added
      TRY.
          IF ls_item-obj_type <> 'NSPC'.
            " If package does not exist yet, it will be created with this call
            lv_package = lo_folder_logic->path_to_package(
              iv_top  = iv_package
              io_dot  = io_dot
              iv_path = <ls_result>-path ).

            check_main_package(
              iv_package  = lv_package
              iv_obj_type = ls_item-obj_type ).
          ENDIF.

          IF ls_item-obj_type = 'DEVC'.
            " Packages have the same filename across different folders. The path needs to be supplied
            " to find the correct file.
            lv_path = <ls_result>-path.
          ENDIF.

          ls_item-devclass = lv_package.
          ls_item-abap_language_version = lo_abap_language_vers->get_abap_language_vers_by_objt(
                                                                    iv_object_type = ls_item-obj_type
                                                                    iv_package = lv_package ).

          IF <ls_result>-packmove = abap_true.
            " Move object to new package
            change_package_assignments( is_item = ls_item
                                        ii_log  = ii_log ).
            " No other changes required
            CONTINUE.
          ENDIF.

          " Create or update object
          lo_files = zcl_abapgit_objects_files=>new(
            is_item = ls_item
            iv_path = lv_path ).

          lo_files->set_files( it_remote ).

          IF lo_files->is_json_metadata( ) = abap_false.
            "analyze XML in order to instantiate the proper serializer
            lo_xml = lo_files->read_xml( ).
            ls_metadata = lo_xml->get_metadata( ).
          ELSE.
            " there's no XML and metadata for JSON format
            CLEAR: lo_xml, ls_metadata.
          ENDIF.

          li_obj = create_object(
            is_item        = ls_item
            is_metadata    = ls_metadata
            io_files       = lo_files
            io_i18n_params = lo_i18n_params ).

          "get required steps for deserialize the object
          lt_steps_id = li_obj->get_deserialize_steps( ).

          LOOP AT lt_steps_id ASSIGNING <lv_step_id>.
            READ TABLE lt_steps WITH KEY step_id = <lv_step_id> ASSIGNING <ls_step>.
            ASSERT sy-subrc = 0.
            IF <lv_step_id> = zif_abapgit_object=>gc_step_id-ddic AND
               zcl_abapgit_objects_activation=>is_ddic_type( ls_item-obj_type ) = abap_false.
              " DDIC only for DDIC objects
              zcx_abapgit_exception=>raise( |Step { <lv_step_id> } is only for DDIC objects| ).
            ENDIF.
            APPEND INITIAL LINE TO <ls_step>-objects ASSIGNING <ls_deser>.
            <ls_deser>-item    = ls_item.
            <ls_deser>-files   = lo_files.
            <ls_deser>-obj     = li_obj.
            <ls_deser>-xml     = lo_xml.
            <ls_deser>-package = lv_package.
          ENDLOOP.

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = ls_item ).
          ii_log->add_error( iv_msg = |Import of object { ls_item-obj_name } failed|
                             is_item = ls_item ).
          "object should not be part of any deserialization step
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    li_progress->off( ).

    "run deserialize for all steps and its objects
    deserialize_steps(
      EXPORTING
        it_steps       = lt_steps
        ii_log         = ii_log
        io_i18n_params = lo_i18n_params
        iv_transport   = iv_transport
      CHANGING
        ct_files       = rt_accessed_files ).

    update_package_tree( iv_package ).

    " Set the original system for all updated objects to what's defined in repo settings
    update_original_system(
      it_items     = lt_items
      ii_log       = ii_log
      io_dot       = io_dot
      iv_transport = iv_transport ).

    zcl_abapgit_factory=>get_default_transport( )->reset( ).

    lo_timer->end( abap_true ).

  ENDMETHOD.


  METHOD deserialize_lxe.

    DATA:
      lo_base TYPE REF TO zcl_abapgit_objects_super,
      lx_exc  TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS <ls_obj> LIKE LINE OF is_step-objects.

    ii_log->add_success( |>> Step { is_step-order } - { is_step-descr }| ).

    LOOP AT is_step-objects ASSIGNING <ls_obj>.

      TRY.
          zcl_abapgit_factory=>get_lxe_texts( )->deserialize(
            iv_object_type = <ls_obj>-item-obj_type
            iv_object_name = <ls_obj>-item-obj_name
            iv_package     = <ls_obj>-item-devclass
            ii_xml         = <ls_obj>-xml
            io_files       = <ls_obj>-files
            io_i18n_params = io_i18n_params ).

          lo_base ?= <ls_obj>-obj.
          APPEND LINES OF lo_base->get_accessed_files( ) TO ct_files.

          ii_log->add_success( iv_msg  = |Translations for { <ls_obj>-item-obj_name } imported|
                               is_item = <ls_obj>-item ).

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc  = lx_exc
                                 is_item = <ls_obj>-item ).
          ii_log->add_error( iv_msg  = |Import of translations for { <ls_obj>-item-obj_name } failed|
                             is_item = <ls_obj>-item ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_step.

    DATA: li_progress TYPE REF TO zif_abapgit_progress,
          li_exit     TYPE REF TO zif_abapgit_exit,
          lo_base     TYPE REF TO zcl_abapgit_objects_super,
          lx_exc      TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS: <ls_obj> LIKE LINE OF is_step-objects.


    zcl_abapgit_objects_activation=>clear( ).

    ii_log->add_success( |>> Step { is_step-order } - { is_step-descr }| ).

    li_progress = zcl_abapgit_progress=>get_instance( lines( is_step-objects ) ).

    LOOP AT is_step-objects ASSIGNING <ls_obj>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Step { is_step-order } - { is_step-descr }:| &&
                     | { <ls_obj>-item-obj_type } { <ls_obj>-item-obj_name }| ).

      TRY.
          <ls_obj>-obj->deserialize( iv_package   = <ls_obj>-package
                                     io_xml       = <ls_obj>-xml
                                     iv_step      = is_step-step_id
                                     ii_log       = ii_log
                                     iv_transport = iv_transport ).

          lo_base ?= <ls_obj>-obj.
          APPEND LINES OF lo_base->get_accessed_files( ) TO ct_files.

          ii_log->add_success( iv_msg = |Object { <ls_obj>-item-obj_name } imported|
                               is_item = <ls_obj>-item ).

        CATCH zcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = <ls_obj>-item ).
          ii_log->add_error( iv_msg = |Import of object { <ls_obj>-item-obj_name } failed|
                             is_item = <ls_obj>-item ).
      ENDTRY.

    ENDLOOP.

    li_progress->show( iv_current = lines( is_step-objects )
                       iv_text    = |Step { is_step-order } - Activating Objects| ).

    CASE is_step-step_id.
      WHEN zif_abapgit_object=>gc_step_id-ddic.
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_true
          ii_log  = ii_log ).
      WHEN zif_abapgit_object=>gc_step_id-abap.
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_false
          ii_log  = ii_log ).
      WHEN zif_abapgit_object=>gc_step_id-late.
        " late can have both DDIC (like TABL with REF TO) and non-DDIC objects
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_true
          ii_log  = ii_log ).
        zcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_false
          ii_log  = ii_log ).
    ENDCASE.

    li_progress->off( ).

  ENDMETHOD.


  METHOD deserialize_steps.

    FIELD-SYMBOLS <ls_step> LIKE LINE OF it_steps.

    LOOP AT it_steps ASSIGNING <ls_step>.
      IF <ls_step>-step_id <> zif_abapgit_object=>gc_step_id-lxe.
        deserialize_step(
          EXPORTING
            is_step      = <ls_step>
            ii_log       = ii_log
            iv_transport = iv_transport
          CHANGING
            ct_files     = ct_files ).
      ELSEIF io_i18n_params->is_lxe_applicable( ) = abap_true.
        deserialize_lxe(
          EXPORTING
            is_step        = <ls_step>
            ii_log         = ii_log
            io_i18n_params = io_i18n_params
          CHANGING
            ct_files       = ct_files ).
      ENDIF.
    ENDLOOP.

    SORT ct_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM ct_files. " Just in case

  ENDMETHOD.


  METHOD determine_i18n_params.

    " TODO: unify with ZCL_ABAPGIT_SERIALIZE=>DETERMINE_I18N_PARAMS, same code

    IF io_dot IS BOUND.
      rs_i18n_params-main_language         = io_dot->get_main_language( ).
      rs_i18n_params-use_lxe               = io_dot->use_lxe( ).
      rs_i18n_params-main_language_only    = iv_main_language_only.
      rs_i18n_params-translation_languages = zcl_abapgit_lxe_texts=>get_translation_languages(
        iv_main_language  = io_dot->get_main_language( )
        it_i18n_languages = io_dot->get_i18n_languages( ) ).
    ENDIF.

    IF rs_i18n_params-main_language IS INITIAL.
      rs_i18n_params-main_language = sy-langu.
    ENDIF.

  ENDMETHOD.


  METHOD exists.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    " Might be called for objects without tadir entry
    IF is_item IS INITIAL.
      RETURN.
    ENDIF.

    " For unsupported objects, assume object exists
    IF is_type_supported( is_item-obj_type ) = abap_false.
      rv_bool = abap_true.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item ).
        rv_bool = li_obj->exists( ).
      CATCH zcx_abapgit_exception.
        " Ignore errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD files_to_deserialize.

    DATA:
      li_instance TYPE REF TO zif_abapgit_status_calc,
      lt_results  TYPE zif_abapgit_definitions=>ty_results_tt.

    li_instance = zcl_abapgit_status_calc=>get_instance(
      iv_root_package = iv_package
      io_dot          = io_dot ).

    lt_results = li_instance->calculate_status(
      it_local     = it_local
      it_remote    = it_remote
      it_cur_state = it_local_checksums ).

    rt_results = prioritize_deser(
                   filter_files_to_deserialize(
                     it_results = lt_results
                     ii_log     = ii_log ) ).

  ENDMETHOD.


  METHOD filter_files_to_deserialize.

    DATA lt_objects LIKE rt_results.
    DATA lr_object  TYPE REF TO zif_abapgit_definitions=>ty_result.
    DATA ls_item    TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_tabix   TYPE sy-tabix.

    rt_results = it_results.

    "preparation for object logging, sort all file entries by objects
    IF ii_log IS BOUND.
      lt_objects = rt_results.
      SORT lt_objects
        BY obj_type
           obj_name.
      DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING obj_type obj_name.
      DELETE lt_objects WHERE obj_type IS INITIAL AND obj_name IS INITIAL.
    ENDIF.

    "ignore objects w/o changes
    DELETE rt_results WHERE match = abap_true.     " Full match
    "log objects w/o changes
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts of the objects have not changed
          ls_item-devclass = lr_object->package.
          ls_item-obj_type = lr_object->obj_type.
          ls_item-obj_name = lr_object->obj_name.
          ii_log->add_success(
            iv_msg  = |Object { ls_item-obj_name } (type { ls_item-obj_type }) not changed; no import required|
            is_item = ls_item ).
          "ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore objects w/o object type
    DELETE rt_results WHERE obj_type IS INITIAL.
    "log objects w/o object type
    IF sy-subrc = 0 AND ii_log IS BOUND.
      " Note: Moving the CHECK condition to the LOOP WHERE clause will lead to a
      " syntax warning in higher releases and syntax error in 702
      LOOP AT lt_objects REFERENCE INTO lr_object.
        CHECK lr_object->obj_type IS INITIAL AND lr_object->obj_name IS NOT INITIAL.
        ls_item-devclass = lr_object->package.
        ls_item-obj_type = lr_object->obj_type.
        ls_item-obj_name = lr_object->obj_name.
        ii_log->add_warning(
          iv_msg  = |Object type for { ls_item-obj_name } not defined - will be ignored by abapGit|
          is_item = ls_item ).
      ENDLOOP.
      DELETE lt_objects WHERE obj_type IS INITIAL.
    ENDIF.

    "ignore objects that exists only local
    DELETE rt_results WHERE lstate = zif_abapgit_definitions=>c_state-added AND rstate IS INITIAL.
    "ignore objects that where deleted remotely
    DELETE rt_results WHERE rstate = zif_abapgit_definitions=>c_state-deleted.
    "log objects that exists only local or where deleted remotely
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts exists only local
          "no log message; ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore table content
    " DELETE rt_results WHERE path = zif_abapgit_data_config=>c_default_path

    SORT rt_results
      BY obj_type ASCENDING
         obj_name ASCENDING
         rstate   DESCENDING  " ensures that non-empty rstate is kept
         lstate   DESCENDING. " ensures that non-empty lstate is kept
    DELETE ADJACENT DUPLICATES FROM rt_results COMPARING obj_type obj_name.

  ENDMETHOD.


  METHOD get_deserialize_steps.
    FIELD-SYMBOLS: <ls_step>    TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-early.
    <ls_step>-descr        = 'Pre-process Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 1.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-ddic.
    <ls_step>-descr        = 'Deserialize DDIC Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 2.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-abap.
    <ls_step>-descr        = 'Deserialize non-DDIC Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 3.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = zif_abapgit_object=>gc_step_id-late.
    <ls_step>-descr        = 'Post-process Objects'.
    <ls_step>-syntax_check = abap_true.
    <ls_step>-order        = 4.

    SORT rt_steps BY order. " ensure correct processing order
  ENDMETHOD.


  METHOD get_extra_from_filename.

    IF iv_filename IS NOT INITIAL.
      FIND REGEX '\..*\.([\-a-z0-9_%]*)\.' IN iv_filename SUBMATCHES rv_extra.
      IF sy-subrc = 0.
        rv_extra = cl_http_utility=>unescape_url( rv_extra ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_active.

    DATA: li_obj TYPE REF TO zif_abapgit_object.

    " For unsupported objects, assume active state
    IF is_type_supported( is_item-obj_type ) = abap_false.
      rv_active = abap_true.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item ).
        rv_active = li_obj->is_active( ).
      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_ref_is_initial
            zcx_abapgit_exception.
        " Ignore errors and assume active state
        rv_active = abap_true.
    ENDTRY.

  ENDMETHOD.


  METHOD is_supported.

    TRY.
        create_object(
          is_item        = is_item
          iv_native_only = iv_native_only ).
        rv_bool = abap_true.
      CATCH zcx_abapgit_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD is_type_supported.

    DATA: ls_item               TYPE zif_abapgit_definitions=>ty_item,
          ls_supported_obj_type TYPE ty_supported_types.

    FIELD-SYMBOLS <ls_supported_obj_type> TYPE ty_supported_types.

    IF iv_obj_type IS INITIAL.
      " empty object type should never exist
      RETURN.
    ENDIF.

    READ TABLE gt_supported_obj_types
      ASSIGNING <ls_supported_obj_type>
      WITH KEY obj_type = iv_obj_type.

    IF sy-subrc <> 0.

      ls_item-obj_type = iv_obj_type.

      ls_supported_obj_type-obj_type  = iv_obj_type.
      ls_supported_obj_type-supported = is_supported( ls_item ).

      INSERT ls_supported_obj_type INTO TABLE gt_supported_obj_types.

      rv_bool = ls_supported_obj_type-supported.
      RETURN.

    ENDIF.

    rv_bool = <ls_supported_obj_type>-supported.

  ENDMETHOD.


  METHOD jump.

    DATA: li_obj  TYPE REF TO zif_abapgit_object,
          lv_exit TYPE abap_bool.

    " Nothing to do for unsupported objects
    IF is_type_supported( is_item-obj_type ) = abap_false.
      zcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDIF.

    " Nothing to do if object does not exist
    li_obj = create_object( is_item ).

    IF li_obj->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Object { is_item-obj_type } { is_item-obj_name } doesn't exist| ).
    ENDIF.

    " First priority object-specific handler
    lv_exit = li_obj->jump( get_extra_from_filename( iv_filename ) ).

    IF lv_exit = abap_false.
      " Open object in new window with generic jumper
      lv_exit = zcl_abapgit_objects_factory=>get_gui_jumper( )->jump(
        is_item        = is_item
        is_sub_item    = is_sub_item
        iv_line_number = iv_line_number
        iv_new_window  = iv_new_window ).
    ENDIF.

    IF lv_exit = abap_false.
      zcx_abapgit_exception=>raise( |Jump to { is_item-obj_type } { is_item-obj_name } not possible| ).
    ENDIF.

  ENDMETHOD.


  METHOD map_results_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_result> TYPE zif_abapgit_definitions=>ty_result.

    LOOP AT it_results ASSIGNING <ls_result>.

      ls_item-devclass = <ls_result>-package.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.


  METHOD map_tadir_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_tadir> TYPE zif_abapgit_definitions=>ty_tadir.

    LOOP AT it_tadir ASSIGNING <ls_tadir>.

      ls_item-devclass = <ls_tadir>-devclass.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.


  METHOD prioritize_deser.

    DATA lt_items    TYPE zif_abapgit_definitions=>ty_items_tt.
    DATA ls_item     LIKE LINE OF lt_items.
    DATA lt_requires TYPE zif_abapgit_definitions=>ty_items_tt.
    DATA ls_require  LIKE LINE OF lt_requires.
    DATA ls_result   LIKE LINE OF it_results.
    DATA lo_graph    TYPE REF TO zcl_abapgit_item_graph.

    lt_items = map_results_to_items( it_results ).

    CREATE OBJECT lo_graph EXPORTING it_items = lt_items.

    LOOP AT lt_items INTO ls_item.
      CLEAR lt_requires.

* TODO: BEGIN extract to object handler method in ZIF_ABAPGIT_OBJECT:
*    METHODS get_deserialize_order
*      IMPORTING
*        it_items TYPE ty_items_tt
*      RETURNING
*        VALUE(rt_requries) TYPE ty_items_tt

      CASE ls_item-obj_type.
        WHEN 'SPRX'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'WEBI'.
        WHEN 'CLAS'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'
            AND obj_type <> 'INTF'
            AND obj_type <> 'XSLT'.
        WHEN 'PROG'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'XSLT'.
        WHEN 'INTF'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'
            AND obj_type <> 'XSLT'.
        WHEN 'TABL'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'.
        WHEN 'IARP'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IASP'.
        WHEN 'IATU' OR 'IAXU' OR 'IAMU'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IASP'
            AND obj_type <> 'PROG'
            AND obj_type <> 'IARP'.
        WHEN 'IDOC' OR 'IEXT'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'TABL'.
        WHEN 'DCLS'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'DDLS'.
        WHEN 'ODSO'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IOBJ'.
        WHEN 'SCP1'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'TOBJ'.
        WHEN 'CHAR'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'OTGR'.
        WHEN 'PINF'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'CLAS'
            AND obj_type <> 'INTF'
            AND obj_type <> 'TABL'
            AND obj_type <> 'DOMA'
            AND obj_type <> 'DTEL'.
        WHEN 'DEVC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'PINF'.
        WHEN 'ENHC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENHO'.
        WHEN 'ENHO'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENSC' AND obj_type <> 'ENHS'.
        WHEN 'ENSC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENHS'.
        WHEN 'IWMO' OR 'IWSV' OR 'IWVB'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SRVB'.
        WHEN 'SUSH'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SRVB' AND obj_type <> 'HTTP'.
        WHEN 'SRVB'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SRVD'.
      ENDCASE.
* TODO: END extract to object handler method

      LOOP AT lt_requires INTO ls_require.
        lo_graph->add_edge(
          is_from = ls_require
          is_to   = ls_item ).
      ENDLOOP.
    ENDLOOP.

    WHILE lo_graph->has_vertices( ) = abap_true.
      ls_item = lo_graph->get_next( ii_log ).
      READ TABLE it_results INTO ls_result WITH KEY sec_key COMPONENTS
        obj_name = ls_item-obj_name
        obj_type = ls_item-obj_type.
      ASSERT sy-subrc = 0.
      APPEND ls_result TO rt_results.
    ENDWHILE.

  ENDMETHOD.


  METHOD serialize.

    DATA: li_obj   TYPE REF TO zif_abapgit_object,
          lx_error TYPE REF TO zcx_abapgit_exception,
          li_xml   TYPE REF TO zif_abapgit_xml_output,
          lo_files TYPE REF TO zcl_abapgit_objects_files.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF rs_files_and_item-files.

    IF is_type_supported( is_item-obj_type ) = abap_false.
      zcx_abapgit_exception=>raise( |Object type ignored, not supported: {
        is_item-obj_type }-{
        is_item-obj_name }| ).
    ENDIF.

    lo_files = zcl_abapgit_objects_files=>new( is_item ).

    li_obj = create_object(
      is_item        = is_item
      io_files       = lo_files
      io_i18n_params = io_i18n_params ).

    CREATE OBJECT li_xml TYPE zcl_abapgit_xml_output.

    rs_files_and_item-item = is_item.

    TRY.
        li_obj->serialize( li_xml ).
      CATCH zcx_abapgit_exception INTO lx_error.
        rs_files_and_item-item-inactive = boolc( li_obj->is_active( ) = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    IF io_i18n_params->is_lxe_applicable( ) = abap_true.
      zcl_abapgit_factory=>get_lxe_texts( )->serialize(
        iv_object_type = is_item-obj_type
        iv_object_name = is_item-obj_name
        io_i18n_params = io_i18n_params
        io_files       = lo_files
        ii_xml         = li_xml ).
    ENDIF.

    IF lo_files->is_json_metadata( ) = abap_false.
      lo_files->add_xml(
        ii_xml      = li_xml
        is_metadata = li_obj->get_metadata( ) ).
    ENDIF.

    rs_files_and_item-files = lo_files->get_files( ).

    " TODO: Do we need this?
    " check_duplicates( rs_files_and_item-files ).

    rs_files_and_item-item-inactive = boolc( li_obj->is_active( ) = abap_false ).

    LOOP AT rs_files_and_item-files ASSIGNING <ls_file>.
      <ls_file>-sha1 = zcl_abapgit_hash=>sha1_blob( <ls_file>-data ).
    ENDLOOP.

  ENDMETHOD.


  METHOD supported_list.

    DATA lt_objects            TYPE STANDARD TABLE OF ko100.
    DATA ls_item               TYPE zif_abapgit_definitions=>ty_item.
    DATA ls_supported_obj_type TYPE ty_supported_types.
    DATA lt_types              TYPE zif_abapgit_exit=>ty_object_types.
    DATA lv_type               LIKE LINE OF lt_types.
    DATA li_exit               TYPE REF TO zif_abapgit_exit.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.
    FIELD-SYMBOLS <ls_supported_obj_type> TYPE ty_supported_types.

    IF gv_supported_obj_types_loaded = abap_true.
      LOOP AT gt_supported_obj_types ASSIGNING <ls_supported_obj_type> WHERE supported = abap_true.
        INSERT <ls_supported_obj_type>-obj_type INTO TABLE rt_types.
      ENDLOOP.
      RETURN.
    ENDIF.

    " delete content because it might be filled already by method IS_TYPE_SUPPORTED
    CLEAR gt_supported_obj_types.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_objects
      EXCEPTIONS
        OTHERS         = 1 ##FM_SUBRC_OK.

    LOOP AT lt_objects ASSIGNING <ls_object> WHERE pgmid = 'R3TR'.
      INSERT <ls_object>-object INTO TABLE lt_types.
    ENDLOOP.

    LOOP AT lt_types INTO lv_type.
      ls_item-obj_type = lv_type.

      ls_supported_obj_type-obj_type  = lv_type.
      ls_supported_obj_type-supported = is_supported( ls_item ).

      INSERT ls_supported_obj_type INTO TABLE gt_supported_obj_types.

      IF ls_supported_obj_type-supported = abap_true.
        INSERT ls_supported_obj_type-obj_type INTO TABLE rt_types.
      ENDIF.
    ENDLOOP.

    gv_supported_obj_types_loaded = abap_true.

  ENDMETHOD.


  METHOD update_original_system.

    DATA:
      lv_srcsystem           TYPE tadir-srcsystem,
      lv_transport_type_from TYPE trfunction,
      lv_transport_type_to   TYPE trfunction,
      lv_errors              TYPE abap_bool,
      lv_msg                 TYPE string.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_items.

    lv_srcsystem = io_dot->get_original_system( ).

    IF lv_srcsystem IS INITIAL.
      RETURN.
    ELSEIF lv_srcsystem = 'SID'.
      " Change objects to local system and switch repairs to development requests
      lv_srcsystem           = sy-sysid.
      lv_transport_type_from = zif_abapgit_cts_api=>c_transport_type-wb_repair.
      lv_transport_type_to   = zif_abapgit_cts_api=>c_transport_type-wb_task.
    ELSE.
      " Change objects to external system and switch development requests to repairs
      lv_transport_type_from = zif_abapgit_cts_api=>c_transport_type-wb_task.
      lv_transport_type_to   = zif_abapgit_cts_api=>c_transport_type-wb_repair.
    ENDIF.

    ii_log->add_info( |>> Setting original system| ).

    LOOP AT it_items ASSIGNING <ls_item>.
      " Local packages are not stored in TADIR
      IF <ls_item>-obj_type = 'DEVC' AND <ls_item>-obj_name(1) = '$'.
        CONTINUE.
      ENDIF.
      IF exists( <ls_item> ) = abap_true.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_tadir_pgmid                 = 'R3TR'
            wi_tadir_object                = <ls_item>-obj_type
            wi_tadir_obj_name              = <ls_item>-obj_name
            wi_tadir_srcsystem             = lv_srcsystem
            wi_test_modus                  = abap_false
          EXCEPTIONS
            tadir_entry_not_existing       = 1
            tadir_entry_ill_type           = 2
            no_systemname                  = 3
            no_systemtype                  = 4
            original_system_conflict       = 5
            object_reserved_for_devclass   = 6
            object_exists_global           = 7
            object_exists_local            = 8
            object_is_distributed          = 9
            obj_specification_not_unique   = 10
            no_authorization_to_delete     = 11
            devclass_not_existing          = 12
            simultanious_set_remove_repair = 13
            order_missing                  = 14
            no_modification_of_head_syst   = 15
            pgmid_object_not_allowed       = 16
            masterlanguage_not_specified   = 17
            devclass_not_specified         = 18
            specify_owner_unique           = 19
            loc_priv_objs_no_repair        = 20
            gtadir_not_reached             = 21
            object_locked_for_order        = 22
            change_of_class_not_allowed    = 23
            no_change_from_sap_to_tmp      = 24
            OTHERS                         = 25.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
          ii_log->add_error(
            iv_msg  = lv_msg
            is_item = <ls_item> ).
          lv_errors = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_errors IS INITIAL.
      " Since original system has changed, the type of transport request needs to be adjusted
      zcl_abapgit_factory=>get_cts_api( )->change_transport_type(
        iv_transport_request   = iv_transport
        iv_transport_type_from = lv_transport_type_from
        iv_transport_type_to   = lv_transport_type_to ).
    ENDIF.

  ENDMETHOD.


  METHOD update_package_tree.

    DATA: lt_packages TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.

    " Make sure all deserialized objects are committed
    COMMIT WORK AND WAIT.

    lt_packages = zcl_abapgit_factory=>get_sap_package( iv_package )->list_subpackages( ).
    APPEND iv_package TO lt_packages.

    LOOP AT lt_packages INTO lv_package.
      " Update package tree for SE80
      lv_tree = 'EU_' && lv_package.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = lv_tree
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
