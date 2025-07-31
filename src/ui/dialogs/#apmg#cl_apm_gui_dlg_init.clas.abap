CLASS /apmg/cl_apm_gui_dlg_init DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Init Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_renderable.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_params,
        package      TYPE devclass,
        name         TYPE string,
        version      TYPE string,
        description  TYPE string,
        private      TYPE abap_bool,
        labels       TYPE string,
        package_json TYPE /apmg/if_apm_types=>ty_package_json,
      END OF ty_params.

    CONSTANTS:
      BEGIN OF c_id,
        package     TYPE string VALUE 'package',
        name        TYPE string VALUE 'name',
        version     TYPE string VALUE 'version',
        description TYPE string VALUE 'description',
        private     TYPE string VALUE 'private',
        labels      TYPE string VALUE 'labels',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_action,
        choose_package TYPE string VALUE 'choose-package',
        choose_labels  TYPE string VALUE 'choose-labels',
        create_package TYPE string VALUE 'create-package',
        init_package   TYPE string VALUE 'init-package',
      END OF c_action .

    DATA:
      form           TYPE REF TO /apmg/cl_apm_html_form,
      form_data      TYPE REF TO /apmg/cl_apm_string_map,
      form_util      TYPE REF TO /apmg/cl_apm_html_form_utils,
      validation_log TYPE REF TO /apmg/cl_apm_string_map.

    METHODS get_form_schema
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_form.

    METHODS get_parameters
      IMPORTING
        !form_data    TYPE REF TO /apmg/cl_apm_string_map
      RETURNING
        VALUE(result) TYPE ty_params.

    METHODS validate_form
      IMPORTING
        !form_data    TYPE REF TO /apmg/cl_apm_string_map
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_string_map
      RAISING
        /apmg/cx_apm_error.

    METHODS choose_labels
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_dlg_init IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    form_data = form_util->normalize_abapgit( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_action-create_package.

        form_data->set(
          iv_key = c_id-package
          iv_val = /apmg/cl_apm_popup_utils=>create_package( form_data->get( c_id-package ) ) ).

        IF form_data->get( c_id-package ) IS NOT INITIAL.
          validation_log = validate_form( form_data ).
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_action-choose_package.

        form_data->set(
          iv_key = c_id-package
          iv_val = /apmg/cl_apm_gui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).

        IF form_data->get( c_id-package ) IS NOT INITIAL.
          validation_log = validate_form( form_data ).
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_action-choose_labels.

        choose_labels( ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-init_package.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          DATA(params) = get_parameters( form_data ).

          /apmg/cl_apm_command_init=>run(
            package      = params-package
            package_json = params-package_json ).

          rs_handled-page  = /apmg/cl_apm_gui_page_package=>create( params-package ).
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-new_page_replacing.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->add( '<div class="form-container">' ).
    html->add( form->render(
      io_values         = form_data
      io_validation_log = validation_log ) ).
    html->add( '</div>' ).

    ri_html = html.

  ENDMETHOD.


  METHOD choose_labels.

    ASSERT 0 = 0.
    " FUTURE
*    DATA(old_labels) = form_data->get( c_id-labels ).
*
*    DATA(new_labels) = /apmg/cl_apm_ui_factory=>get_popups( )->popup_to_select_labels( old_labels ).
*
*    form_data->set(
*      iv_key = c_id-labels
*      iv_val = new_labels ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    validation_log = NEW #( ).
    form_data      = NEW #( ).
    form           = get_form_schema( ).
    form_util      = /apmg/cl_apm_html_form_utils=>create( form ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_init( ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = 'Init Package'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'init-package-form'
      iv_help_page = 'https://docs.abappm.com/' ). " TODO

    result->text(
      iv_name        = c_id-package
      iv_side_action = c_action-choose_package
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package (should be a dedicated one)'
      iv_placeholder = 'Z... / $...'
      iv_max         = 30
    )->text(
      iv_name        = c_id-name
      iv_required    = abap_true
      iv_label       = 'Name'
      iv_hint        = 'Unique name for package'
      iv_min         = /apmg/if_apm_types=>c_package_name-min_length
      iv_max         = /apmg/if_apm_types=>c_package_name-max_length
    )->text(
      iv_name        = c_id-version
      iv_required    = abap_true
      iv_label       = 'Version'
      iv_hint        = 'Semantic version (x.y.z)'
    )->text(
      iv_name        = c_id-description
      iv_label       = 'Description'
* FUTURE
*    )->text(
*      iv_name        = c_id-labels
*      iv_side_action = c_action-choose_labels
*      iv_label       = |Labels (comma-separated, allowed chars: "{ /apmg/cl_apm_repo_labels=>c_allowed_chars }")|
*      iv_hint        = 'Comma-separated labels for grouping and repo organization (optional)'
    )->checkbox(
      iv_name        = c_id-private
      iv_label       = 'Private Package (will not become public)' ).

* FUTURE
*    IF /apmg/cl_apm_feature=>is_enabled( /apmg/cl_apm_abap_language_vers=>c_feature_flag ) = abap_true.
*      result->radio(
*        iv_name        = c_id-abap_lang_vers
*        iv_default_value = ''
*        iv_label       = 'ABAP Language Version'
*        iv_hint        = 'Define the ABAP language version for objects in the repository'
*      )->option(
*        iv_label       = 'Any'
*        iv_value       = ''
*      )->option(
*        iv_label       = 'Ignore'
*        iv_value       = zif_abapgit_dot_abapgit=>c_abap_language_version-ignore
*      )->option(
*        iv_label       = 'Standard'
*        iv_value       = zif_abapgit_dot_abapgit=>c_abap_language_version-standard
*      )->option(
*        iv_label       = 'For Key Users'
*        iv_value       = zif_abapgit_dot_abapgit=>c_abap_language_version-key_user
*      )->option(
*        iv_label       = 'For Cloud Development'
*        iv_value       = zif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development )
*    ENDIF

    result->command(
      iv_label       = 'Init Package'
      iv_cmd_type    = /apmg/if_apm_html_form=>c_cmd_type-input_main
      iv_action      = c_action-init_package
    )->command(
      iv_label       = 'Create Package'
      iv_action      = c_action-create_package
    )->command(
      iv_label       = 'Back'
      iv_action      = /apmg/if_apm_gui_router=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_parameters.

    form_data->to_struc( CHANGING cs_container = result ).

    result-package_json = CORRESPONDING #( result ).

  ENDMETHOD.


  METHOD validate_form.

    result = form_util->validate( form_data ).

    DATA(package) = CONV devclass( form_data->get( c_id-package ) ).
    IF package IS NOT INITIAL.
      TRY.
          zcl_abapgit_factory=>get_sap_package( package )->validate_name( ).

          " Check if package owned by SAP is allowed (new packages are ok, since they are created automatically)
          DATA(username) = zcl_abapgit_factory=>get_sap_package( package )->read_responsible( ).

          IF sy-subrc = 0 AND username = 'SAP' AND
            zcl_abapgit_factory=>get_environment( )->is_sap_object_allowed( ) = abap_false.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
              EXPORTING
                text = |Package { package } not allowed, responsible user = 'SAP'|.
          ENDIF.
        CATCH zcx_abapgit_exception INTO DATA(error).
          result->set(
            iv_key = c_id-package
            iv_val = error->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF /apmg/cl_apm_package_json_vali=>is_valid_name( form_data->get( c_id-name ) ) = abap_false.
      result->set(
        iv_key = c_id-name
        iv_val = 'Invalid name' ).
    ENDIF.

    IF /apmg/cl_apm_package_json_vali=>is_valid_version( form_data->get( c_id-version ) ) = abap_false.
      result->set(
        iv_key = c_id-version
        iv_val = 'Invalid version' ).
    ENDIF.

    " FUTURE
    " TRY
    "     /apmg/cl_apm_repo_labels=>validate( form_data->get( c_id-labels ) )
    "   CATCH zcx_abapgit_exception INTO error
    "     result->set(
    "       iv_key = c_id-labels
    "       iv_val = error->get_text( ) )
    " ENDTRY

  ENDMETHOD.
ENDCLASS.
