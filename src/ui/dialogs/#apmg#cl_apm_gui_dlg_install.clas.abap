CLASS /apmg/cl_apm_gui_dlg_install DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Install Command
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
        package_json TYPE /apmg/if_apm_types=>ty_package_json,
      END OF ty_params.

    CONSTANTS:
      BEGIN OF c_id,
        package TYPE string VALUE 'package',
        name    TYPE string VALUE 'name',
        version TYPE string VALUE 'version',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_action,
        choose_package  TYPE string VALUE 'choose-package',
        create_package  TYPE string VALUE 'create-package',
        install_package TYPE string VALUE 'install-package',
      END OF c_action .

    DATA:
      registry       TYPE string,
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

ENDCLASS.



CLASS /apmg/cl_apm_gui_dlg_install IMPLEMENTATION.


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

      WHEN c_action-install_package.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          DATA(params) = get_parameters( form_data ).

          /apmg/cl_apm_command_install=>run(
            registry     = registry
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


  METHOD constructor.

    super->constructor( ).

    validation_log = NEW #( ).
    form_data      = NEW #( ).
    form           = get_form_schema( ).
    form_util      = /apmg/cl_apm_html_form_utils=>create( form ).

    registry = /apmg/cl_apm_settings=>factory( )->get( )-registry.

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_install( ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = 'Install Package'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'install-package-form'
      iv_help_page = 'https://docs.abappm.com/' ). " TODO

    result->text(
      iv_name        = c_id-package
      iv_side_action = c_action-choose_package
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package (should be a dedicated one)'
      iv_placeholder = 'Z... / $...'
      iv_min         = 2
      iv_max         = 30
    )->text(
      iv_name        = c_id-name
      iv_required    = abap_true
      iv_label       = 'Name'
      iv_hint        = 'Name of the package'
      iv_min         = /apmg/if_apm_types=>c_package_name-min_length
      iv_max         = /apmg/if_apm_types=>c_package_name-max_length
    )->text(
      iv_name        = c_id-version
      iv_required    = abap_true
      iv_label       = 'Version'
      iv_hint        = 'Semantic version (x.y.z)' ).

    result->command(
      iv_label       = 'Install Package'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_action-install_package
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
            zcx_abapgit_exception=>raise( |Package { package } not allowed, responsible user = 'SAP'| ).
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

  ENDMETHOD.
ENDCLASS.
