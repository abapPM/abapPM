CLASS /apmg/cl_apm_gui_dlg_update DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Update Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_menu_provider,
      /apmg/if_apm_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !package      TYPE devclass OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !package TYPE devclass OPTIONAL
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_params,
        package    TYPE devclass,
        name       TYPE string,
        version    TYPE string,
        to_version TYPE string,
        transport  TYPE trkorr,
        force      TYPE abap_bool,
      END OF ty_params.

    CONSTANTS:
      BEGIN OF c_id,
        package    TYPE string VALUE 'package',
        name       TYPE string VALUE 'name',
        version    TYPE string VALUE 'version',
        to_version TYPE string VALUE 'to-version',
        transport  TYPE string VALUE 'transport',
        force      TYPE string VALUE 'force',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_action,
        update_package   TYPE string VALUE 'update-package',
        choose_transport TYPE string VALUE 'choose-transport',
      END OF c_action .

    DATA:
      registry       TYPE string,
      update_package TYPE devclass,
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

    METHODS read_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_string_map
      RAISING
        /apmg/cx_apm_error.

    METHODS validate_form
      IMPORTING
        !form_data    TYPE REF TO /apmg/cl_apm_string_map
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_string_map
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_dlg_update IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    form_data = form_util->normalize_abapgit( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_action-choose_transport.

        form_data->set(
          iv_key = c_id-transport
          iv_val = /apmg/cl_apm_gui_factory=>get_popups( )->popup_to_select_transport( ) ).

        IF form_data->get( c_id-transport ) IS NOT INITIAL.
          validation_log = validate_form( form_data ).
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_action-update_package.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          DATA(params) = get_parameters( form_data ).

          /apmg/cl_apm_command_update=>run(
            registry   = registry
            package    = params-package
            to_version = params-to_version
            transport  = params-transport
            force      = params-force ).

          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-go_back.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_menu_provider~get_menu.

    ro_toolbar = /apmg/cl_apm_gui_menus=>registry( registry ).

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

    update_package = package.
    IF update_package IS NOT INITIAL.
      form_data = read_package( update_package ).
    ENDIF.

    registry = /apmg/cl_apm_settings=>factory( )->get( )-registry.

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_update( package ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = 'Update Package'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'update-package-form'
      iv_help_page = 'https://docs.abappm.com/' ). " TODO

    result->text(
      iv_name        = c_id-package
      iv_readonly    = abap_true
      iv_label       = 'Package'
    )->text(
      iv_name        = c_id-name
      iv_readonly    = abap_true
      iv_label       = 'Name'
    )->text(
      iv_name        = c_id-version
      iv_readonly    = abap_true
      iv_label       = 'Current Version'
    )->text(
      iv_name        = c_id-to_version
      iv_label       = 'Target Version'
    )->text(
      iv_name        = c_id-transport
      iv_side_action = c_action-choose_transport
      iv_label       = 'Transport'
      iv_upper_case  = abap_true
      iv_max         = 20
    )->checkbox(
      iv_name        = c_id-force
      iv_label       = 'Force'
      iv_hint        = 'Overwrite already installed dependencies' ).

    result->command(
      iv_label       = 'Update Package'
      iv_cmd_type    = /apmg/if_apm_html_form=>c_cmd_type-input_main
      iv_action      = c_action-update_package
    )->command(
      iv_label       = 'Back'
      iv_action      = /apmg/if_apm_gui_router=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_parameters.

    form_data->to_struc( CHANGING cs_container = result ).

  ENDMETHOD.


  METHOD read_package.

    DATA(package_json) = /apmg/cl_apm_package_json=>factory( package )->get( ).

    DATA(latest) = /apmg/cl_apm_registry=>get_latest_version(
      registry = registry
      name     = package_json-name ).

    result = NEW #( ).

    result->set(
      iv_key = c_id-package
      iv_val = package
    )->set(
      iv_key = c_id-name
      iv_val = package_json-name
    )->set(
      iv_key = c_id-version
      iv_val = package_json-version
    )->set(
      iv_key = c_id-to_version
      iv_val = latest ).

  ENDMETHOD.


  METHOD validate_form.

    result = form_util->validate( form_data ).

    DATA(package) = CONV devclass( form_data->get( c_id-package ) ).

    DATA(msg) = /apmg/cl_apm_auth=>check_package_allowed( package ).
    IF msg IS NOT INITIAL.
      result->set(
        iv_key = c_id-package
        iv_val = msg ).
    ENDIF.

    DATA(transport) = CONV trkorr( form_data->get( c_id-transport ) ).

    IF transport IS INITIAL.
      msg = /apmg/cl_apm_auth=>check_transport_required( package ).
      IF msg IS NOT INITIAL.
        result->set(
          iv_key = c_id-transport
          iv_val = msg ).
      ENDIF.
    ENDIF.

    IF NOT /apmg/cl_apm_package_json_vali=>is_valid_version( form_data->get( c_id-to_version ) ).
      result->set(
        iv_key = c_id-to_version
        iv_val = 'Invalid version' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
