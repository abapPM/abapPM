CLASS /apmg/cl_apm_gui_dlg_undepreca DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Undeprecate Command
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
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_params,
        package TYPE devclass,
        name    TYPE string,
        version TYPE string,
      END OF ty_params.

    CONSTANTS:
      BEGIN OF c_id,
        package TYPE string VALUE 'package',
        name    TYPE string VALUE 'name',
        version TYPE string VALUE 'version',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_action,
        choose_package     TYPE string VALUE 'choose-package',
        confirm_undeprecate TYPE string VALUE 'confirm-undeprecate',
        cancel_confirm     TYPE string VALUE 'cancel-confirm',
        undeprecate        TYPE string VALUE 'undeprecate',
      END OF c_action.

    DATA:
      registry            TYPE string,
      undeprecate_package TYPE devclass,
      form                TYPE REF TO /apmg/cl_apm_html_form,
      form_data           TYPE REF TO /apmg/cl_apm_string_map,
      form_util           TYPE REF TO /apmg/cl_apm_html_form_utils,
      validation_log      TYPE REF TO /apmg/cl_apm_string_map,
      confirm_popup       TYPE REF TO /apmg/cl_apm_popup_to_confirm,
      pending_params      TYPE ty_params.

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

    METHODS read_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_string_map
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_dlg_undepreca IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-confirm_undeprecate.

        confirm_popup->close( ).
        /apmg/cl_apm_command_deprecate=>run(
          registry     = registry
          name         = pending_params-name
          range        = pending_params-version
          message_text = '' ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-go_back.

      WHEN c_action-cancel_confirm.

        confirm_popup->close( ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN /apmg/if_apm_gui_router=>c_action-go_back.

        IF confirm_popup IS BOUND.
          IF confirm_popup->was_closed( ) = abap_false.
            confirm_popup->close( ).
            rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
          ENDIF.
        ENDIF.

      WHEN OTHERS.

        form_data = form_util->normalize_abapgit( ii_event->form_data( ) ).

        CASE ii_event->mv_action.
          WHEN c_action-choose_package.

            form_data->set(
              iv_key = c_id-package
              iv_val = /apmg/cl_apm_gui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).

            IF form_data->get( c_id-package ) IS NOT INITIAL.
              validation_log = validate_form( form_data ).
            ELSE.
              form_data = read_package( |{ form_data->get( c_id-package ) }| ).
            ENDIF.
            rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

          WHEN c_action-undeprecate.

            validation_log = validate_form( form_data ).

            IF validation_log->is_empty( ) = abap_true.
              DATA(params) = get_parameters( form_data ).

              pending_params = params.
              confirm_popup = /apmg/cl_apm_popup_to_confirm=>create(
                titlebar              = 'Undeprecate Version'
                text_question         = |This will UNDEPRECATE package "{ params-name }" { params-version }|
                text_button_1         = 'Undeprecate'
                action_button_1       = c_action-confirm_undeprecate
                text_button_2         = 'Cancel'
                action_button_2       = c_action-cancel_confirm
                default_button        = '2'
                display_cancel_button = abap_false
                popup_type            = 'ICON_MESSAGE_WARNING' ).
            ENDIF.

            rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

        ENDCASE.

    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_menu_provider~get_menu.

    ro_toolbar = /apmg/cl_apm_gui_menus=>registry( registry ).

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    registry = /apmg/cl_apm_settings=>factory( )->get( )-registry.

    register_handlers( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->add( '<div class="form-container">' ).
    html->add( form->render(
      io_values         = form_data
      io_validation_log = validation_log ) ).
    html->add( '</div>' ).

    IF confirm_popup IS BOUND AND confirm_popup->was_closed( ) = abap_false.
      html->add( /apmg/cl_apm_gui_in_page_modal=>create( confirm_popup ) ).
    ENDIF.

    ri_html = html.

  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).

    validation_log = NEW #( ).
    form_data      = NEW #( ).
    form           = get_form_schema( ).
    form_util      = /apmg/cl_apm_html_form_utils=>create( form ).

    undeprecate_package = package.
    IF undeprecate_package IS NOT INITIAL.
      form_data = read_package( undeprecate_package ).
    ENDIF.

    registry = /apmg/cl_apm_settings=>factory( )->get( )-registry.

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_undepreca( package ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = 'Undeprecate Package'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'undeprecate-package-form'
      iv_help_page = 'https://docs.abappm.com/' ). " TODO

    result->text(
      iv_name  = c_id-name
      iv_label = 'Name'
    )->text(
      iv_name  = c_id-version
      iv_label = 'Version or Range of Versions' ).

    result->command(
      iv_label    = 'Undeprecate'
      iv_cmd_type = /apmg/if_apm_html_form=>c_cmd_type-input_main
      iv_action   = c_action-undeprecate
    )->command(
      iv_label  = 'Back'
      iv_action = /apmg/if_apm_gui_router=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_parameters.

    form_data->to_struc( CHANGING cs_container = result ).

  ENDMETHOD.


  METHOD read_package.

    DATA(package_json) = /apmg/cl_apm_package_json=>factory( package )->get( ).

    result = NEW #( ).

    result->set(
      iv_key = c_id-package
      iv_val = package
    )->set(
      iv_key = c_id-name
      iv_val = package_json-name
    )->set(
      iv_key = c_id-version
      iv_val = package_json-version ).

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

    IF NOT /apmg/cl_apm_package_json_vali=>is_valid_name( form_data->get( c_id-name ) ).
      result->set(
        iv_key = c_id-name
        iv_val = 'Invalid name' ).
    ENDIF.

    IF NOT /apmg/cl_apm_semver_ranges=>valid_range( form_data->get( c_id-version ) ).
      result->set(
        iv_key = c_id-version
        iv_val = 'Invalid version or range' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
