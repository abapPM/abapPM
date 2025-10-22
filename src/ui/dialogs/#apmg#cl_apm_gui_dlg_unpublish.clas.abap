CLASS /apmg/cl_apm_gui_dlg_unpublish DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Unpublish Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
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
        choose_package    TYPE string VALUE 'choose-package',
        unpublish_package TYPE string VALUE 'unpublish-package',
        unpublish_version TYPE string VALUE 'unpublish-version',
        refresh           TYPE string VALUE 'refresh',
      END OF c_action.

    DATA:
      registry          TYPE string,
      unpublish_package TYPE devclass,
      form              TYPE REF TO /apmg/cl_apm_html_form,
      form_data         TYPE REF TO /apmg/cl_apm_string_map,
      form_util         TYPE REF TO /apmg/cl_apm_html_form_utils,
      validation_log    TYPE REF TO /apmg/cl_apm_string_map.

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

    METHODS confirm_popup_version
      IMPORTING
        !params       TYPE ty_params
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    METHODS confirm_popup_package
      IMPORTING
        !params       TYPE ty_params
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_dlg_unpublish IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

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

      WHEN c_action-refresh.

        form_data = read_package( |{ form_data->get( c_id-package ) }| ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-unpublish_package.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          DATA(params) = get_parameters( form_data ).

          IF confirm_popup_package( params ) = abap_true.
            /apmg/cl_apm_command_unpublish=>run(
              registry = registry
              name     = params-name ).
          ENDIF.

          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-go_back.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render. " Display errors
        ENDIF.

      WHEN c_action-unpublish_version.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          params = get_parameters( form_data ).

          IF confirm_popup_version( params ) = abap_true.
            /apmg/cl_apm_command_unpublish=>run(
              registry = registry
              name     = params-name
              version  = params-version ).
          ENDIF.

          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-go_back.
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


  METHOD confirm_popup_package.

    DATA(question) = |This will UNPUBLISH the COMPLETE { params-name } package | &&
                     |from the registry (Note: Terms will apply)|.

    DATA(answer) = /apmg/cl_apm_gui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Unpublish Package'
      iv_text_question         = question
      iv_text_button_1         = 'Unpublish'
      iv_icon_button_1         = 'ICON_EXPORT'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false
      iv_popup_type            = 'ICON_MESSAGE_WARNING' ).

    IF answer = '2'.
      MESSAGE 'Unpublish cancelled' TYPE 'S'.
      RETURN.
    ENDIF.

    result = abap_true.

  ENDMETHOD.


  METHOD confirm_popup_version.

    DATA(question) = |This will UNPUBLISH { params-name } { params-version } | &&
                     |from the registry (Note: Terms will apply)|.

    DATA(answer) = /apmg/cl_apm_gui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Unpublish Version'
      iv_text_question         = question
      iv_text_button_1         = 'Unpublish'
      iv_icon_button_1         = 'ICON_EXPORT'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false
      iv_popup_type            = 'ICON_MESSAGE_WARNING' ).

    IF answer = '2'.
      MESSAGE 'Unpublish cancelled' TYPE 'S'.
      RETURN.
    ENDIF.

    result = abap_true.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    validation_log = NEW #( ).
    form_data      = NEW #( ).
    form           = get_form_schema( ).
    form_util      = /apmg/cl_apm_html_form_utils=>create( form ).

    unpublish_package = package.
    IF unpublish_package IS NOT INITIAL.
      form_data = read_package( unpublish_package ).
    ENDIF.

    registry = /apmg/cl_apm_settings=>factory( )->get( )-registry.

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_unpublish( package ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = 'Unpublish Package'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'unpublish-package-form'
      iv_help_page = 'https://docs.abappm.com/' ). " TODO

    result->text(
      iv_name  = c_id-name
      iv_label = 'Name'
    )->text(
      iv_name  = c_id-version
      iv_label = 'Version' ).

    result->command(
      iv_label    = 'Unpublish Version'
      iv_cmd_type = /apmg/if_apm_html_form=>c_cmd_type-input_main
      iv_action   = c_action-unpublish_version
    )->command(
      iv_label    = 'Unpublish Complete Package'
      iv_action   = c_action-unpublish_package
    )->command(
      iv_label    = 'Back'
      iv_action   = /apmg/if_apm_gui_router=>c_action-go_back ).

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
    IF package IS NOT INITIAL.
      TRY.
          zcl_abapgit_factory=>get_sap_package( package )->validate_name( ).
        CATCH zcx_abapgit_exception INTO DATA(error).
          result->set(
            iv_key = c_id-package
            iv_val = error->get_text( ) ).
      ENDTRY.

      IF /apmg/cl_apm_auth=>is_package_allowed( package ) = abap_false.
        result->set(
          iv_key = c_id-package
          iv_val = 'Package not allowed (responsible user = "SAP")' ).
      ENDIF.
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
