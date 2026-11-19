CLASS /apmg/cl_apm_gui_dlg_publish DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Publish Command
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
        tag     TYPE string,
        access  TYPE string,
      END OF ty_params.

    CONSTANTS:
      BEGIN OF c_id,
        package TYPE string VALUE 'package',
        name    TYPE string VALUE 'name',
        version TYPE string VALUE 'version',
        tag     TYPE string VALUE 'tag',
        access  TYPE string VALUE 'access',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_action,
        choose_package TYPE string VALUE 'choose-package',
        publish        TYPE string VALUE 'publish',
        refresh        TYPE string VALUE 'refresh',
        confirmed      TYPE string VALUE 'confirmed',
        cancel         TYPE string VALUE 'cancel',
      END OF c_action.

    DATA:
      registry        TYPE string,
      publish_package TYPE devclass,
      is_scoped       TYPE abap_bool,
      params          TYPE ty_params,
      popup           TYPE REF TO /apmg/cl_apm_popup_to_confirm,
      form            TYPE REF TO /apmg/cl_apm_html_form,
      form_data       TYPE REF TO /apmg/cl_apm_string_map,
      form_util       TYPE REF TO /apmg/cl_apm_html_form_utils,
      validation_log  TYPE REF TO /apmg/cl_apm_string_map.

    METHODS init_form
      RAISING
        /apmg/cx_apm_error.

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

    METHODS popup_to_confirm
      IMPORTING
        !params TYPE ty_params
      RAISING
        /apmg/cx_apm_error.

    METHODS popup_render
      RETURNING
        VALUE(result) TYPE REF TO object
      RAISING
        /apmg/cx_apm_error.

    METHODS popup_close.

ENDCLASS.



CLASS /apmg/cl_apm_gui_dlg_publish IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    form_data = form_util->normalize( ii_event->form_data( ) ).

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

      WHEN c_action-publish.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          params = get_parameters( form_data ).
          popup_to_confirm( params ).
        ENDIF.

        publish_package = params-package.

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-confirmed.

        popup_close( ).

        /apmg/cl_apm_command_publish=>run(
          registry = registry
          package  = params-package
          tag      = params-tag
          access   = params-access ).

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-go_back.

      WHEN c_action-cancel.

        popup_close( ).
        init_form( ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

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

    html->add( popup_render( ) ).

    ri_html = html.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    registry = /apmg/cl_apm_settings=>factory( )->get( )-registry.

    validation_log = NEW #( ).
    form_data      = NEW #( ).

    publish_package = package.

    init_form( ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_publish( package ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = 'Publish Package'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'publish-package-form'
      iv_help_page = 'https://docs.abappm.com/' ). " TODO

    result->text(
      iv_name        = c_id-package
      iv_side_action = c_action-choose_package
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package'
      iv_placeholder = 'Z... / $...'
      iv_max         = 30
    )->text(
      iv_name     = c_id-name
      iv_label    = 'Name'
      iv_readonly = abap_true
    )->text(
      iv_name     = c_id-version
      iv_label    = 'Version'
      iv_readonly = abap_true
    )->text(
      iv_name  = c_id-tag
      iv_label = 'Tag'
      iv_hint  = 'Distribution Tag'
    )->radio(
      iv_name     = c_id-access
      iv_label    = 'Package Access'
      iv_hint     = 'The package will be viewable (and installable) publicly'
    )->option(
      iv_value = /apmg/if_apm_command=>c_access-public
      iv_label = 'Public'
    )->option(
      iv_value = /apmg/if_apm_command=>c_access-private
      iv_label = 'Private' ).

    result->command(
      iv_label    = 'Publish'
      iv_cmd_type = /apmg/if_apm_html_form=>c_cmd_type-input_main
      iv_action   = c_action-publish
    )->command(
      iv_label  = 'Refresh'
      iv_action = c_action-refresh
    )->command(
      iv_label  = 'Back'
      iv_action = /apmg/if_apm_gui_router=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_parameters.

    form_data->to_struc( CHANGING cs_container = result ).

  ENDMETHOD.


  METHOD init_form.

    IF publish_package IS NOT INITIAL.
      form_data = read_package( publish_package ).
    ENDIF.

    form      = get_form_schema( ).
    form_util = /apmg/cl_apm_html_form_utils=>create( form ).

  ENDMETHOD.


  METHOD popup_close.

    IF popup IS BOUND.
      popup->close( ).
    ENDIF.

  ENDMETHOD.


  METHOD popup_render.

    IF popup IS INITIAL OR popup->was_closed( ) = abap_true.
      result = /apmg/cl_apm_html=>create( ).
    ELSE.
      result = /apmg/cl_apm_gui_in_page_modal=>create( popup ).
    ENDIF.

  ENDMETHOD.


  METHOD popup_to_confirm.

    DATA(question) = |This will <b>publish</b> all objects in { params-package } | &&
                     |including subpackages as <b>{ params-access }</b> package | &&
                     |"<b>{ params-name }</b>" version <b>{ params-version }</b> | &&
                     |to the registry|.

    popup = /apmg/cl_apm_popup_to_confirm=>create(
      titlebar              = 'Publish Package'
      text_question         = question
      text_button_1         = 'Publish'
      action_button_1       = c_action-confirmed
      text_button_2         = 'Cancel'
      action_button_2       = c_action-cancel
      default_button        = '2'
      display_cancel_button = abap_false
      popup_type            = 'ICON_MESSAGE_WARNING' ).

  ENDMETHOD.


  METHOD read_package.

    TRY.
        DATA(package_json) = /apmg/cl_apm_package_json=>factory( package )->get( ).
      CATCH /apmg/cx_apm_error.
        RETURN.
    ENDTRY.

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

    DATA(prerelease) = /apmg/cl_apm_semver_functions=>prerelease( package_json-version ).

    IF prerelease IS NOT INITIAL.
      DATA(tag) = prerelease[ 1 ].

      result->set(
        iv_key = c_id-tag
        iv_val = tag ).
    ELSE.
      result->set(
        iv_key = c_id-tag
        iv_val = 'latest' ).
    ENDIF.

    is_scoped = xsdbool( package_json-name CP '@*' ).

    DATA(access) = /apmg/if_apm_command=>c_access-public.

    IF is_scoped = abap_true.
      " Set access based on existing package
      TRY.
          access = /apmg/cl_apm_registry=>get_packument(
            registry = registry
            name     = package_json-name )-access.

          " Legacy packages didn't store access so we assume they are private
          IF access IS INITIAL.
            access = /apmg/if_apm_command=>c_access-private.
          ENDIF.
        CATCH /apmg/cx_apm_error.
          access = /apmg/if_apm_command=>c_access-private.
      ENDTRY.
    ENDIF.

    result->set(
      iv_key = c_id-access
      iv_val = access ).

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

    DATA(access) = form_data->get( c_id-access ).

    IF is_scoped = abap_false AND access <> /apmg/if_apm_command=>c_access-public.
      result->set(
        iv_key = c_id-access
        iv_val = 'Unscoped packages must be public' ).
    ENDIF.

    " TODO: Validate dist-tag

  ENDMETHOD.
ENDCLASS.
