CLASS /apmg/cl_apm_gui_dlg_login DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Login Command
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
        username TYPE string,
        password TYPE string,
      END OF ty_params.

    CONSTANTS:
      BEGIN OF c_id,
        username TYPE string VALUE 'username',
        password TYPE string VALUE 'password',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_action,
        login TYPE string VALUE 'login',
      END OF c_action.

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



CLASS /apmg/cl_apm_gui_dlg_login IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    form_data = form_util->normalize_abapgit( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_action-login.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          DATA(params) = get_parameters( form_data ).

*          TRY.
          /apmg/cl_apm_command_login=>run(
            registry = registry
            username = params-username
            password = params-password ).

          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-go_back.
*            CATCH /apmg/cx_apm_error INTO DATA(error).
*              rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
*          ENDTRY.
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

    registry = /apmg/cl_apm_settings=>factory( )->get( )-registry.

    /apmg/cl_apm_registry=>check_logged_out( registry ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_login( ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = 'Login'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'login-form'
      iv_help_page = 'https://docs.abappm.com/' ). " TODO

    result->text(
      iv_name        = c_id-username
      iv_required    = abap_true
      iv_label       = 'User'
      iv_hint        = 'Username for registry'
      iv_max         = 30
    )->text(
      iv_name        = c_id-password
      iv_required    = abap_true
      iv_password    = abap_true
      iv_label       = 'Password'
      iv_min         = /apmg/if_apm_types=>c_package_name-min_length
      iv_max         = /apmg/if_apm_types=>c_package_name-max_length
    )->command(
      iv_label       = 'Login'
      iv_cmd_type    = /apmg/if_apm_html_form=>c_cmd_type-input_main
      iv_action      = c_action-login
    )->command(
      iv_label       = 'Back'
      iv_action      = /apmg/if_apm_gui_router=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_parameters.

    form_data->to_struc( CHANGING cs_container = result ).

    result = CORRESPONDING #( result ).

  ENDMETHOD.


  METHOD validate_form.

    result = form_util->validate( form_data ).

    DATA(username) = CONV devclass( form_data->get( c_id-username ) ).
    IF username IS NOT INITIAL.
      TRY.
          "...
        CATCH zcx_abapgit_exception INTO DATA(error).
          result->set(
            iv_key = c_id-username
            iv_val = error->get_text( ) ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
