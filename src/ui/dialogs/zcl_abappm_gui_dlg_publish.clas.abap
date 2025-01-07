CLASS zcl_abappm_gui_dlg_publish DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
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
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !package      TYPE devclass OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !package TYPE devclass
      RAISING
        zcx_abapgit_exception.

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
        choose_package  TYPE string VALUE 'choose-package',
        publish_package TYPE string VALUE 'publish-package',
        refresh         TYPE string VALUE 'refresh',
      END OF c_action.

    DATA:
      registry       TYPE string,
      pubish_package TYPE devclass,
      form           TYPE REF TO zcl_abappm_html_form,
      form_data      TYPE REF TO zcl_abappm_string_map,
      form_util      TYPE REF TO zcl_abappm_html_form_utils,
      validation_log TYPE REF TO zcl_abappm_string_map.

    METHODS get_form_schema
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_html_form.

    METHODS get_parameters
      IMPORTING
        !form_data    TYPE REF TO zcl_abappm_string_map
      RETURNING
        VALUE(result) TYPE ty_params.

    METHODS validate_form
      IMPORTING
        !form_data    TYPE REF TO zcl_abappm_string_map
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS read_package
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_string_map
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_dlg_publish IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    validation_log = NEW #( ).
    form_data      = NEW #( ).
    form           = get_form_schema( ).
    form_util      = zcl_abappm_html_form_utils=>create( form ).

    pubish_package = package.
    IF pubish_package IS NOT INITIAL.
      form_data = read_package( pubish_package ).
    ENDIF.

    TRY.
        registry = zcl_abappm_settings=>factory( )->get( )-registry.
      CATCH zcx_abappm_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW zcl_abappm_gui_dlg_publish( package ).

    result = zcl_abappm_gui_page_hoc=>create(
      page_title      = 'Publish Package'
      child_component = component ).

  ENDMETHOD.


  METHOD get_form_schema.

    result = zcl_abappm_html_form=>create(
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
      iv_name        = c_id-name
      iv_label       = 'Name'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-version
      iv_label       = 'Version'
      iv_readonly    = abap_true ).

    result->command(
      iv_label       = 'Publish Package'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_action-publish_package
    )->command(
      iv_label       = 'Refresh'
      iv_action      = c_action-refresh
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_parameters.

    form_data->to_struc( CHANGING cs_container = result ).

  ENDMETHOD.


  METHOD read_package.

    TRY.
        DATA(package_json) = zcl_abappm_package_json=>factory( package )->get( ).
      CATCH zcx_abappm_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
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

  ENDMETHOD.


  METHOD validate_form.

    DATA package TYPE devclass.

    result = form_util->validate( form_data ).

    package = form_data->get( c_id-package ).
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

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    form_data = form_util->normalize_abapgit( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_action-choose_package.

        form_data->set(
          iv_key = c_id-package
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).
        IF form_data->get( c_id-package ) IS NOT INITIAL.
          validation_log = validate_form( form_data ).
        ELSE.
          form_data = read_package( |{ form_data->get( c_id-package ) }| ).
        ENDIF.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-refresh.

        form_data = read_package( |{ form_data->get( c_id-package ) }| ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-publish_package.

        validation_log = validate_form( form_data ).

        IF validation_log->is_empty( ) = abap_true.
          DATA(params) = get_parameters( form_data ).

          TRY.
              zcl_abappm_command_publish=>run(
                registry     = registry
                package      = params-package ).

              rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
            CATCH zcx_abappm_error INTO DATA(error).
              zcx_abapgit_exception=>raise_with_text( error ).
          ENDTRY.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    DATA(html) = zcl_abapgit_html=>create( ).

    html->add( '<div class="form-container">' ).
    html->add( form->render(
      io_values         = form_data
      io_validation_log = validation_log ) ).
    html->add( '</div>' ).

    ri_html = html.

  ENDMETHOD.
ENDCLASS.
