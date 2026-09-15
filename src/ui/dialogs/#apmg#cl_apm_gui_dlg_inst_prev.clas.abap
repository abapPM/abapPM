CLASS /apmg/cl_apm_gui_dlg_inst_prev DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Dialog for Install Preview
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_menu_provider,
      /apmg/if_apm_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !registry     TYPE string
        !root_name    TYPE /apmg/if_apm_types=>ty_name
        !version      TYPE /apmg/if_apm_types=>ty_version
        !package      TYPE devclass
        !transport    TYPE trkorr
        !diff         TYPE REF TO /apmg/if_apm_arborist_diff
        !log          TYPE /apmg/if_apm_arborist=>ty_log
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !registry  TYPE string
        !root_name TYPE /apmg/if_apm_types=>ty_name
        !version   TYPE /apmg/if_apm_types=>ty_version
        !package   TYPE devclass
        !transport TYPE trkorr
        !diff      TYPE REF TO /apmg/if_apm_arborist_diff
        !log       TYPE /apmg/if_apm_arborist=>ty_log
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        added     TYPE string VALUE 'added',
        changed   TYPE string VALUE 'changed',
        removed   TYPE string VALUE 'removed',
        warnings  TYPE string VALUE 'warnings',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_action,
        confirm          TYPE string VALUE 'confirm-install',
        create_packages  TYPE string VALUE 'create-packages',
      END OF c_action.

    DATA registry TYPE string.
    data transport TYPE trkorr.
    DATA root_name TYPE /apmg/if_apm_types=>ty_name.
    DATA version TYPE /apmg/if_apm_types=>ty_version.
    DATA diff TYPE REF TO /apmg/if_apm_arborist_diff.
    DATA log TYPE /apmg/if_apm_arborist=>ty_log.
    DATA changes TYPE /apmg/if_apm_arborist_diff=>ty_diff_refs.
    DATA add_names TYPE string_table.
    DATA form TYPE REF TO /apmg/cl_apm_html_form.
    DATA form_data TYPE REF TO /apmg/cl_apm_string_map.
    DATA form_util TYPE REF TO /apmg/cl_apm_html_form_utils.
    DATA validation_log TYPE REF TO /apmg/cl_apm_string_map.

    METHODS get_form_schema
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_form.

    METHODS initialize_form_data
      IMPORTING
        !transport TYPE trkorr.

    METHODS get_assignments
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_command_install=>ty_package_assignments.

    METHODS validate_form
      IMPORTING
        !require_existing TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result)     TYPE REF TO /apmg/cl_apm_string_map
      RAISING
        /apmg/cx_apm_error.

    METHODS add_validation
      IMPORTING
        !target TYPE REF TO /apmg/cl_apm_string_map
        !key    TYPE string
        !text   TYPE string.

    METHODS create_missing_packages
      RAISING
        /apmg/cx_apm_error.

    METHODS get_warning_text
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS /apmg/cl_apm_gui_dlg_inst_prev IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    form_data = form_util->normalize_abapgit( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_action-create_packages.
        validation_log = validate_form( require_existing = abap_false ).
        IF validation_log->is_empty( ) = abap_true.
          create_missing_packages( ).
          validation_log = validate_form( ).
        ENDIF.
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-confirm.
        validation_log = validate_form( ).
        IF validation_log->is_empty( ) = abap_true.
          DATA(assignments) = get_assignments( ).

          /apmg/cl_apm_command_install=>run(
            registry    = registry
            root_name   = root_name
            diff        = diff
            assignments = assignments
            transport   = transport ).

          DATA(root_package) = assignments[ name = root_name ]-package.
          rs_handled-page  = /apmg/cl_apm_gui_page_package=>create( root_package ).
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-new_page_replacing.
        ELSE.
          rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.
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


  METHOD add_validation.

    DATA(message) = target->get( key ).
    IF message IS NOT INITIAL.
      message = message && |\n|.
    ENDIF.
    target->set(
      iv_key = key
      iv_val = message && text ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    me->registry  = registry.
    me->transport = transport.
    me->root_name = root_name.
    me->version   = version.
    me->diff      = diff.
    me->log       = log.

    IF diff IS NOT BOUND.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = 'The Arborist diff is not available'.
    ENDIF.

    changes = diff->get_changes( root_name ).
    IF changes IS INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |No install changes were found for { root_name }|.
    ENDIF.

    validation_log = NEW #( ).
    form_data      = NEW #( ).
    form           = get_form_schema( ).
    form_util      = /apmg/cl_apm_html_form_utils=>create( form ).
    initialize_form_data( transport ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_dlg_inst_prev(
      registry  = registry
      root_name = root_name
      version   = version
      package   = package
      transport = transport
      diff      = diff
      log       = log ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title      = |Install Preview: { root_name }@{ version }|
      child_component = component ).

  ENDMETHOD.


  METHOD create_missing_packages.

    DATA(assignments) = get_assignments( ).

    LOOP AT assignments ASSIGNING FIELD-SYMBOL(<assignment>).
      DATA(sap_package) = zcl_abapgit_factory=>get_sap_package( <assignment>-package ).
      IF sap_package->exists( ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(created_package) = /apmg/cl_apm_popup_utils=>create_package( <assignment>-package ).
      IF created_package IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE add_names TRANSPORTING NO FIELDS WITH KEY table_line = <assignment>-name.
      IF sy-subrc = 0.
        form_data->set(
          iv_key = |{ c_id-added }-{ sy-tabix }-4|
          iv_val = created_package ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_assignments.

    LOOP AT add_names ASSIGNING FIELD-SYMBOL(<name>).
      INSERT VALUE #(
        name    = <name>
        package = to_upper( form_data->get( |{ c_id-added }-{ sy-tabix }-4| ) ) )
        INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_form_schema.

    result = /apmg/cl_apm_html_form=>create(
      iv_form_id   = 'install-preview-form'
      iv_help_page = 'https://docs.abappm.com/' ).

    DATA(warning_text) = get_warning_text( ).
    IF warning_text IS NOT INITIAL.
      result->freetext(
        iv_name = c_id-warnings
        iv_text = warning_text ).
    ENDIF.

    result->table(
      iv_name  = c_id-added
      iv_label = 'Packages to Add'
    )->column(
      iv_label    = 'Action'
      iv_width    = '10%'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'Name'
      iv_width    = '35%'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'Version'
      iv_width    = '20%'
      iv_readonly = abap_true
    )->column(
      iv_label = 'SAP Package'
      iv_width = '35%' ).

    result->table(
      iv_name  = c_id-changed
      iv_label = 'Packages to Change'
    )->column(
      iv_label    = 'Action'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'Name'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'Installed Version'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'Target Version'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'SAP Package'
      iv_readonly = abap_true ).

    result->table(
      iv_name  = c_id-removed
      iv_label = 'Packages to Remove'
    )->column(
      iv_label    = 'Action'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'Name'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'Installed Version'
      iv_readonly = abap_true
    )->column(
      iv_label    = 'SAP Package'
      iv_readonly = abap_true ).

    result->command(
      iv_label    = 'Install Changes'
      iv_cmd_type = /apmg/if_apm_html_form=>c_cmd_type-input_main
      iv_action   = c_action-confirm
    )->command(
      iv_label  = 'Create Missing SAP Packages'
      iv_action = c_action-create_packages
    )->command(
      iv_label  = 'Back'
      iv_action = /apmg/if_apm_gui_router=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_warning_text.

    LOOP AT log ASSIGNING FIELD-SYMBOL(<entry>)
        WHERE type = /apmg/if_apm_arborist=>c_log_type-warning.
      IF result IS INITIAL.
        result = 'Warnings:<br>'.
      ENDIF.
      DATA(message) = escape(
        val    = <entry>-message
        format = cl_abap_format=>e_html_text ).
      result = result && |- { message }<br>|.
    ENDLOOP.

  ENDMETHOD.


  METHOD initialize_form_data.

    DATA(add_row) = 0.
    DATA(change_row) = 0.
    DATA(remove_row) = 0.

    LOOP AT changes INTO DATA(change).
      DATA(actual) = change->get_actual( ).
      DATA(ideal) = change->get_ideal( ).

      CASE change->get_action( ).
        WHEN /apmg/if_apm_arborist=>c_diff_action-add.
          add_row = add_row + 1.
          APPEND ideal->name TO add_names.
          form_data->set( iv_key = |{ c_id-added }-{ add_row }-1| iv_val = 'ADD' ).
          form_data->set( iv_key = |{ c_id-added }-{ add_row }-2| iv_val = ideal->name ).
          form_data->set( iv_key = |{ c_id-added }-{ add_row }-3| iv_val = ideal->version ).
          form_data->set(
            iv_key = |{ c_id-added }-{ add_row }-4|
            iv_val = ideal->get_manifest( )-sap_package-default ).

        WHEN /apmg/if_apm_arborist=>c_diff_action-change.
          change_row = change_row + 1.
          form_data->set( iv_key = |{ c_id-changed }-{ change_row }-1| iv_val = 'CHANGE' ).
          form_data->set( iv_key = |{ c_id-changed }-{ change_row }-2| iv_val = ideal->name ).
          form_data->set( iv_key = |{ c_id-changed }-{ change_row }-3| iv_val = actual->version ).
          form_data->set( iv_key = |{ c_id-changed }-{ change_row }-4| iv_val = ideal->version ).
          form_data->set( iv_key = |{ c_id-changed }-{ change_row }-5| iv_val = actual->package ).

        WHEN /apmg/if_apm_arborist=>c_diff_action-remove.
          remove_row = remove_row + 1.
          form_data->set( iv_key = |{ c_id-removed }-{ remove_row }-1| iv_val = 'REMOVE' ).
          form_data->set( iv_key = |{ c_id-removed }-{ remove_row }-2| iv_val = actual->name ).
          form_data->set( iv_key = |{ c_id-removed }-{ remove_row }-3| iv_val = actual->version ).
          form_data->set( iv_key = |{ c_id-removed }-{ remove_row }-4| iv_val = actual->package ).
      ENDCASE.
    ENDLOOP.

    form_data->set( iv_key = |{ c_id-added }-rows| iv_val = |{ add_row }| ).
    form_data->set( iv_key = |{ c_id-changed }-rows| iv_val = |{ change_row }| ).
    form_data->set( iv_key = |{ c_id-removed }-rows| iv_val = |{ remove_row }| ).

  ENDMETHOD.


  METHOD validate_form.

    result = form_util->validate( form_data ).
    DATA(assignments) = get_assignments( ).
    DATA(installed_packages) = /apmg/cl_apm_package_json=>list( instanciate = abap_true ).

    LOOP AT assignments ASSIGNING FIELD-SYMBOL(<assignment>).
      IF <assignment>-package IS INITIAL.
        add_validation(
          target = result
          key    = c_id-added
          text   = |SAP package is required for { <assignment>-name }| ).
        CONTINUE.
      ENDIF.

      IF /apmg/cl_apm_package_json_vali=>is_valid_sap_package( <assignment>-package ) = abap_false.
        add_validation(
          target = result
          key    = c_id-added
          text   = |Invalid SAP package { <assignment>-package } for { <assignment>-name }| ).
        CONTINUE.
      ENDIF.

      DATA(message) = /apmg/cl_apm_auth=>check_package_allowed( <assignment>-package ).
      IF message IS NOT INITIAL.
        add_validation( target = result key = c_id-added text = message ).
      ENDIF.

      IF line_exists( installed_packages[ KEY package COMPONENTS package = <assignment>-package ] ).
        DATA(installed) = installed_packages[ KEY package COMPONENTS package = <assignment>-package ].
        add_validation(
          target = result
          key    = c_id-added
          text   = |SAP package { <assignment>-package } already contains { installed-name }| ).
      ENDIF.

      DATA(sap_package) = zcl_abapgit_factory=>get_sap_package( <assignment>-package ).
      IF require_existing = abap_true AND sap_package->exists( ) = abap_false.
        add_validation(
          target = result
          key    = c_id-added
          text   = |SAP package { <assignment>-package } does not exist; create it before installing| ).
      ELSEIF sap_package->exists( ) = abap_true.
        DATA(assigned_package) = <assignment>-package.
        SELECT COUNT(*) FROM tadir INTO @DATA(count) WHERE devclass = @assigned_package. "#EC CI_SGLSELECT
        IF count > 1.
          add_validation(
            target = result
            key    = c_id-added
            text   = |SAP package { <assignment>-package } is not empty| ).
        ENDIF.
      ENDIF.

      IF transport IS INITIAL.
        message = /apmg/cl_apm_auth=>check_transport_required( <assignment>-package ).
        IF message IS NOT INITIAL.
          add_validation(
            target = result
            key    = c_id-added
            text   = |{ message }: { <assignment>-package }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT assignments ASSIGNING FIELD-SYMBOL(<left>).
      LOOP AT assignments ASSIGNING FIELD-SYMBOL(<right>).
        IF <right>-name > <left>-name
            AND <left>-package IS NOT INITIAL
            AND <left>-package = <right>-package.
          add_validation(
            target = result
            key    = c_id-added
            text   = |SAP package { <left>-package } is assigned to { <left>-name } and { <right>-name }| ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF transport IS INITIAL.
      LOOP AT changes INTO DATA(change).
        IF change->get_action( ) = /apmg/if_apm_arborist=>c_diff_action-add.
          CONTINUE.
        ENDIF.
        DATA(actual) = change->get_actual( ).
        IF actual IS BOUND.
          message = /apmg/cl_apm_auth=>check_transport_required( actual->package ).
          IF message IS NOT INITIAL.
            add_validation(
              target = result
              key    = c_id-added
              text   = |{ message }: { actual->package }| ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
