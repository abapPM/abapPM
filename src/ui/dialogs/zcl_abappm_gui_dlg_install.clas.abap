CLASS zcl_abappm_gui_dlg_install DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_params,
        package   TYPE devclass,
        name      TYPE string,
        version   TYPE string,
        only_deps TYPE abap_bool,
      END OF ty_params.

    CONSTANTS:
      BEGIN OF c_id,
        package   TYPE string VALUE 'package',
        name      TYPE string VALUE 'name',
        version   TYPE string VALUE 'version',
        only_deps TYPE string VALUE 'only_deps',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        choose_package  TYPE string VALUE 'choose-package',
        create_package  TYPE string VALUE 'create-package',
        install_package TYPE string VALUE 'install-package',
      END OF c_event .

    DATA:
      mv_registry       TYPE string,
      mo_form           TYPE REF TO zcl_abappm_html_form,
      mo_form_data      TYPE REF TO zcl_abapgit_string_map,
      mo_form_util      TYPE REF TO zcl_abappm_html_form_utils,
      mo_validation_log TYPE REF TO zcl_abapgit_string_map.

    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abappm_html_form.

ENDCLASS.



CLASS zcl_abappm_gui_dlg_install IMPLEMENTATION.


  METHOD constructor.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abappm_html_form_utils=>create( mo_form ).

    TRY.
        mv_registry = zcl_abappm_settings=>factory( )->get( )-registry.
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_dlg_install.

    CREATE OBJECT lo_component.

    ri_page = zcl_abappm_gui_page_hoc=>create(
      iv_page_title      = 'Install Package'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abappm_html_form=>create(
                iv_form_id   = 'install-package-form'
                iv_help_page = 'https://docs.abappm.com/' ). " TODO

    ro_form->text(
      iv_name        = c_id-package
      iv_side_action = c_event-choose_package
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
      iv_hint        = 'Name of the package'
      iv_min         = zif_abappm_package_json_types=>c_package_name-min_length
      iv_max         = zif_abappm_package_json_types=>c_package_name-max_length
    )->text(
      iv_name        = c_id-version
      iv_required    = abap_true
      iv_label       = 'Version'
      iv_hint        = 'Semantic version (x.y.z)'
    )->checkbox(
      iv_name        = c_id-only_deps
      iv_label       = 'Install Only Dependencies' ).

    ro_form->command(
      iv_label       = 'Install Package'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-install_package
    )->command(
      iv_label       = 'Create Package'
      iv_action      = c_event-create_package
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD validate_form.

    DATA:
      lv_as4user TYPE as4user,
      lv_package TYPE devclass,
      lx_err     TYPE REF TO zcx_abapgit_exception.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    lv_package = io_form_data->get( c_id-package ).
    IF lv_package IS NOT INITIAL.
      TRY.
          zcl_abapgit_factory=>get_sap_package( lv_package )->validate_name( ).

          " Check if package owned by SAP is allowed (new packages are ok, since they are created automatically)
          lv_as4user = zcl_abapgit_factory=>get_sap_package( lv_package )->read_responsible( ).

          IF sy-subrc = 0 AND lv_as4user = 'SAP' AND
            zcl_abapgit_factory=>get_environment( )->is_sap_object_allowed( ) = abap_false.
            zcx_abapgit_exception=>raise( |Package { lv_package } not allowed, responsible user = 'SAP'| ).
          ENDIF.
        CATCH zcx_abapgit_exception INTO lx_err.
          ro_validation_log->set(
            iv_key = c_id-package
            iv_val = lx_err->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF zcl_package_json_valid=>is_valid_name( io_form_data->get( c_id-name ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-name
        iv_val = 'Invalid name' ).
    ENDIF.

    IF zcl_package_json_valid=>is_valid_version( io_form_data->get( c_id-version ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-version
        iv_val = 'Invalid version' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lx_error        TYPE REF TO zcx_abappm_error,
      ls_params       TYPE ty_params,
      ls_package_json TYPE zif_package_json_types=>ty_package_json.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-create_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = zcl_abapgit_services_repo=>create_package(
            iv_prefill_package = |{ mo_form_data->get( c_id-package ) }| ) ).
        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          mo_validation_log = validate_form( mo_form_data ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).
        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          mo_validation_log = validate_form( mo_form_data ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-install_package.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          mo_form_data->to_abap( CHANGING cs_container = ls_params ).

          MOVE-CORRESPONDING ls_params TO ls_package_json.

          TRY.
              zcl_abappm_command_install=>run(
                iv_registry          = mv_registry
                iv_package           = ls_params-package
                is_package_json      = ls_package_json
                iv_only_dependencies = ls_params-only_deps ).

              rs_handled-page  = zcl_abappm_gui_page_package=>create( ls_params-package ).
              rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
            CATCH zcx_abappm_error INTO lx_error.
              zcx_abapgit_exception=>raise_with_text( lx_error ).
          ENDTRY.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
