CLASS zcl_abappm_gui_dlg_publish DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !iv_package    TYPE devclass OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass
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
      END OF c_action.

    DATA:
      mv_registry       TYPE string,
      mv_package        TYPE devclass,
      mo_form           TYPE REF TO zcl_abappm_html_form,
      mo_form_data      TYPE REF TO zcl_abapgit_string_map,
      mo_form_util      TYPE REF TO zcl_abappm_html_form_utils,
      mo_validation_log TYPE REF TO zcl_abapgit_string_map.

    METHODS read_package
      IMPORTING
        !iv_package         TYPE devclass
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

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



CLASS zcl_abappm_gui_dlg_publish IMPLEMENTATION.


  METHOD constructor.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abappm_html_form_utils=>create( mo_form ).

    mv_package = iv_package.
    IF mv_package IS NOT INITIAL.
      mo_form_data = read_package( mv_package ).
    ENDIF.

    TRY.
        mv_registry = zcl_abappm_settings=>factory( )->get( )-registry.
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_dlg_publish.

    CREATE OBJECT lo_component
      EXPORTING
        iv_package = iv_package.

    ri_page = zcl_abappm_gui_page_hoc=>create(
      iv_page_title      = 'Publish Package'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abappm_html_form=>create(
                iv_form_id   = 'publish-package-form'
                iv_help_page = 'https://docs.abappm.com/' ). " TODO

    ro_form->text(
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

    ro_form->command(
      iv_label       = 'Publish Package'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_action-publish_package
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD read_package.

    DATA:
      lx_error        TYPE REF TO zcx_abappm_error,
      ls_package_json TYPE zif_abappm_package_json_types=>ty_package_json.

    TRY.
        ls_package_json = zcl_abappm_package_json=>factory( iv_package )->get( ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    CREATE OBJECT ro_form_data.

    ro_form_data->set(
      iv_key = c_id-package
      iv_val = iv_package
    )->set(
      iv_key = c_id-name
      iv_val = ls_package_json-name
    )->set(
      iv_key = c_id-version
      iv_val = ls_package_json-version ).

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

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lx_error        TYPE REF TO zcx_abappm_error,
      ls_params       TYPE ty_params,
      ls_package_json TYPE zif_package_json_types=>ty_package_json.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_action-choose_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).
        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          mo_validation_log = validate_form( mo_form_data ).
        ELSE.
          mo_form_data = read_package( |{ mo_form_data->get( c_id-package ) }| ).
        ENDIF.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-publish_package.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          mo_form_data->to_abap( CHANGING cs_container = ls_params ).

          TRY.
              zcl_abappm_command_publish=>run(
                iv_registry     = mv_registry
                iv_package      = ls_params-package ).

              rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
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
