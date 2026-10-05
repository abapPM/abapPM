CLASS /apmg/cl_apm_gui_page_welcome DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Welcome Page
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
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        setup_persistence  TYPE string VALUE 'setup_persistence',
        setup_certificates TYPE string VALUE 'setup_certificates',
        refresh            TYPE string VALUE 'refresh',
      END OF c_action.

    CONSTANTS c_ping_pong TYPE string VALUE 'PONG'.

    DATA emoji TYPE REF TO /apmg/cl_apm_emoji.

    METHODS get_styles
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_welcome
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_connections
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_persistence
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS confirm_popup
      RETURNING
        VALUE(result) TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_page_welcome IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-refresh.

        " Re-runs connection check
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-setup_persistence.

        IF confirm_popup( ) = abap_true.
          /apmg/cl_apm_persist_apm_setup=>install( ).
        ENDIF.

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-setup_certificates.

        IF confirm_popup( ) = abap_true.
          /apmg/cl_apm_certificates=>setup( ).
        ENDIF.

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_menu_provider~get_menu.

    DATA(toolbar) = /apmg/cl_apm_html_toolbar=>create( 'apm-welcome' ).

    toolbar->add(
      iv_txt = /apmg/cl_apm_html=>icon( 'file' ) && ' Init'
      iv_act = /apmg/if_apm_gui_router=>c_action-apm_init
    )->add(
      iv_txt = /apmg/cl_apm_html=>icon( 'download-solid' ) && ' Install'
      iv_act = /apmg/if_apm_gui_router=>c_action-apm_install
    )->add(
      iv_txt = /apmg/cl_apm_html=>icon( 'bars' ) && ' Package List'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_home
    )->add(
      iv_txt = /apmg/cl_apm_gui_buttons=>settings( )
      io_sub = /apmg/cl_apm_gui_menus=>settings( )
    )->add(
      iv_txt = /apmg/cl_apm_gui_buttons=>refresh( )
      iv_act = c_action-refresh
    )->add(
      iv_txt = /apmg/cl_apm_gui_buttons=>help( )
      io_sub = /apmg/cl_apm_gui_menus=>help( abap_false ) ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    register_styles( /apmg/cl_apm_gui_styles=>emoji( get_styles( ) ) ).

    render_welcome( html ).
    render_connections( html ).
    render_persistence( html ).

    ri_html = html.

  ENDMETHOD.


  METHOD confirm_popup.

    DATA(question) =
      `This will install certificates for the apm Registry and Playground into Trust Management (STRUST)`.

    DATA(answer) = /apmg/cl_apm_gui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Setup'
      iv_text_question         = question
      iv_text_button_1         = 'Install Certificates'
      iv_icon_button_1         = 'ICON_EXPORT'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false
      iv_popup_type            = 'ICON_MESSAGE_WARNING' ).

    IF answer = '2'.
      MESSAGE 'Setup cancelled' TYPE 'S'.
      RETURN.
    ENDIF.

    result = abap_true.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    emoji = /apmg/cl_apm_emoji=>create( ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_page_welcome( ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title         = 'Welcome!'
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD get_styles.

    result = |table.repo_tab tbody td \{ padding: 10px \}\n|
      && |table.repo_tab .col-name \{ width: 25% \}\n|
      && |table.repo_tab .col-action \{ width: 70% \}|
      && |table.repo_tab .col-status \{ width: 5% \}|.

  ENDMETHOD.


  METHOD render_connections.

    html->add( '<div style="padding:10px 150px 30px;font-size:large;">' ).
    html->add( '<h3>' ).
    html->add( 'Connection Check' ).
    html->add( '</h3>' ).

    html->add( '<table class="repo_tab w100 paddings">' ).
    html->add( '<tbody>' ).

    DATA(missing_certificates) = abap_false.

    DO 2 TIMES.
      IF sy-index = 2.
        DATA(name)     = `Playground`.
        DATA(registry) = /apmg/if_apm_constants=>c_playground.
        DATA(action)   = /apmg/if_apm_gui_router=>c_action-playground.
      ELSE.
        name     = `Registry`.
        registry = /apmg/if_apm_constants=>c_registry.
        action   = /apmg/if_apm_gui_router=>c_action-registry.
      ENDIF.

      TRY.
          DATA(ping) = /apmg/cl_apm_command_ping=>run( registry ).
        CATCH /apmg/cx_apm_error INTO DATA(error).
          ping = error->get_text( ).
          IF ping CS '421'.
            missing_certificates = abap_true.
          ENDIF.
      ENDTRY.

      html->add( '<tr>' ).
      html->td(
        iv_content = name
        iv_class   = 'col-name' ).
      html->td(
        iv_content = html->a( iv_txt = registry iv_act = action )
        iv_class   = 'col-action' ).

      IF ping = c_ping_pong.
        html->td(
          iv_content = emoji->format( ':heavy_check_mark:' )
          iv_class   = 'col-status' ).
      ELSE.
        html->td(
          iv_content = emoji->format( ':x:' )
          iv_class   = 'col-status' ).
        html->add( '</tr>' ).
        html->add( '<tr>' ).
        html->td( '' ).
        html->td(
          iv_content = ping
          iv_colspan = 2 ).
      ENDIF.
      html->add( '</tr>' ).
    ENDDO.

    IF missing_certificates = abap_true.
      html->add( '<tr>' ).
      html->td( '' ).
      html->td(
        iv_content = html->a(
          iv_txt = 'Install missing certificates...'
          iv_act = c_action-setup_certificates )
        iv_colspan = 2 ).
      html->add( '</tr>' ).
    ENDIF.

    html->add( '</tbody>' ).
    html->add( '</table>' ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_persistence.

    html->add( '<div style="padding:10px 150px 30px;font-size:large;">' ).
    html->add( '<h3>' ).
    html->add( 'Persistence Check' ).
    html->add( '</h3>' ).

    html->add( '<table class="repo_tab w100 paddings">' ).
    html->add( '<tbody>' ).

    DATA(missing_persistence) = abap_false.

    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          DATA(name)   = `Database Table`.
          DATA(object) = /apmg/if_apm_persist_apm=>c_tabname.
          DATA(action) = |type=TABL&name={ /apmg/if_apm_persist_apm=>c_tabname }|.
          DATA(exists) = /apmg/cl_apm_persist_apm_setup=>table_exists( ).
        WHEN 2.
          name   = `Lock Object`.
          object = /apmg/if_apm_persist_apm=>c_lock.
          action = |type=ENQU&name={ /apmg/if_apm_persist_apm=>c_lock }|.
          exists = /apmg/cl_apm_persist_apm_setup=>lock_exists( ).
        WHEN 3.
          name   = `Transport Object`.
          object = /apmg/if_apm_persist_apm=>c_zapm.
          action = |type=SOBJ&nameE={ /apmg/if_apm_persist_apm=>c_zapm }|.
          exists = /apmg/cl_apm_persist_apm_setup=>logo_exists( ).
      ENDCASE.

      html->add( '<tr>' ).
      html->td(
        iv_content = name
        iv_class   = 'col-name' ).
      html->td(
        iv_content = html->a(
          iv_txt   = object
          iv_act   = |{ /apmg/if_apm_gui_router=>c_action-jump }?{ action }| )
        iv_class   = 'col-action' ).

      IF exists = abap_true.
        html->td(
          iv_content = emoji->format( ':heavy_check_mark:' )
          iv_class   = 'col-status' ).
      ELSE.
        missing_persistence = abap_true.
        html->td(
          iv_content = emoji->format( ':x:' )
          iv_class   = 'col-status' ).
      ENDIF.

      html->add( '</tr>' ).
    ENDDO.

    IF missing_persistence = abap_true.
      html->add( '<tr>' ).
      html->td( '' ).
      html->td(
        iv_content = html->a(
          iv_txt = 'Install missing persistence...'
          iv_act = c_action-setup_persistence )
        iv_colspan = 2 ).
      html->add( '</tr>' ).
    ENDIF.

    html->add( '</tbody>' ).
    html->add( '</table>' ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_welcome.

    DATA(apm) = |<strong>apm</strong>|.

    DATA(tutorial) = html->a(
      iv_txt   = 'Tutorial'
      iv_title = 'Tutorial'
      iv_act   = /apmg/if_apm_gui_router=>c_action-tutorial ).

    html->add( '<div style="padding:20px 150px 0;font-size:large;">' ).
    html->add( '<h1>' ).
    html->add( emoji->format( 'Welcome to apm :wave:' ) ).
    html->add( '</h1>' ).
    html->add( '<p>' ).
    html->add( |You're looking at something that, until recently, didn't exist: a real package manager for ABAP.| ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |abapGit gave ABAP its git. For over 10 years we could share code but were missing solid| ).
    html->add( |versioning, dependencies, and automatic installs. We had git, but no npm.| ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |That's why I built { apm } and why a growing community of ABAP developers is building| ).
    html->add( |ABAP open-source with you. Whatever you do here, you're now part of that.| ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |You can:| ).
    html->add( '</p>' ).
    html->add( '<ul>' ).
    html->add( '<li>' ).
    html->add( emoji->format( ':mag_right:' ) ).
    html->add( |Browse the registry and find packages to solve problems you would otherwise build from scratch| ).
    html->add( '</li>' ).
    html->add( '<li>' ).
    html->add( emoji->format( ':package:' ) ).
    html->add( |Install packages with a few clicks, dependencies resolve automatically| ).
    html->add( '</li>' ).
    html->add( '<li>' ).
    html->add( emoji->format( ':rocket:' ) ).
    html->add( |Publish & share your own ABAP code with a manifest and a version number, for the whole| ).
    html->add( |ecosystem to use| ).
    html->add( '</li>' ).
    html->add( '</ul>' ).
    html->add( '<p>' ).
    html->add( |No complex setup. One ABAP report. You're already up and running.| ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |Thanks for being here early. { apm } is new, so expect a few hiccups along the way, and please| ).
    html->add( |keep the feedback coming. This ecosystem grows because of developers like you.| ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |Ready to install your first package? Try the { tutorial }! Welcome aboard.| ).
    html->add( emoji->format( ':tada:' ) ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |Marc & the ABAP open-source community<br>| ).
    html->add( emoji->format( 'Made with :heart: in Canada' ) ).
    html->add( '</p>' ).
    html->add( '<p class="center">' ).
    html->add( emoji->format( ':arrow_down:' ) ).
    html->add( '</p>' ).
    html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
