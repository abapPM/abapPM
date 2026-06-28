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

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        setup   TYPE string VALUE 'setup',
        refresh TYPE string VALUE 'refresh',
      END OF c_action.

    CONSTANTS c_ping_pong TYPE string VALUE 'PONG'.

    METHODS render_styles
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

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

      WHEN c_action-setup.

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

    render_styles( html ).

    render_welcome( html ).
    render_connections( html ).

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


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_page_welcome( ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title         = 'Welcome!'
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD render_connections.

    DATA(emoji) = /apmg/cl_apm_emoji=>create( ).

    html->add( '<div style="padding:10px 150px 30px;font-size:large;">' ).
    html->add( '<h3>' ).
    html->add( 'Connection Check' ).
    html->add( '</h3>' ).

    html->add( '<table class="repo_tab w100 paddings">' ).
    html->add( '<tbody>' ).

    DATA(missing_certificates) = abap_false.

    DO 2 TIMES.
      IF sy-index = 2.
        DATA(name)     = 'Playground'.
        DATA(registry) = /apmg/if_apm_constants=>c_playground.
        DATA(action)   = /apmg/if_apm_gui_router=>c_action-playground.
      ELSE.
        name     = 'Registry'.
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
      html->td( name ).
      html->td( html->a( iv_txt = registry iv_act = action ) ).

      IF ping = c_ping_pong.
        html->td( emoji->format( ':heavy_check_mark:' ) ).
      ELSE.
        html->td( emoji->format( ':x:' ) ).
        html->add( '<tr>' ).
        html->td( '' ).
        html->td(
          iv_content   = ping
          is_data_attr = VALUE #( name = 'colspan' value = 2 ) ).
        html->add( '</tr>' ).
        html->add( '</tr>' ).
      ENDIF.
    ENDDO.

    IF missing_certificates = abap_true.
      html->add( '<tr>' ).
      html->td( '' ).
      html->td(
        iv_content   = html->a(
          iv_txt = 'Install missing certificates...'
          iv_act = c_action-setup )
        is_data_attr = VALUE #( name = 'colspan' value = 2 ) ).
      html->add( '</tr>' ).
    ENDIF.

    html->add( '</tbody>' ).
    html->add( '</table>' ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_styles.

    " Emoji Styles
    DATA(emoji_styles) = concat_lines_of(
      table = /apmg/cl_apm_emoji=>create( )->get_css( )
      sep   = cl_abap_char_utilities=>newline ).

    html->add( '<style>' ).
    html->add( emoji_styles ).
    html->add( '</style>' ).

  ENDMETHOD.


  METHOD render_welcome.

    DATA(emoji) = /apmg/cl_apm_emoji=>create( ).

    DATA(apm) = |<strong>apm</strong>|.

    DATA(first_package) = html->a(
      iv_txt   = 'first package'
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
    html->add( |abapGit gave ABAP its git. For over 10 years we could share code we but were missing solid no| ).
    html->add( |versioning, dependencies, and automatic installs. We had git, but no npm.| ).
    html->add( |Now, finally, ABAP has both.| ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |That's why I built { apm } and why a growing community of ABAP open-source developers| ).
    html->add( |is building it with you. Whatever you do here, you're now part of that.| ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |From here you can:| ).
    html->add( '</p>' ).
    html->add( '<ul>' ).
    html->add( '<li>' ).
    html->add( emoji->format( ':mag_right:' ) ).
    html->add( |Browse the registry and find packages to solve problems you'd otherwise build from scratch| ).
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
    html->add( |Now go install your { first_package }. Welcome aboard.| ).
    html->add( emoji->format( ':tada:' ) ).
    html->add( '</p>' ).
    html->add( '<p>' ).
    html->add( |Marc & the ABAP open-source community<br>| ).
    html->add( emoji->format( 'Made with :heart: in Canada' ) ).
    html->add( '</p>' ).
    html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
