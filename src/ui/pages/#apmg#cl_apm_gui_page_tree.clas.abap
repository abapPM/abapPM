CLASS /apmg/cl_apm_gui_page_tree DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Package Tree
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* CSS classes have to match repo-overview to allow JS to work properly
* TODO: move tree to tree_list
* TODO: apply filter and sorting to tree_list
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

    CONSTANTS:
      c_page_id TYPE string VALUE 'apm-package-tree',
      BEGIN OF c_action,
        select           TYPE string VALUE 'select',
        apply_filter     TYPE string VALUE 'apply_filter',
        label_filter     TYPE string VALUE 'label_filter',
        toggle_favorites TYPE string VALUE 'toggle_favorites',
        change_order_by  TYPE string VALUE 'change_order_by',
        direction        TYPE string VALUE 'direction',
        refresh          TYPE string VALUE 'refresh',
      END OF c_action,
      c_raw_field_suffix TYPE string VALUE '_RAW' ##NO_TEXT.

    DATA:
      tree     TYPE /apmg/if_apm_arborist=>ty_node_refs,
      log      TYPE /apmg/if_apm_arborist=>ty_log,
      settings TYPE /apmg/if_apm_settings=>ty_settings.

    " PREPARE

    METHODS build_table_scheme
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_gui_chunk_lib=>ty_col_spec_tt.

    " CONTENT

    METHODS render_log
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_package_tree
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_header
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_table_body
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_item
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
        !node TYPE /apmg/if_apm_arborist=>ty_node_ref
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_errors
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
        !node TYPE /apmg/if_apm_arborist=>ty_node_ref
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_edges_in
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
        !node TYPE /apmg/if_apm_arborist=>ty_node_ref
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_edges_out
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
        !node TYPE /apmg/if_apm_arborist=>ty_node_ref
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_edges_fill
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_footer
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    " EDGES

    METHODS render_edges
      IMPORTING
        !edges        TYPE /apmg/cl_apm_arborist_node=>ty_edges
        !view         TYPE i
      RETURNING
        VALUE(result) TYPE string.

    " HEADER

    METHODS render_action_toolbar
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_header_bar
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_filter_bar
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_filter_help_hint
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_registry
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    " CSS + JS

    METHODS render_styles
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS get_scripts
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    " ACTIONS

    METHODS set_order_by
      IMPORTING
        !order_by TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS set_order_direction
      IMPORTING
        !order_descending TYPE abap_bool
      RAISING
        /apmg/cx_apm_error.

    METHODS set_filter
      IMPORTING
        !postdata TYPE /apmg/if_apm_html_viewer=>ty_post_data
      RAISING
        /apmg/cx_apm_error.

    METHODS apply_filter
      CHANGING
        !packages TYPE /apmg/if_apm_package_json=>ty_packages.

    METHODS apply_order_by
      CHANGING
        packages TYPE /apmg/if_apm_package_json=>ty_packages.

    " INIT

    METHODS load_package_tree
      RAISING
        /apmg/cx_apm_error.

    METHODS load_settings
      RAISING
        /apmg/cx_apm_error.

    METHODS save_settings
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_page_tree IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    DATA(package) = CONV devclass( ii_event->query( )->get( 'KEY' ) ).

    CASE ii_event->mv_action.
      WHEN c_action-refresh.

        load_package_tree( ).

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-select.

        settings-last_package = package.
        save_settings( ).

        rs_handled-page  = /apmg/cl_apm_gui_page_package=>create( package ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN c_action-change_order_by.

        set_order_by( ii_event->query( )->get( 'ORDERBY' ) ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-direction.

        set_order_direction( xsdbool( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-apply_filter.

        set_filter( ii_event->mt_postdata ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_menu_provider~get_menu.

    DATA(toolbar) = /apmg/cl_apm_html_toolbar=>create( 'apm-package-tree' ).

    toolbar->add(
      iv_txt = 'Back'
      iv_act = /apmg/if_apm_gui_router=>c_action-go_back ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    load_settings( ).
    load_package_tree( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    render_styles( html ).
    render_log( html ).

    html->add( |<div class="repo-overview">| ).

    render_header_bar( html ).
    render_package_tree( html ).

    html->add( |</div>| ).

    register_deferred_script( get_scripts( ) ).

    ri_html = html.

  ENDMETHOD.


  METHOD apply_filter.

    IF settings-tree_settings-filter IS INITIAL.
      RETURN.
    ENDIF.

    DELETE packages
      WHERE package    NS settings-tree_settings-filter
        AND name       NS settings-tree_settings-filter
        AND version    NS settings-tree_settings-filter
        AND changed_by NS settings-tree_settings-filter.

  ENDMETHOD.


  METHOD apply_order_by.

    DATA:
      sort_order      TYPE abap_sortorder_tab,
      sort_order_item LIKE LINE OF sort_order.

    IF settings-tree_settings-order_by IS NOT INITIAL.
      CLEAR sort_order_item.

      IF settings-tree_settings-order_by = 'CHANGED_AT'.
        sort_order_item-name = settings-tree_settings-order_by && c_raw_field_suffix.
      ELSE.
        sort_order_item-name   = settings-tree_settings-order_by.
        sort_order_item-astext = abap_true.
      ENDIF.

      sort_order_item-descending = settings-tree_settings-order_descending.
      INSERT sort_order_item INTO TABLE sort_order.
    ENDIF.

    SORT packages BY (sort_order).

  ENDMETHOD.


  METHOD build_table_scheme.

    DATA(table_schema) = NEW lcl_table_scheme( ).

    table_schema->add_column(
      tech_name      = 'NAME'
      display_name   = 'Name'
      css_class      = 'name'
      width          = '25%'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'VERSION'
      display_name   = 'Version'
      css_class      = 'version'
      width          = '25%'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'PACKAGE'
      display_name   = 'Package'
      css_class      = 'package'
      width          = '40%'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'STATUS'
      display_name   = 'Status'
      css_class      = 'status'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'DEPENDENCIES'
      display_name   = 'Deps'
      css_class      = 'ro-detail count'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'DEV_DEPENDENCIES'
      display_name   = 'devDeps'
      css_class      = 'ro-detail count'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'OPTIONAL_DEPENDENCIES'
      display_name   = 'optDeps'
      css_class      = 'ro-detail count'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'PEER_DEPENDENCIES'
      display_name   = 'peerDeps'
      css_class      = 'ro-detail count'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'BUNDLE_DEPENDENCIES'
      display_name   = 'bundleDeps'
      css_class      = 'ro-detail count'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'KEY'
      display_name   = 'Key'
      css_class      = 'ro-detail nodisplay'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'GO'
      css_class      = 'ro-go wmin'
      allow_order_by = abap_false ).

    result = table_schema->columns.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    load_settings( ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_page_tree( ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title         = 'Package Tree'
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD get_scripts.

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    html->add( |var gHelper = new RepoOverViewHelper(\{ focusFilterKey: "f", pageId: "{ c_page_id }" \});| ).

    result = html.

  ENDMETHOD.


  METHOD load_package_tree.

    DATA(arborist) = /apmg/cl_apm_arborist=>factory( settings-registry ).

    tree = arborist->load_actual_tree( ).
    log  = arborist->get_log( ).

  ENDMETHOD.


  METHOD load_settings.

    TRY.
        settings = /apmg/cl_apm_settings=>factory( )->get( ).
      CATCH /apmg/cx_apm_error.
        " Settings didn't exist, so save the defaults
        /apmg/cl_apm_settings=>factory( )->set( settings )->save( ).
    ENDTRY.

  ENDMETHOD.


  METHOD render_action_toolbar.

    " FUTURE
    html->add( '' ).

  ENDMETHOD.


  METHOD render_edges.

    CONSTANTS c_spacer TYPE string VALUE `<span>&nbsp;</span>`.

    CHECK edges IS NOT INITIAL.

    result = '<div class="pad-1em">'.

    result &&= '<strong>'.

    CASE view.
      WHEN 1.
        result &&= 'Dependencies'.
      WHEN 2.
        result &&= 'Dependents'.
      WHEN 3.
        result &&= 'Ranges'.
      WHEN 4.
        result &&= 'Messages'.
      WHEN 5.
        result &&= 'Checks'.
    ENDCASE.

    result &&= '</strong>'.

    result &&= '<div>'.

    LOOP AT edges ASSIGNING FIELD-SYMBOL(<edge>).
      result &&= '<br>'.
      CASE view.
        WHEN 1.
          IF <edge>->to IS INITIAL.
            result &&= c_spacer.
          ELSE.
            result &&= <edge>->to->name.
          ENDIF.
        WHEN 2.
          IF <edge>->from IS INITIAL.
            result &&= c_spacer.
          ELSE.
            result &&= <edge>->from->name.
          ENDIF.
        WHEN 3.
          result &&= |{ <edge>->name }: { <edge>->spec }|.
        WHEN 4.
          IF <edge>->error IS INITIAL.
            result &&= c_spacer.
          ELSE.
            result &&= |<span class="red">{ <edge>->get_error_description( ) }</span>|.
          ENDIF.
        WHEN 5.
          IF <edge>->error IS INITIAL.
            result &&= |<span style="color:green">ok</span>|.
          ELSE.
            result &&= |<span style="color:red">{ <edge>->error }</span>|.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    result &&= '</div></div>'.

  ENDMETHOD.


  METHOD render_filter_bar.

    html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).
    html->add( /apmg/cl_apm_gui_chunk_lib=>render_text_input(
      iv_name  = |filter|
      iv_label = |Filter: { render_filter_help_hint( ) }|
      iv_value = settings-tree_settings-filter ) ).
    html->add( |<input type="submit" class="hidden-submit">| ).
    html->add( |</form>| ).

    html->add( '<span class="toolbar-light pad-sides">' ).
    html->add( html->a(
      iv_txt   = '<i id="icon-filter-detail" class="icon icon-check"></i> Detail'
      iv_act   = |gHelper.toggleRepoListDetail()|
      iv_class = 'command'
      iv_typ   = /apmg/if_apm_html=>c_action_type-onclick ) ).
    html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_filter_help_hint.

    DATA fragments TYPE string_table.

    APPEND `Filter is applied to all text fields in the below table.` TO fragments.
    APPEND ` Search works for any portion of the text (so can be a mid part as well).` TO fragments.

    result = /apmg/cl_apm_gui_chunk_lib=>render_help_hint( concat_lines_of( table = fragments ) ).

  ENDMETHOD.


  METHOD render_header_bar.

    html->add( |<div class="repo-overview-toolbar">| ).

    render_filter_bar( html ).
    render_registry( html ).
    render_action_toolbar( html ).

    html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_log.

    html->add( '<div>' ).

    " TODO: Improve formatting/color by type
    LOOP AT log ASSIGNING FIELD-SYMBOL(<log>).
      html->add( |{ <log>-type } { <log>-message }<br>| ).
    ENDLOOP.

    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_package_tree.

    html->add( |<table>| ).

    render_table_header( html ).
    render_table_body( html ).
    render_table_footer( html ).

    html->add( |</table>| ).

  ENDMETHOD.


  METHOD render_registry.

    IF settings-registry = /apmg/if_apm_constants=>c_registry.
      DATA(fav_class) = 'transport-box'. " green
    ELSE.
      fav_class = 'user-box'. " blue
    ENDIF.

    html->add( '<span style="float:right">' ).
    html->add( |<span class="{ fav_class }">| ).
    html->add_a(
      iv_title = 'Registry'
      iv_txt   = settings-registry
      iv_act   = |{ /apmg/if_apm_gui_router=>c_action-url }?url={ settings-registry }| ).
    html->add( '</span>' ).
    html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_styles.

    " Emoji Styles
    DATA(emoji_styles) = concat_lines_of(
      table = /apmg/cl_apm_emoji=>create( )->get_css( )
      sep   = cl_abap_char_utilities=>newline ).

    html->add( '<style>' ).
    html->add( emoji_styles ).
    html->add( 'tr.border-top td { border-top: 1px solid darkgray; }' ).
    html->add( '</style>' ).

  ENDMETHOD.


  METHOD render_table_body.

    html->add( '<tbody>' ).

    LOOP AT tree ASSIGNING FIELD-SYMBOL(<node>).
      render_table_item(
        html = html
        node = <node> ).
      render_table_errors(
        html = html
        node = <node> ).
      " Dependencies
      render_table_edges_out(
        html = html
        node = <node> ).
      " Dependents
      render_table_edges_in(
        html = html
        node = <node> ).
    ENDLOOP.

    html->add( '</tbody>' ).

  ENDMETHOD.


  METHOD render_table_edges_fill.

    html->td(
      iv_class     = 'ro-detail top'
      is_data_attr = VALUE #( name = 'colspan' value = 5 )
      iv_content   = '' ).

    html->td(
      iv_class   = 'ro-detail nodisplay top'
      iv_content = '' ).

    html->td(
      iv_class   = 'ro-go wmin top'
      iv_content = '' ).

  ENDMETHOD.


  METHOD render_table_edges_in.

    CHECK node->edges_in IS NOT INITIAL.

    html->add( '<tr>' ).

    " Name
    html->td(
      iv_content = render_edges( edges = node->edges_in view = 2 )
      iv_class   = 'top' ).

    " Version
    html->td(
      iv_content = render_edges( edges = node->edges_in view = 3 )
      iv_class   = 'top' ).

    " Messages
    html->td(
      iv_content = render_edges( edges = node->edges_in view = 4 )
      iv_class   = 'top' ).

    " Status
    html->td(
      iv_content = render_edges( edges = node->edges_in view = 5 )
      iv_class   = 'top' ).

    " Remaining columns
    render_table_edges_fill( html ).

    html->add( '</tr>' ).

  ENDMETHOD.


  METHOD render_table_edges_out.

    CHECK node->edges_out IS NOT INITIAL.

    html->add( '<tr>' ).

    " Name
    html->td(
      iv_content = render_edges( edges = node->edges_out view = 1 )
      iv_class   = 'top' ).

    " Version
    html->td(
      iv_content = render_edges( edges = node->edges_out view = 3 )
      iv_class   = 'top' ).

    " Messages
    html->td(
      iv_content = render_edges( edges = node->edges_out view = 4 )
      iv_class   = 'top' ).

    " Status
    html->td(
      iv_content = render_edges( edges = node->edges_out view = 5 )
      iv_class   = 'top' ).

    " Remaining columns
    render_table_edges_fill( html ).

    html->add( '</tr>' ).

  ENDMETHOD.


  METHOD render_table_errors.

    CHECK node->errors IS NOT INITIAL.

    DATA(out) = ``.
    LOOP AT node->errors ASSIGNING FIELD-SYMBOL(<error>).
      IF sy-tabix > 1.
        out &&= '<br>'.
      ENDIF.
      out &&= <error>.
    ENDLOOP.

    html->add( '<tr>' ).

    html->td(
      iv_content   = out
      is_data_attr = VALUE #( name = 'colspan' value = 4 )
      iv_class     = 'top red' ).

    " Remaining columns
    render_table_edges_fill( html ).

    html->add( '</tr>' ).

  ENDMETHOD.


  METHOD render_table_footer.
    ASSERT 1 = 1.
  ENDMETHOD.


  METHOD render_table_header.

    html->add( /apmg/cl_apm_gui_chunk_lib=>render_table_header(
      it_col_spec         = build_table_scheme( )
      iv_order_by         = settings-tree_settings-order_by
      iv_order_descending = settings-tree_settings-order_descending ) ).

  ENDMETHOD.


  METHOD render_table_item.

    " Start of row
    html->add( |<tr data-key="{ node->package }" class="border-top">| ).

    " Name
    html->td(
      iv_content =
        html->a(
          iv_txt = node->name
          iv_act = |{ c_action-select }?key={ node->package }| )
      iv_class = 'top' ).

    " Version
    html->td(
      iv_content =
        html->a(
          iv_txt = node->version
          iv_act = |{ c_action-select }?key={ node->package }| )
      iv_class = 'top' ).

    " Package
    " FIX: node->installed flag should be evaluated but it's not correct in arborist
    IF node->package IS INITIAL.
      DATA(package) = /apmg/cl_apm_gui_chunk_lib=>render_package_name(
        iv_package     = 'N/A'
        iv_interactive = abap_false ).
    ELSE.
      package = /apmg/cl_apm_gui_chunk_lib=>render_package_name( node->package ).
    ENDIF.

    html->td(
      iv_content = package->render( )
      iv_class   = 'top' ).

    " Status
    IF node->errors IS INITIAL.
      html->td(
        iv_content = |<span class="boxed green-filled-set">OK</span>|
        iv_class   = 'top' ).
    ELSE.
      DATA(error_count) = lines( node->errors ).
      DATA(error_text) = `error`.
      IF error_count <> 1.
        error_text = `errors`.
      ENDIF.
      html->td(
        iv_content = |<span class="boxed red-filled-set">{ error_count } { error_text }</span>|
        iv_class   = 'top' ).
    ENDIF.

    " Dependency counts
    html->td(
      iv_class   = 'ro-detail top'
      iv_content = |{ lines( node->dependencies ) }| ).
    html->td(
      iv_class   = 'ro-detail top'
      iv_content = |{ lines( node->dev_dependencies ) }| ).
    html->td(
      iv_class   = 'ro-detail top'
      iv_content = |{ lines( node->optional_dependencies ) }| ).
    html->td(
      iv_class   = 'ro-detail top'
      iv_content = |{ lines( node->peer_dependencies ) }| ).
    html->td(
      iv_class   = 'ro-detail top'
      iv_content = |{ lines( node->bundle_dependencies ) }| ).

    " Details: key for navigation
    html->td(
      iv_class   = 'ro-detail nodisplay top'
      iv_content = |{ node->package }| ).

    " Go-to action
    html->td(
      iv_class   = 'ro-go wmin top'
      iv_content = html->a(
        iv_title = 'Open'
        iv_txt   = '&rtrif;'
        iv_act   = |{ c_action-select }?key={ node->package }| ) ).

    html->add( '</tr>' ).

  ENDMETHOD.


  METHOD save_settings.

    /apmg/cl_apm_settings=>factory( )->set( settings )->save( ).

  ENDMETHOD.


  METHOD set_filter.

    READ TABLE postdata ASSIGNING FIELD-SYMBOL(<postdata>) INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX 'filter=(.*)'
        IN <postdata>
        SUBMATCHES settings-tree_settings-filter ##SUBRC_OK ##REGEX_POSIX.
    ENDIF.

    settings-tree_settings-filter = condense( settings-tree_settings-filter ).
    save_settings( ).

  ENDMETHOD.


  METHOD set_order_by.

    IF settings-tree_settings-order_by <> order_by.
      set_order_direction( abap_false ). " Reset ordering
    ENDIF.

    settings-tree_settings-order_by = order_by.
    save_settings( ).

  ENDMETHOD.


  METHOD set_order_direction.

    settings-tree_settings-order_descending = order_descending.
    save_settings( ).

  ENDMETHOD.
ENDCLASS.
