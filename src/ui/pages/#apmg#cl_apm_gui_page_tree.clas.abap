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
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_hotkeys,
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
      settings TYPE /apmg/if_apm_settings=>ty_settings.

    " PREPARE

    METHODS build_table_scheme
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_gui_chunk_lib=>ty_col_spec_tt.

    " CONTENT

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

    METHODS render_table_footer
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    " EDGES

    METHODS render_edges_out
      IMPORTING
        !edges        TYPE /apmg/cl_apm_arborist_node=>ty_edges
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_edges_in
      IMPORTING
        !edges        TYPE /apmg/cl_apm_arborist_node=>ty_edges
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_edges_ranges
      IMPORTING
        !edges        TYPE /apmg/cl_apm_arborist_node=>ty_edges
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_edges_status
      IMPORTING
        !edges        TYPE /apmg/cl_apm_arborist_node=>ty_edges
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


  METHOD /apmg/if_apm_gui_hotkeys~get_hotkey_actions.

    DATA hotkey_action LIKE LINE OF rt_hotkey_actions.

    hotkey_action-ui_component = 'Package List'.

    hotkey_action-description = |Global Settings|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-go_settings.
    hotkey_action-hotkey      = |x|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Personal Settings|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-go_settings_personal.
    hotkey_action-hotkey      = |p|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Refresh|.
    hotkey_action-action      = c_action-refresh.
    hotkey_action-hotkey      = |r|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Init|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_init.
    hotkey_action-hotkey      = |t|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Install|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_install.
    hotkey_action-hotkey      = |i|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Uninstall|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_uninstall.
    hotkey_action-hotkey      = |u|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Publish|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_publish.
    hotkey_action-hotkey      = |p|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Unpublish|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_unpublish.
    hotkey_action-hotkey      = |q|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    " registered/handled in js
    hotkey_action-description = |Previous Package|.
    hotkey_action-action      = `#`.
    hotkey_action-hotkey      = |4|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Next Package|.
    hotkey_action-action      = `##`.
    hotkey_action-hotkey      = |6|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Show Package|.
    hotkey_action-action      = `###`.
    hotkey_action-hotkey      = |Enter|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Focus Filter|.
    hotkey_action-action      = `####`.
    hotkey_action-hotkey      = |f|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

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
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'VERSION'
      display_name   = 'Version'
      css_class      = 'version'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'PACKAGE'
      display_name   = 'Package'
      css_class      = 'package'
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

    tree = /apmg/cl_apm_arborist=>factory( settings-registry )->load_actual_tree( ).

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


  METHOD render_edges_in.

    CHECK edges IS NOT INITIAL.

    result = '<div class="pad-1em">'.

    result &&= 'Dependents'.

    result &&= '<div>'.

    LOOP AT edges ASSIGNING FIELD-SYMBOL(<edge>).
      result &&= |<br>{ <edge>->to->name } < { <edge>->from->name }|.
    ENDLOOP.

    result &&= '</div></div>'.

  ENDMETHOD.


  METHOD render_edges_out.

    CHECK edges IS NOT INITIAL.

    result = '<div class="pad-1em">'.

    result &&= 'Dependencies'.

    result &&= '<div>'.

    LOOP AT edges ASSIGNING FIELD-SYMBOL(<edge>).
      result &&= |<br>{ <edge>->from->name } > { <edge>->to->name }|.
    ENDLOOP.

    result &&= '</div></div>'.

  ENDMETHOD.


  METHOD render_edges_ranges.

    CHECK edges IS NOT INITIAL.

    result = '<div class="pad-1em">'.

    result &&= 'Ranges'.

    result &&= '<div>'.

    LOOP AT edges ASSIGNING FIELD-SYMBOL(<edge>).
      result &&= |<br>{ <edge>->name }: { <edge>->spec }|.
    ENDLOOP.

    result &&= '</div></div>'.

  ENDMETHOD.


  METHOD render_edges_status.

    CHECK edges IS NOT INITIAL.

    result = '<div class="pad-1em">'.

    result &&= 'Check'.

    result &&= '<div>'.

    LOOP AT edges ASSIGNING FIELD-SYMBOL(<edge>).
      IF <edge>->error IS INITIAL.
        result &&= |<br><span style="color:green">ok</span>|.
      ELSE.
        result &&= |<br><span style="color:red">{ <edge>->error }</span>|.
      ENDIF.
    ENDLOOP.

    result &&= '</div></div>'.

  ENDMETHOD.


  METHOD render_filter_bar.

    html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).
    html->add( /apmg/cl_apm_gui_chunk_lib=>render_text_input(
      iv_name      = |filter|
      iv_label     = |Filter: { render_filter_help_hint( ) }|
      iv_value     = settings-tree_settings-filter ) ).
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


  METHOD render_package_tree.

    html->add( |<table>| ).

    render_table_header( html ).
    render_table_body( html ).
    render_table_footer( html ).

    html->add( |</table>| ).

  ENDMETHOD.


  METHOD render_registry.

    IF settings-registry = /apmg/if_apm_settings=>c_registry.
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
      table = /apmg/cl_apm_emoji=>create( )->get_emoji_css( )
      sep   = cl_abap_char_utilities=>newline ).

    html->add( '<style>' ).
    html->add( emoji_styles ).
    html->add( '</style>' ).

  ENDMETHOD.


  METHOD render_table_body.

    html->add( '<tbody>' ).

    LOOP AT tree ASSIGNING FIELD-SYMBOL(<node>).
      render_table_item(
        html = html
        node = <node> ).
    ENDLOOP.

    html->add( |</tbody>| ).

  ENDMETHOD.


  METHOD render_table_footer.

  ENDMETHOD.


  METHOD render_table_header.

    html->add( /apmg/cl_apm_gui_chunk_lib=>render_table_header(
      it_col_spec         = build_table_scheme( )
      iv_order_by         = settings-tree_settings-order_by
      iv_order_descending = settings-tree_settings-order_descending ) ).

  ENDMETHOD.


  METHOD render_table_item.

    " Start of row
    html->add( |<tr data-key="{ node->package }">| ).

    " Name
    html->td(
      iv_content =
        html->a(
          iv_txt = node->name
          iv_act = |{ c_action-select }?key={ node->package }| )
        && render_edges_out( node->edges_out )
        && render_edges_in( node->edges_in )
      iv_class = 'top' ).

    " Version
    html->td(
      iv_content =
        html->a(
          iv_txt = node->version
          iv_act = |{ c_action-select }?key={ node->package }| )
        && render_edges_ranges( node->edges_out )
        && render_edges_ranges( node->edges_in )
      iv_class = 'top' ).

    " Package
    html->td(
      ii_content = /apmg/cl_apm_gui_chunk_lib=>render_package_name( node->package )
      iv_class   = 'top' ).

    " Status
    IF node->errors IS INITIAL.
      html->td(
        iv_content = |<span class="boxed green-filled-set">OK</span>|
                     && render_edges_status( node->edges_out )
                     && render_edges_status( node->edges_in )
        iv_class   = 'top' ).
    ELSE.
      DATA(error_count) = lines( node->errors ).
      DATA(error_text) = 'error'.
      IF error_count <> 1.
        error_text = 'errors'.
      ENDIF.
      html->td(
        iv_content = |<span class="boxed red-filled-set">{ error_count } { error_text }</span>|
                     && render_edges_status( node->edges_out )
                     && render_edges_status( node->edges_in )
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

    html->add( `</tr>` ).

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
