CLASS /apmg/cl_apm_gui_page_list DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Package List
*
* Copyright 2024 apm.to Inc. <https://apm.to>
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
      IMPORTING
        !only_favorites TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)   TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !only_favorites TYPE abap_bool OPTIONAL
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        select           TYPE string VALUE 'select',
        apply_filter     TYPE string VALUE 'apply_filter',
        label_filter     TYPE string VALUE 'label_filter',
        toggle_favorites TYPE string VALUE 'toggle_favorites',
        change_order_by  TYPE string VALUE 'change_order_by',
        direction        TYPE string VALUE 'direction',
        refresh          TYPE string VALUE 'refresh',
      END OF c_action,
      c_label_filter_prefix TYPE string VALUE 'label:',
      c_raw_field_suffix    TYPE string VALUE '_RAW' ##NO_TEXT.

    DATA:
      packages     TYPE /apmg/if_apm_package_json=>ty_packages,
      all_labels   TYPE string_table,
      label_colors TYPE REF TO /apmg/cl_apm_string_map,
      settings     TYPE /apmg/if_apm_settings=>ty_settings.

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

    METHODS render_package_list
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS get_palette
      IMPORTING
        !action       TYPE string
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_styles
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_table_header
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_table_footer
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_table_body
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_table_item
      IMPORTING
        !html    TYPE REF TO /apmg/if_apm_html
        !package TYPE /apmg/if_apm_package_json=>ty_package
      RAISING
        /apmg/cx_apm_error.

    METHODS render_header_bar
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_header_label_list
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS apply_order_by
      CHANGING
        packages TYPE /apmg/if_apm_package_json=>ty_packages.

    METHODS prepare_packages
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_package_json=>ty_packages
      RAISING
        /apmg/cx_apm_error.

    METHODS get_scripts
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_action_toolbar
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_filter_bar
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS render_registry
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html.

    METHODS build_table_scheme
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_gui_chunk_lib=>ty_col_spec_tt.

    METHODS collect_all_labels
      IMPORTING
        !packages     TYPE /apmg/if_apm_package_json=>ty_packages
      RETURNING
        VALUE(result) TYPE string_table.

    METHODS render_filter_help_hint
      RETURNING
        VALUE(result) TYPE string.

    METHODS load_package_list
      RAISING
        /apmg/cx_apm_error.

    METHODS load_settings
      RAISING
        /apmg/cx_apm_error.

    METHODS save_settings
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_page_list IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    DATA(package) = CONV devclass( ii_event->query( )->get( 'KEY' ) ).

    CASE ii_event->mv_action.
      WHEN c_action-refresh.

        load_package_list( ).

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-select.

        settings-last_package = package.
        save_settings( ).

        rs_handled-page  = /apmg/cl_apm_gui_page_package=>create( package ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-new_page.

      WHEN c_action-change_order_by.

        set_order_by( ii_event->query( )->get( 'ORDERBY' ) ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-toggle_favorites.

        IF ii_event->query( )->has( 'FORCE_STATE' ) = abap_true.
          settings-list_settings-only_favorites = ii_event->query( )->get( 'FORCE_STATE' ).
        ELSE.
          settings-list_settings-only_favorites = xsdbool( settings-list_settings-only_favorites = abap_false ).
        ENDIF.
        save_settings( ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-direction.

        set_order_direction( xsdbool( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-apply_filter.

        set_filter( ii_event->mt_postdata ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-label_filter.

        IF ii_event->mv_getdata IS NOT INITIAL.
          settings-list_settings-filter = c_label_filter_prefix && ii_event->mv_getdata.
        ELSE.
          CLEAR settings-list_settings-filter. " Unexpected request
        ENDIF.
        save_settings( ).
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

    CONSTANTS:
      c_dummy_key    TYPE string VALUE `?key=#`,
      c_action_class TYPE string VALUE `action_link`.

    DATA(commands) = /apmg/cl_apm_html_toolbar=>create( 'apm-package-list-commands' ).

    commands->add(
      iv_txt      = 'Deprecate'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_deprecate }{ c_dummy_key }|
      iv_class    = c_action_class
      iv_li_class = c_action_class
    )->add(
      iv_txt      = 'Undeprecate'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_undeprecate }{ c_dummy_key }|
      iv_class    = c_action_class
      iv_li_class = c_action_class
    )->add(
      iv_txt      = 'Danger'
      iv_typ      = /apmg/if_apm_html=>c_action_type-separator
    )->add(
      iv_txt      = 'Unpublish'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_unpublish }{ c_dummy_key }|
      iv_class    = |{ c_action_class } red|
      iv_li_class = c_action_class
    )->add(
      iv_txt      = 'Uninstall'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_uninstall }{ c_dummy_key }|
      iv_class    = |{ c_action_class } red|
      iv_li_class = c_action_class ).

    DATA(toolbar) = /apmg/cl_apm_html_toolbar=>create( 'apm-package-list' ).

    toolbar->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'file' ) && ' Init'
      iv_act      = /apmg/if_apm_gui_router=>c_action-apm_init
    )->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'download-solid' ) && ' Install'
      iv_act      = /apmg/if_apm_gui_router=>c_action-apm_install
    )->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'upload-solid' ) && ' Publish'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_publish }{ c_dummy_key }|
      iv_class    = c_action_class
      iv_li_class = c_action_class
    )->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'chevron-right' ) && ' Commands'
      io_sub      = commands
    )->add(
      iv_txt      = /apmg/cl_apm_gui_buttons=>settings( )
      io_sub      = /apmg/cl_apm_gui_menus=>settings( )
    )->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'redo-alt-solid' )
      iv_act      = c_action-refresh
    )->add(
      iv_txt      = /apmg/cl_apm_gui_buttons=>advanced( )
      io_sub      = /apmg/cl_apm_gui_menus=>advanced( )
    )->add(
      iv_txt      = /apmg/cl_apm_gui_buttons=>help( )
      io_sub      = /apmg/cl_apm_gui_menus=>help( ) ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    " FUTURE
    " label_colors = zcl_abapgit_repo_labels=>split_colors_into_map( settings-gui_settings-label_colors )

    load_settings( ).
    load_package_list( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    render_styles( html ).

    html->add( |<div class="repo-overview">| ).

    render_header_bar( html ).
    render_header_label_list( html ).
    render_package_list( html ).

    html->add( |</div>| ).

    register_deferred_script( get_scripts( ) ).
    register_deferred_script( get_palette( c_action-select ) ).

    ri_html = html.

  ENDMETHOD.


  METHOD apply_filter.

    IF settings-list_settings-filter IS INITIAL.
      RETURN.
    ENDIF.

    DATA(prefix_length) = strlen( c_label_filter_prefix ).

    IF strlen( settings-list_settings-filter ) > prefix_length AND
        settings-list_settings-filter+0(prefix_length) = c_label_filter_prefix.
      DATA(filter_label) = settings-list_settings-filter+prefix_length.

      CASE filter_label.
        WHEN 'all'.
          DELETE packages WHERE labels IS INITIAL.
        WHEN 'none'.
          DELETE packages WHERE labels IS NOT INITIAL.
        WHEN OTHERS.
          LOOP AT packages ASSIGNING FIELD-SYMBOL(<package>).
            IF line_exists( <package>-labels[ filter_label ] ).
              DELETE packages INDEX sy-tabix.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ELSE. " Regular filter
      DELETE packages
        WHERE package    NS settings-list_settings-filter
          AND name       NS settings-list_settings-filter
          AND version    NS settings-list_settings-filter
          AND changed_by NS settings-list_settings-filter.
    ENDIF.

  ENDMETHOD.


  METHOD apply_order_by.

    DATA:
      sort_order      TYPE abap_sortorder_tab,
      sort_order_item LIKE LINE OF sort_order.

    sort_order_item-name       = 'FAVORITE'.
    sort_order_item-descending = abap_true.
    sort_order_item-astext     = abap_true.
    INSERT sort_order_item INTO TABLE sort_order.

    IF settings-list_settings-order_by IS NOT INITIAL.
      CLEAR sort_order_item.

      IF settings-list_settings-order_by = 'CHANGED_AT'.
        sort_order_item-name = settings-list_settings-order_by && c_raw_field_suffix.
      ELSE.
        sort_order_item-name   = settings-list_settings-order_by.
        sort_order_item-astext = abap_true.
      ENDIF.

      sort_order_item-descending = settings-list_settings-order_descending.
      INSERT sort_order_item INTO TABLE sort_order.
    ENDIF.

    SORT packages BY (sort_order).

  ENDMETHOD.


  METHOD build_table_scheme.

    DATA(table_schema) = NEW lcl_table_scheme( ).

    table_schema->add_column(
      tech_name      = 'FAVORITE'
      css_class      = 'wmin'
      allow_order_by = abap_false
    )->add_column(
      tech_name      = 'PACKAGE'
      display_name   = 'Package'
      css_class      = 'package'
      allow_order_by = abap_true ).

    IF all_labels IS NOT INITIAL.
      table_schema->add_column(
        tech_name      = 'LABELS'
        display_name   = 'Labels'
        allow_order_by = abap_false ).
    ENDIF.

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
      tech_name      = 'DESCRIPTION'
      display_name   = 'Description'
      css_class      = 'description'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'CHANGED_BY'
      display_name   = 'Changed by'
      css_class      = 'ro-detail'
      allow_order_by = abap_true
    )->add_column(
      tech_name      = 'CHANGED_AT'
      display_name   = 'Changed at'
      css_class      = 'ro-detail'
      add_tz         = abap_true
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


  METHOD collect_all_labels.

    LOOP AT packages ASSIGNING FIELD-SYMBOL(<package>).
      APPEND LINES OF <package>-labels TO result.
    ENDLOOP.

    SORT result.
    DELETE result WHERE table_line IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM result.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    load_settings( ).

    " Overwrite setting
    IF only_favorites = abap_true.
      settings-list_settings-only_favorites = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW /apmg/cl_apm_gui_page_list( only_favorites ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title         = 'Package List'
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD get_palette.

    DATA(package_list)  = packages.
    DATA(package_count) = lines( package_list ).
    SORT package_list BY description AS TEXT.

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->add( 'var repoCatalog = [' ).
    LOOP AT package_list ASSIGNING FIELD-SYMBOL(<package>).
      DATA(display_name) = escape(
        val    = <package>-description
        format = cl_abap_format=>e_html_js ) &&
        |  ({ <package>-package }, { <package>-name }@{ <package>-version })|.
      DATA(json) = |\{|
        && | key: "{ <package>-package }",|
        && | isOffline: "",|
        && | displayName: "{ display_name }"|
        && | \}|.
      IF sy-tabix < package_count.
        json = json && ','.
      ENDIF.
      html->add( json ).
    ENDLOOP.
    html->add( '];' ).

    html->add( 'var gGoRepoPalette = new CommandPalette(' ).
    html->add( |  createRepoCatalogEnumerator(repoCatalog, "{ action }"), \{| ).
    html->add( '  toggleKey: "F2",' ).
    html->add( '  hotkeyDescription: "Go to Package"' ).
    html->add( '});' ).

    result = html.

  ENDMETHOD.


  METHOD get_scripts.

    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    html->add( 'var gHelper = new RepoOverViewHelper({ focusFilterKey: "f", pageId: "apm-list" });' ).

    result = html.

  ENDMETHOD.


  METHOD load_package_list.

    packages = /apmg/cl_apm_package_json=>list(
      instanciate = abap_true
      is_bundle   = abap_false ).

  ENDMETHOD.


  METHOD load_settings.

    TRY.
        settings = /apmg/cl_apm_settings=>factory( )->get( ).
      CATCH /apmg/cx_apm_error.
        " Settings didn't exist, so save the defaults
        /apmg/cl_apm_settings=>factory( )->set( settings )->save( ).
    ENDTRY.

  ENDMETHOD.


  METHOD prepare_packages.

    result = packages.

    LOOP AT result ASSIGNING FIELD-SYMBOL(<package>).
      READ TABLE settings-package_settings ASSIGNING FIELD-SYMBOL(<settings>)
        WITH TABLE KEY package = <package>-package.
      IF sy-subrc = 0.
        <package>-favorite        = <settings>-favorite.
        <package>-labels          = <settings>-labels.
        <package>-write_protected = <settings>-write_protected.
      ENDIF.
    ENDLOOP.

    IF settings-list_settings-only_favorites = abap_true.
      DELETE result WHERE favorite = abap_false.
    ENDIF.

    " Hmmm, side effect, not ideal, but we need label list before filter applied
    all_labels = collect_all_labels( result ).

    apply_order_by( CHANGING packages = result ).
    apply_filter( CHANGING packages = result ).

  ENDMETHOD.


  METHOD render_action_toolbar.

    " FUTURE
    html->add( '' ).

  ENDMETHOD.


  METHOD render_filter_bar.

    html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).
    html->add( /apmg/cl_apm_gui_chunk_lib=>render_text_input(
      iv_name      = |filter|
      iv_label     = |Filter: { render_filter_help_hint( ) }|
      iv_value     = settings-list_settings-filter ) ).
    html->add( |<input type="submit" class="hidden-submit">| ).
    html->add( |</form>| ).

    IF settings-list_settings-only_favorites = abap_true.
      DATA(icon_class) = `blue`.
    ELSE.
      icon_class = `grey`.
    ENDIF.

    html->add( '<span class="toolbar-light pad-sides">' ).
    html->add( html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { icon_class }"></i> Only Favorites|
      iv_class = 'command'
      iv_act   = |{ c_action-toggle_favorites }| ) ).
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
    APPEND `<br>Starting query from <code>label:xxx</code> will filter appropriate label.` TO fragments.
    APPEND `Two "special" label queries are available:` TO fragments.
    APPEND ` <code>all</code> (to select all packages that have at least one label)` TO fragments.
    APPEND ` and <code>none</code> (to select unlabeled packages).` TO fragments.

    result = /apmg/cl_apm_gui_chunk_lib=>render_help_hint( concat_lines_of( table = fragments ) ).

  ENDMETHOD.


  METHOD render_header_bar.

    html->add( |<div class="repo-overview-toolbar">| ).

    render_filter_bar( html ).
    render_registry( html ).
    render_action_toolbar( html ).

    html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_header_label_list.

    IF all_labels IS INITIAL.
      RETURN.
    ENDIF.

    html->add( |<div class="repo-label-catalog">| ).
    html->add( '<label>Filter by label:</label>' ).
    html->add( /apmg/cl_apm_gui_chunk_lib=>render_label_list(
      it_labels           = all_labels
      io_label_colors     = label_colors
      iv_clickable_action = c_action-label_filter ) ).
    html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_package_list.

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

    DATA(list) = prepare_packages( ).

    LOOP AT list ASSIGNING FIELD-SYMBOL(<package>).
      render_table_item(
        html     = html
        package  = <package> ).
    ENDLOOP.

    html->add( |</tbody>| ).

  ENDMETHOD.


  METHOD render_table_footer.

    IF settings-list_settings-only_favorites = abap_true.
      html->add( `<tfoot>` ).
      html->add( `<tr><td colspan="100%">` ).
      html->add( |(Only favorites are shown. {
        html->a(
          iv_txt   = |Show All|
          iv_act   = |{ c_action-toggle_favorites }?force_state={ abap_false }| )
        })| ).
      html->add( `</td></tr>` ).
      html->add( `</tfoot>` ).
    ENDIF.

  ENDMETHOD.


  METHOD render_table_header.

    html->add( /apmg/cl_apm_gui_chunk_lib=>render_table_header(
      it_col_spec         = build_table_scheme( )
      iv_order_by         = settings-list_settings-order_by
      iv_order_descending = settings-list_settings-order_descending ) ).

  ENDMETHOD.


  METHOD render_table_item.

    " Start of row
    IF package-favorite = abap_true.
      DATA(fav_class) = ' class="favorite"'.
      DATA(fav_color) = 'blue'.
    ELSE.
      fav_class = ''.
      fav_color = 'grey'.
    ENDIF.

    html->add( |<tr data-key="{ package-id }"{ fav_class }">| ).

    " Favorite
    DATA(favorite_icon) = html->icon(
      iv_name  = |star/{ fav_color }|
      iv_class = 'pad-sides'
      iv_hint  = 'Click to toggle favorite' ).

    html->td(
      iv_class   = 'wmin'
      iv_content = html->a(
        iv_act = |{ /apmg/if_apm_gui_router=>c_action-favorite_package }?package={ package-package }|
        iv_txt = favorite_icon ) ).

    " Package
    IF package-write_protected = abap_true.
      DATA(lock_icon) = html->icon(
        iv_name  = 'lock/grey70'
        iv_class = 'm-em5-sides'
        iv_hint  = 'Locked against changes by apm' ).
    ENDIF.

    html->td( ii_content = /apmg/cl_apm_gui_chunk_lib=>render_package_name(
      iv_package        = package-package
      iv_suppress_title = xsdbool( settings-list_settings-only_favorites = abap_false ) ) ).

    " Labels
    IF all_labels IS NOT INITIAL.
      html->td(
        iv_content = /apmg/cl_apm_gui_chunk_lib=>render_label_list(
          it_labels       = package-labels
          io_label_colors = label_colors )
        iv_class   = 'labels' ).
    ENDIF.

    " Name
    html->td(
      html->a(
        iv_txt = package-name
        iv_act = |{ c_action-select }?key={ package-package }| ) ).

    " Version
    html->td(
      html->a(
        iv_txt = package-version
        iv_act = |{ c_action-select }?key={ package-package }| ) && lock_icon ).

    " Description
    html->td(
      html->a(
        iv_txt = /apmg/cl_apm_emoji=>create( )->format_emoji( package-description )
        iv_act = |{ c_action-select }?key={ package-package }| ) ).

    " Details: changed by
    html->td(
      iv_class   = 'ro-detail'
      ii_content = /apmg/cl_apm_gui_chunk_lib=>render_user_name(
        iv_username       = package-changed_by
        iv_suppress_title = xsdbool( settings-list_settings-only_favorites = abap_false ) ) ).

    " Details: changed at
    html->td(
      iv_class   = 'ro-detail'
      iv_content = |{ package-changed_at }| ).

    " Details: key for navigation
    html->td(
      iv_class   = 'ro-detail nodisplay'
      iv_content = |{ package-package }| ).

    " Go-to action
    html->td(
      iv_class   = 'ro-go wmin'
      iv_content = html->a(
        iv_title = 'Open'
        iv_txt   = '&rtrif;'
        iv_act   = |{ c_action-select }?key={ package-package }| ) ).

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
        SUBMATCHES settings-list_settings-filter ##SUBRC_OK.
    ENDIF.

    settings-list_settings-filter = condense( settings-list_settings-filter ).
    save_settings( ).

  ENDMETHOD.


  METHOD set_order_by.

    IF settings-list_settings-order_by <> order_by.
      set_order_direction( abap_false ). " Reset ordering
    ENDIF.

    settings-list_settings-order_by = order_by.
    save_settings( ).

  ENDMETHOD.


  METHOD set_order_direction.

    settings-list_settings-order_descending = order_descending.
    save_settings( ).

  ENDMETHOD.
ENDCLASS.
