CLASS zcl_abappm_gui_page_list DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Package List
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_hotkeys.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !iv_only_favorites TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(ri_page)     TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_only_favorites TYPE abap_bool OPTIONAL
      RAISING
        zcx_abapgit_exception.

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
      mt_packages     TYPE zif_abappm_package_json=>ty_packages,
      mt_all_labels   TYPE string_table,
      mo_label_colors TYPE REF TO zcl_abapgit_string_map,
      ms_settings     TYPE zif_abappm_settings=>ty_settings.

    METHODS set_order_by
      IMPORTING
        !iv_order_by TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS set_order_direction
      IMPORTING
        !iv_order_descending TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS set_filter
      IMPORTING
        it_postdata TYPE zif_abapgit_html_viewer=>ty_post_data
      RAISING
        zcx_abapgit_exception.

    METHODS apply_filter
      CHANGING
        ct_packages TYPE zif_abappm_package_json=>ty_packages.

    METHODS render_package_list
      IMPORTING
        ii_html     TYPE REF TO zif_abapgit_html
        it_packages TYPE zif_abappm_package_json=>ty_packages
      RAISING
        zcx_abapgit_exception.

    METHODS render_package_palette
      IMPORTING
        iv_action      TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_table_header
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html.

    METHODS render_table_footer
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html.

    METHODS render_table_body
      IMPORTING
        ii_html     TYPE REF TO zif_abapgit_html
        it_packages TYPE zif_abappm_package_json=>ty_packages
      RAISING
        zcx_abapgit_exception.

    METHODS render_table_item
      IMPORTING
        ii_html    TYPE REF TO zif_abapgit_html
        is_package TYPE zif_abappm_package_json=>ty_package
      RAISING
        zcx_abapgit_exception.

    METHODS render_header_bar
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html.

    METHODS render_header_label_list
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html.

    METHODS apply_order_by
      CHANGING ct_packages TYPE zif_abappm_package_json=>ty_packages.

    METHODS prepare_packages
      RETURNING
        VALUE(rt_packages) TYPE zif_abappm_package_json=>ty_packages
      RAISING
        zcx_abapgit_exception.

    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_action_toolbar
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    METHODS render_filter_bar
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    METHODS render_registry
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    METHODS build_table_scheme
      RETURNING
        VALUE(rt_tab_scheme) TYPE zif_abapgit_definitions=>ty_col_spec_tt.

    METHODS collect_all_labels
      IMPORTING
        it_packages    TYPE zif_abappm_package_json=>ty_packages
      RETURNING
        VALUE(rt_list) TYPE string_table.

    METHODS render_filter_help_hint
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS load_package_list
      RAISING
        zcx_abapgit_exception.

    METHODS load_settings
      RAISING
        zcx_abapgit_exception.

    METHODS save_settings
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_page_list IMPLEMENTATION.


  METHOD apply_filter.

    DATA lv_pfxl TYPE i.
    DATA lv_idx TYPE i.
    DATA lv_filter_label TYPE string.
    FIELD-SYMBOLS <ls_package> LIKE LINE OF ct_packages.

    IF ms_settings-list_settings-filter IS INITIAL.
      RETURN.
    ENDIF.

    lv_pfxl = strlen( c_label_filter_prefix ).

    IF strlen( ms_settings-list_settings-filter ) > lv_pfxl AND
        ms_settings-list_settings-filter+0(lv_pfxl) = c_label_filter_prefix.
      lv_filter_label = ms_settings-list_settings-filter+lv_pfxl.
      IF lv_filter_label = 'all'.
        DELETE ct_packages WHERE labels IS INITIAL.
      ELSEIF lv_filter_label = 'none'.
        DELETE ct_packages WHERE labels IS NOT INITIAL.
      ELSE.
        LOOP AT ct_packages ASSIGNING <ls_package>.
          lv_idx = sy-tabix.
          READ TABLE <ls_package>-labels TRANSPORTING NO FIELDS WITH KEY table_line = lv_filter_label.
          IF sy-subrc <> 0.
            DELETE ct_packages INDEX lv_idx.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE. " Regular filter
      DELETE ct_packages WHERE
            package    NS ms_settings-list_settings-filter
        AND name       NS ms_settings-list_settings-filter
        AND version    NS ms_settings-list_settings-filter
        AND changed_by NS ms_settings-list_settings-filter.
    ENDIF.

  ENDMETHOD.


  METHOD apply_order_by.

    DATA:
      lt_sort TYPE abap_sortorder_tab,
      ls_sort LIKE LINE OF lt_sort.

    ls_sort-name       = 'FAVORITE'.
    ls_sort-descending = abap_true.
    ls_sort-astext     = abap_true.
    INSERT ls_sort INTO TABLE lt_sort.

    IF ms_settings-list_settings-order_by IS NOT INITIAL.
      CLEAR ls_sort.

      IF ms_settings-list_settings-order_by = 'CHANGED_AT'.
        ls_sort-name = ms_settings-list_settings-order_by && c_raw_field_suffix.
      ELSE.
        ls_sort-name   = ms_settings-list_settings-order_by.
        ls_sort-astext = abap_true.
      ENDIF.

      ls_sort-descending = ms_settings-list_settings-order_descending.
      INSERT ls_sort INTO TABLE lt_sort.
    ENDIF.

    SORT ct_packages BY (lt_sort).

  ENDMETHOD.


  METHOD build_table_scheme.

    DATA lo_tab_scheme TYPE REF TO lcl_table_scheme.

    CREATE OBJECT lo_tab_scheme.

    lo_tab_scheme->add_column(
      iv_tech_name      = 'FAVORITE'
      iv_css_class      = 'wmin'
      iv_allow_order_by = abap_false
    )->add_column(
      iv_tech_name      = 'PACKAGE'
      iv_display_name   = 'Package'
      iv_css_class      = 'package'
      iv_allow_order_by = abap_true ).

    IF mt_all_labels IS NOT INITIAL.
      lo_tab_scheme->add_column(
        iv_tech_name      = 'LABELS'
        iv_display_name   = 'Labels'
        iv_allow_order_by = abap_false ).
    ENDIF.

    lo_tab_scheme->add_column(
      iv_tech_name      = 'NAME'
      iv_display_name   = 'Name'
      iv_css_class      = 'name'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'VERSION'
      iv_display_name   = 'Version'
      iv_css_class      = 'version'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'DESCRIPTION'
      iv_display_name   = 'Description'
      iv_css_class      = 'description'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'CHANGED_BY'
      iv_display_name   = 'Changed by'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'CHANGED_AT'
      iv_display_name   = 'Changed at'
      iv_css_class      = 'ro-detail'
      iv_add_tz         = abap_true
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'GO'
      iv_css_class      = 'ro-go wmin'
      iv_allow_order_by = abap_false ).

    rt_tab_scheme = lo_tab_scheme->mt_col_spec.

  ENDMETHOD.


  METHOD collect_all_labels.

    FIELD-SYMBOLS <ls_package> LIKE LINE OF it_packages.

    LOOP AT it_packages ASSIGNING <ls_package>.
      APPEND LINES OF <ls_package>-labels TO rt_list.
    ENDLOOP.

    SORT rt_list.
    DELETE rt_list WHERE table_line IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM rt_list.

  ENDMETHOD.


  METHOD constructor.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    super->constructor( ).

    load_package_list( ).

    load_settings( ).

    " Overwrite setting
    IF iv_only_favorites = abap_true.
      ms_settings-list_settings-only_favorites = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_page_list.

    CREATE OBJECT lo_component
      EXPORTING
        iv_only_favorites = iv_only_favorites.

    ri_page = zcl_abappm_gui_page_hoc=>create(
      iv_page_title         = 'Package List'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD load_package_list.
    mt_packages = zcl_abappm_package_json=>list( iv_instanciate = abap_true ).
  ENDMETHOD.


  METHOD load_settings.
    TRY.
        ms_settings = zcl_abappm_settings=>factory( )->load( )->get( ).
      CATCH zcx_abappm_error ##NO_HANDLER.
        " Settings don't exist (yet)
    ENDTRY.
  ENDMETHOD.


  METHOD prepare_packages.

    FIELD-SYMBOLS:
      <ls_package>  LIKE LINE OF rt_packages,
      <ls_settings> LIKE LINE OF ms_settings-package_settings.

    rt_packages = mt_packages.

    LOOP AT rt_packages ASSIGNING <ls_package>.
      READ TABLE ms_settings-package_settings ASSIGNING <ls_settings>
        WITH TABLE KEY package = <ls_package>-package.
      IF sy-subrc = 0.
        <ls_package>-favorite        = <ls_settings>-favorite.
        <ls_package>-labels          = <ls_settings>-labels.
        <ls_package>-write_protected = <ls_settings>-write_protected.
      ENDIF.
    ENDLOOP.

    IF ms_settings-list_settings-only_favorites = abap_true.
      DELETE rt_packages WHERE favorite = abap_false.
    ENDIF.

    " Hmmm, side effect, not ideal, but we need label list before filter applied
    mt_all_labels = collect_all_labels( rt_packages ).

    apply_order_by( CHANGING ct_packages = rt_packages ).
    apply_filter( CHANGING ct_packages = rt_packages ).

  ENDMETHOD.


  METHOD render_action_toolbar.
    " FUTURE
  ENDMETHOD.


  METHOD render_filter_bar.

    DATA lv_icon_class TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_text_input(
      iv_name      = |filter|
      iv_label     = |Filter: { render_filter_help_hint( ) }|
      iv_value     = ms_settings-list_settings-filter ) ).
    ri_html->add( |<input type="submit" class="hidden-submit">| ).
    ri_html->add( |</form>| ).

    IF ms_settings-list_settings-only_favorites = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.

    ri_html->add( '<span class="toolbar-light pad-sides">' ).
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Only Favorites|
      iv_class = 'command'
      iv_act   = |{ c_action-toggle_favorites }| ) ).
    ri_html->add( ri_html->a(
      iv_txt   = '<i id="icon-filter-detail" class="icon icon-check"></i> Detail'
      iv_act   = |gHelper.toggleRepoListDetail()|
      iv_class = 'command'
      iv_typ   = zif_abapgit_html=>c_action_type-onclick ) ).
    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_filter_help_hint.

    DATA lt_fragments TYPE string_table.

    APPEND `Filter is applied to all text fields in the below table.` TO lt_fragments.
    APPEND ` Search works for any portion of the text (so can be a mid part as well).` TO lt_fragments.
    APPEND `<br>Starting query from <code>label:xxx</code> will filter appropriate label.` TO lt_fragments.
    APPEND `Two "special" label queries are available:` TO lt_fragments.
    APPEND ` <code>all</code> (to select all packages that have at least one label)` TO lt_fragments.
    APPEND ` and <code>none</code> (to select unlabeled packages).` TO lt_fragments.

    rv_html = zcl_abapgit_gui_chunk_lib=>render_help_hint( concat_lines_of( table = lt_fragments ) ).

  ENDMETHOD.


  METHOD render_header_bar.

    ii_html->add( |<div class="repo-overview-toolbar">| ).
    ii_html->add( render_filter_bar( ) ).
    ii_html->add( render_registry( ) ).
    " FUTURE
    " ii_html->add( render_action_toolbar( ) ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_header_label_list.

    IF mt_all_labels IS INITIAL.
      RETURN.
    ENDIF.

    ii_html->add( |<div class="repo-label-catalog">| ).
    ii_html->add( '<label>Filter by label:</label>' ).
    ii_html->add( zcl_abapgit_gui_chunk_lib=>render_label_list(
      it_labels           = mt_all_labels
      io_label_colors     = mo_label_colors
      iv_clickable_action = c_action-label_filter ) ).
    ii_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_package_list.

    ii_html->add( |<table>| ).

    render_table_header( ii_html ).
    render_table_body(
      ii_html     = ii_html
      it_packages = it_packages ).
    render_table_footer( ii_html ).

    ii_html->add( |</table>| ).

  ENDMETHOD.


  METHOD render_package_palette.

    DATA:
      lv_size     TYPE i,
      lv_json     TYPE string,
      lt_packages LIKE mt_packages.

    FIELD-SYMBOLS <ls_package> LIKE LINE OF lt_packages.

    lt_packages = mt_packages.
    lv_size = lines( lt_packages ).
    SORT lt_packages BY description AS TEXT.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( 'var repoCatalog = [' ).
    LOOP AT lt_packages ASSIGNING <ls_package>.
      lv_json = |\{|
        && | key: "{ <ls_package>-package }",|
        && | isOffline: "",|
        && | displayName: "{ escape(
                               val    = <ls_package>-description
                               format = cl_abap_format=>e_html_js ) }"|
        && | \}|.
      IF sy-tabix < lv_size.
        lv_json = lv_json && ','.
      ENDIF.
      ri_html->add( lv_json ).
    ENDLOOP.
    ri_html->add( '];' ).

    ri_html->add( |var gGoRepoPalette = new CommandPalette(createRepoCatalogEnumerator(repoCatalog, "{
      iv_action }"), \{| ).
    ri_html->add( '  toggleKey: "F2",' ).
    ri_html->add( '  hotkeyDescription: "Go to Package"' ).
    ri_html->add( '});' ).

  ENDMETHOD.


  METHOD render_registry.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<span style="float:right">' ).
    ri_html->add( '<span class="transport-box">' ).
    ri_html->add_a(
      iv_title = 'Registry'
      iv_txt   = ms_settings-registry
      iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ ms_settings-registry }| ).
    ri_html->add( '</span>' ).
    ri_html->add( '</span>').

  ENDMETHOD.


  METHOD render_scripts.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'var gHelper = new RepoOverViewHelper({ focusFilterKey: "f" });' ).

  ENDMETHOD.


  METHOD render_table_body.

    FIELD-SYMBOLS <ls_package> LIKE LINE OF it_packages.

    ii_html->add( '<tbody>' ).

    LOOP AT it_packages ASSIGNING <ls_package>.
      render_table_item(
        ii_html    = ii_html
        is_package = <ls_package> ).
    ENDLOOP.

    ii_html->add( |</tbody>| ).

  ENDMETHOD.


  METHOD render_table_footer.

    IF ms_settings-list_settings-only_favorites = abap_true.
      ii_html->add( `<tfoot>` ).
      ii_html->add( `<tr><td colspan="100%">` ).
      ii_html->add( |(Only favorites are shown. {
        ii_html->a(
          iv_txt   = |Show All|
          iv_act   = |{ c_action-toggle_favorites }?force_state={ abap_false }| )
      })| ).
      ii_html->add( `</td></tr>` ).
      ii_html->add( `</tfoot>` ).
    ENDIF.

  ENDMETHOD.


  METHOD render_table_header.

    ii_html->add( zcl_abapgit_gui_chunk_lib=>render_table_header(
      it_col_spec         = build_table_scheme( )
      iv_order_by         = ms_settings-list_settings-order_by
      iv_order_descending = ms_settings-list_settings-order_descending ) ).

  ENDMETHOD.


  METHOD render_table_item.

    DATA:
      lv_is_online_repo   TYPE abap_bool,
      lv_repo_type_icon   TYPE string,
      lv_favorite_icon    TYPE string,
      lv_fav_tr_class     TYPE string,
      lv_lock             TYPE string,
      lx_error            TYPE REF TO zcx_abappm_error,
      ls_settings         TYPE zif_abappm_settings=>ty_settings,
      ls_package_settings TYPE zif_abappm_settings=>ty_package_settings.

    TRY.
        ls_settings = zcl_abappm_settings=>factory( )->load( )->get( ).
        READ TABLE ls_settings-package_settings INTO ls_package_settings WITH KEY package = is_package-package.
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    " Start of row
    IF ls_package_settings-favorite = abap_true.
      lv_fav_tr_class = ' class="favorite"'.
    ELSE.
      lv_fav_tr_class = ''.
    ENDIF.

    ii_html->add( |<tr{ lv_fav_tr_class } data-key="{ is_package-package }">| ).

    " Favorite
    lv_favorite_icon = ii_html->icon(
      iv_name  = 'star/grey' " blue is added in css, based on TR style
      iv_class = 'pad-sides'
      iv_hint  = 'Click to toggle favorite' ).

    ii_html->td(
      iv_class   = 'wmin'
      iv_content = ii_html->a(
        iv_act = |{ zif_abappm_gui_router=>c_action-favorite_package }?package={ is_package-package }|
        iv_txt = lv_favorite_icon ) ).

    " Package
    IF ls_package_settings-write_protected = abap_true.
      lv_lock = ii_html->icon(
        iv_name  = 'lock/grey70'
        iv_class = 'm-em5-sides'
        iv_hint  = 'Locked against changes by apm' ).
    ENDIF.

    ii_html->td( ii_content = zcl_abapgit_gui_chunk_lib=>render_package_name(
      iv_package        = is_package-package
      iv_suppress_title = boolc( NOT ms_settings-list_settings-only_favorites = abap_true ) )  ).

    " Labels
    IF mt_all_labels IS NOT INITIAL.
      ii_html->td(
        iv_content = zcl_abapgit_gui_chunk_lib=>render_label_list(
          it_labels       = ls_package_settings-labels
          io_label_colors = mo_label_colors )
        iv_class   = 'labels' ).
    ENDIF.

    " Name
    ii_html->td(
      ii_html->a(
        iv_txt = is_package-name
        iv_act = |{ c_action-select }?package={ is_package-package }| ) ).

    " Version
    ii_html->td(
      ii_html->a(
        iv_txt = is_package-version
        iv_act = |{ c_action-select }?package={ is_package-package }| ) && lv_lock ).

    " Description
    ii_html->td(
      ii_html->a(
        iv_txt = is_package-description
        iv_act = |{ c_action-select }?package={ is_package-package }| ) ).

    " Details: changed by
    ii_html->td(
      iv_class   = 'ro-detail'
      ii_content = zcl_abapgit_gui_chunk_lib=>render_user_name(
        iv_username = is_package-changed_by
        iv_suppress_title = boolc( NOT ms_settings-list_settings-only_favorites = abap_true ) ) ).

    " Details: changed at
    ii_html->td(
      iv_class   = 'ro-detail'
      iv_content = |{ is_package-changed_at }| ).

    " Go-to action
    ii_html->td(
      iv_class   = 'ro-go wmin'
      iv_content = ii_html->a(
        iv_title = 'Open'
        iv_txt   = '&rtrif;'
        iv_act   = |{ c_action-select }?package={ is_package-package }| ) ).

    ii_html->add( `</tr>` ).

  ENDMETHOD.


  METHOD save_settings.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    TRY.
        zcl_abappm_settings=>factory( )->set( ms_settings )->save( ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD set_filter.

    FIELD-SYMBOLS <lv_postdata> LIKE LINE OF it_postdata.

    READ TABLE it_postdata ASSIGNING <lv_postdata> INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX 'filter=(.*)'
        IN <lv_postdata>
        SUBMATCHES ms_settings-list_settings-filter.
    ENDIF.

    ms_settings-list_settings-filter = condense( ms_settings-list_settings-filter ).
    save_settings( ).

  ENDMETHOD.


  METHOD set_order_by.
    IF ms_settings-list_settings-order_by <> iv_order_by.
      set_order_direction( abap_false ). " Reset ordering
    ENDIF.
    ms_settings-list_settings-order_by = iv_order_by.
    save_settings( ).
  ENDMETHOD.


  METHOD set_order_direction.
    ms_settings-list_settings-order_descending = iv_order_descending.
    save_settings( ).
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_package TYPE devclass.

    lv_package = ii_event->query( )->get( 'PACKAGE' ).

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-select.

        ms_settings-last_package = lv_package.
        save_settings( ).

        rs_handled-page  = zcl_abappm_gui_page_package=>create( lv_package ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-change_order_by.

        set_order_by( ii_event->query( )->get( 'ORDERBY' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-toggle_favorites.

        IF ii_event->query( )->has( 'FORCE_STATE' ) = abap_true.
          ms_settings-list_settings-only_favorites = ii_event->query( )->get( 'FORCE_STATE' ).
        ELSE.
          ms_settings-list_settings-only_favorites = boolc( ms_settings-list_settings-only_favorites = abap_false ).
        ENDIF.
        save_settings( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-direction.

        set_order_direction( boolc( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-apply_filter.

        set_filter( ii_event->mt_postdata ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-label_filter.

        IF ii_event->mv_getdata IS NOT INITIAL.
          ms_settings-list_settings-filter = c_label_filter_prefix && ii_event->mv_getdata.
        ELSE.
          CLEAR ms_settings-list_settings-filter. " Unexpected request
        ENDIF.
        save_settings( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Package List'.

    ls_hotkey_action-description = |Settings|.
    ls_hotkey_action-action      = zif_abappm_gui_router=>c_action-go_settings.
    ls_hotkey_action-hotkey      = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Refresh|.
    ls_hotkey_action-action      = c_action-refresh.
    ls_hotkey_action-hotkey      = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Init|.
    ls_hotkey_action-action      = zif_abappm_gui_router=>c_action-apm_init.
    ls_hotkey_action-hotkey      = |t|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Install|.
    ls_hotkey_action-action      = zif_abappm_gui_router=>c_action-apm_install.
    ls_hotkey_action-hotkey      = |i|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Publish|.
    ls_hotkey_action-action      = zif_abappm_gui_router=>c_action-apm_publish.
    ls_hotkey_action-hotkey      = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Uninstall|.
    ls_hotkey_action-action      = zif_abappm_gui_router=>c_action-apm_uninstall.
    ls_hotkey_action-hotkey      = |u|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    " registered/handled in js
    ls_hotkey_action-description = |Previous Package|.
    ls_hotkey_action-action      = `#`.
    ls_hotkey_action-hotkey      = |4|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Next Package|.
    ls_hotkey_action-action      = `##`.
    ls_hotkey_action-hotkey      = |6|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Show Package|.
    ls_hotkey_action-action      = `###`.
    ls_hotkey_action-hotkey      = |Enter|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Focus Filter|.
    ls_hotkey_action-action      = `####`.
    ls_hotkey_action-hotkey      = |f|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-main' ).

    ro_toolbar->add(
      iv_txt = zcl_abapgit_html=>icon( 'file' ) && ' Init'
      iv_act = zif_abappm_gui_router=>c_action-apm_init
    )->add(
      iv_txt = zcl_abapgit_html=>icon( 'download-solid' ) && ' Install'
      iv_act = zif_abappm_gui_router=>c_action-apm_install
    )->add(
      iv_txt = zcl_abapgit_html=>icon( 'upload-solid' ) && ' Publish'
      iv_act = zif_abappm_gui_router=>c_action-apm_publish
    )->add(
      iv_txt = zcl_abapgit_html=>icon( 'times-solid' ) && ' Uninstall'
      iv_act = zif_abappm_gui_router=>c_action-apm_uninstall
    )->add(
      iv_txt = zcl_abappm_gui_buttons=>settings( )
      io_sub = zcl_abappm_gui_menus=>settings( )
    )->add(
      iv_txt = zcl_abapgit_html=>icon( 'redo-alt-solid' )
      iv_act = c_action-refresh
    )->add(
      iv_txt = zcl_abappm_gui_buttons=>advanced( )
      io_sub = zcl_abappm_gui_menus=>advanced( )
    )->add(
      iv_txt = zcl_abappm_gui_buttons=>help( )
      io_sub = zcl_abappm_gui_menus=>help( ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lt_packages TYPE zif_abappm_package_json=>ty_packages.

    register_handlers( ).

    mo_label_colors = zcl_abapgit_repo_labels=>split_colors_into_map( ms_settings-gui_settings-label_colors ).

    lt_packages = prepare_packages( ).

    ri_html = zcl_abapgit_html=>create( ).

    zcl_abapgit_exit=>get_instance( )->wall_message_list( ri_html ).

    ri_html->add( |<div class="repo-overview">| ).
    render_header_bar( ri_html ).
    render_header_label_list( ri_html ).
    render_package_list(
      ii_html     = ri_html
      it_packages = lt_packages ).
    ri_html->add( |</div>| ).

    register_deferred_script( render_scripts( ) ).
    register_deferred_script( render_package_palette( c_action-select ) ).

  ENDMETHOD.
ENDCLASS.
