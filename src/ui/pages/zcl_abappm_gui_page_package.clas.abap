CLASS zcl_abappm_gui_page_package DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* Markdown Extension for abapGit
*
* https://github.com/Marc-Bernard-Tools/ABAP-Markdown-Ext-for-abapGit
*
* Copyright 2023 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_menu_provider.

    CONSTANTS:
      BEGIN OF c_default,
        path     TYPE string VALUE '/',
        filename TYPE string VALUE 'README.md',
        view     TYPE string VALUE 'view_readme_rendered',
      END OF c_default.

    CLASS-METHODS create
      IMPORTING
        !iv_package    TYPE devclass
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

    CONSTANTS:
      BEGIN OF c_action,
        view_readme_rendered TYPE string VALUE 'view_readme_rendered',
        view_readme_source   TYPE string VALUE 'view_readme_source',
        view_readme_raw      TYPE string VALUE 'view_readme_raw',
        edit_readme          TYPE string VALUE 'edit_readme',
        view_json            TYPE string VALUE 'view_json',
        edit_json            TYPE string VALUE 'edit_json',
        view_dependencies    TYPE string VALUE 'view_dependencies',
        edit_dependencies    TYPE string VALUE 'edit_dependencies',
      END OF c_action.

    CONSTANTS:
      BEGIN OF c_markdown,
        logo TYPE string VALUE 'markdown-logo.png',
        mime TYPE string VALUE 'ZMARKDOWN_LOGO',
      END OF c_markdown.

    TYPES:
      BEGIN OF ty_markdown,
        path     TYPE string,
        filename TYPE string,
        data     TYPE string,
      END OF ty_markdown,
      ty_markdown_files TYPE SORTED TABLE OF ty_markdown WITH UNIQUE KEY path filename.

    DATA:
      mv_package      TYPE devclass,
      mv_view         TYPE string,
      ms_markdown     TYPE ty_markdown,
      ms_package_json TYPE zif_abappm_package_json_types=>ty_package_json.

    METHODS get_toolbar
      RETURNING
        VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar
      RAISING
        zcx_abapgit_exception.

    METHODS get_markdown
      RETURNING
        VALUE(rv_markdown) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_package_json
      RETURNING
        VALUE(rs_package_json) TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abapgit_exception.

    METHODS get_json_data
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_mime
      IMPORTING
        !iv_mime_name   TYPE csequence
      RETURNING
        VALUE(rv_xdata) TYPE xstring.

    METHODS get_root_href
      IMPORTING
        !iv_url        TYPE string
        !iv_branch     TYPE string
      RETURNING
        VALUE(rv_root) TYPE string.

    METHODS get_root_img
      IMPORTING
        !iv_url        TYPE string
        !iv_branch     TYPE string
      RETURNING
        VALUE(rv_root) TYPE string.

    METHODS get_package_boxed
      IMPORTING
        !iv_name       TYPE string
        !iv_value      TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS render_styles
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_top
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header_content
      IMPORTING
        !iv_image      TYPE string
        !iv_text       TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_content
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_markdown
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_markdown_source
      IMPORTING
        !iv_raw        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_dependencies
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_json
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_footer
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_page_package IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    IF iv_package IS INITIAL.
      zcx_abapgit_exception=>raise( 'Missing package' ).
    ELSE.
      mv_package = iv_package.
    ENDIF.

    mv_view              = c_default-view.
    ms_markdown-path     = c_default-path.
    ms_markdown-filename = c_default-filename.
    ms_markdown-data     = get_markdown( ).
    ms_package_json      = get_package_json( ).

    gui_services( )->cache_asset(
      iv_type    = 'image'
      iv_subtype = 'png'
      iv_url     = |{ c_markdown-logo }|
      iv_xdata   = get_mime( c_markdown-mime ) ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abappm_gui_page_package.

    CREATE OBJECT lo_component
      EXPORTING
        iv_package = iv_package.

    ri_page = zcl_abappm_gui_page_hoc=>create(
      iv_page_title         = 'Package View'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD get_json_data.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    TRY.
        " Always load data since it can be edited in another page
        rv_json = zcl_abappm_package_json=>factory( mv_package )->load( )->get_json( ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_markdown.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    TRY.
        " Always load data since it can be edited in another page
        rv_markdown = zcl_abappm_readme=>factory( mv_package )->load( )->get( ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_mime.

    DATA:
      ls_key    TYPE wwwdatatab,
      lv_size_c TYPE wwwparams-value,
      lv_size   TYPE i,
      lt_w3mime TYPE STANDARD TABLE OF w3mime,
      ls_w3mime LIKE LINE OF lt_w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_w3mime INTO ls_w3mime.
      CONCATENATE rv_xdata ls_w3mime-line INTO rv_xdata IN BYTE MODE.
    ENDLOOP.
    rv_xdata = rv_xdata(lv_size).

  ENDMETHOD.


  METHOD get_package_boxed.
    rv_html = |<span class="transport-box">{ iv_name }: { iv_value }</span>|.
  ENDMETHOD.


  METHOD get_package_json.

    DATA lx_error TYPE REF TO zcx_abappm_error.

    TRY.
        " Always load data since it can be edited in another page
        rs_package_json = zcl_abappm_package_json=>factory( mv_package )->load( )->get( ).
      CATCH zcx_abappm_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_root_href.

    IF iv_url CS 'github.com'.
      rv_root = |{ iv_url }/blob/{ iv_branch }|.
    ELSEIF iv_url CS 'gitlab.com'.
      rv_root = |{ iv_url }/-/blob/{ iv_branch }|.
    ELSE.
      " TODO: Add other git hosts
      rv_root = |{ iv_url }/blob/{ iv_branch }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_root_img.

    IF iv_url CS 'github.com'.
      rv_root = |{ iv_url }/raw/{ iv_branch }|.
    ELSEIF iv_url CS 'gitlab.com'.
      rv_root = |{ iv_url }/-/raw/{ iv_branch }|.
    ELSE.
      " TODO: Add other git hosts
      rv_root = |{ iv_url }/raw/{ iv_branch }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_toolbar.

    DATA lv_action TYPE string.

    CASE mv_view.
      WHEN c_action-view_readme_rendered OR c_action-view_readme_source OR c_action-view_readme_raw.
        lv_action = c_action-edit_readme.
      WHEN c_action-view_dependencies.
        lv_action = c_action-edit_dependencies.
      WHEN c_action-view_json.
        lv_action = c_action-edit_json.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar' )->add(
      iv_txt = 'Edit'
      iv_act = |{ lv_action }?key={ mv_package }| ).

  ENDMETHOD.


  METHOD render_content.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="content syntax-hl">' ).

    CASE mv_view.
      WHEN c_action-view_readme_rendered.
        ri_html->add( render_markdown( ) ).
      WHEN c_action-view_readme_source.
        ri_html->add( render_markdown_source( abap_false ) ).
      WHEN c_action-view_readme_raw.
        ri_html->add( render_markdown_source( abap_true ) ).
      WHEN c_action-view_dependencies.
        ri_html->add( render_dependencies( ) ).
      WHEN c_action-view_json.
        ri_html->add( render_json( ) ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_dependencies.

    DATA ls_dependency TYPE zif_abappm_package_json_types=>ty_dependency.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="html">' ).

    IF ms_package_json-dependencies IS NOT INITIAL.
      ri_html->add( |<h2>Dependencies ({ lines( ms_package_json-dependencies ) })</h2>| ).

      LOOP AT ms_package_json-dependencies INTO ls_dependency.
        ri_html->add( get_package_boxed(
          iv_name  = ls_dependency-name
          iv_value = ls_dependency-range ) ).
        " TODO: Display status of dependency
        " Is it installed and if yes in which version?
      ENDLOOP.
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_footer.

    ri_html = zcl_abapgit_html=>create( ).

  ENDMETHOD.


  METHOD render_header.

    ri_html = zcl_abapgit_html=>create( ).

    CASE mv_view.
      WHEN c_action-view_readme_rendered
        OR c_action-view_readme_source
        OR c_action-view_readme_raw.
        IF ms_markdown-data IS INITIAL.
          ri_html->add( '<div class="dummydiv success">' ).
          ri_html->add( 'Readme not found' ).
          ri_html->add( '</div>' ).
        ELSE.
          ri_html->add( render_header_content(
            iv_image = |<img src="{ c_markdown-logo }" title="{ ms_markdown-filename }" class="logo">  |
            iv_text  = |{ ms_markdown-path+1 }{ ms_markdown-filename }| ) ).
        ENDIF.
      WHEN c_action-view_dependencies
        OR c_action-view_json.
        ri_html->add( render_header_content(
          iv_image = zcl_abapgit_html=>icon( 'file' )
          iv_text  = 'package.abap.json' ) ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD render_header_content.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="header">' ).
    ri_html->add( iv_image ).
    ri_html->add( '<span class="indent5em">' ).
    ri_html->add( iv_text ).
    ri_html->add( '</span>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_json.

    DATA:
      lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter,
      lv_json        TYPE string,
      lt_json        TYPE string_table.

    ri_html = zcl_abapgit_html=>create( ).

    lo_highlighter = zcl_abapgit_syntax_factory=>create( '*.json' ).

    lv_json = get_json_data( ).
    SPLIT lv_json AT |\n| INTO TABLE lt_json.

    ri_html->add( '<div class="source">' ).
    ri_html->add( '<pre class="syntax-hl">' ).
    LOOP AT lt_json INTO lv_json.
      ri_html->add( lo_highlighter->process_line( lv_json ) ).
    ENDLOOP.
    ri_html->add( '</pre>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_markdown.

    DATA:
      lo_markdown TYPE REF TO zcl_abappm_markdown,
      lv_url      TYPE string,
      lv_branch   TYPE string,
      lv_html     TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    " TODO: Replace main branch with actual HEAD ref
    lv_url    = ms_package_json-repository-url.
    lv_branch = 'main'.

    CREATE OBJECT lo_markdown
      EXPORTING
        root_href = get_root_href(
                       iv_url    = lv_url
                       iv_branch = lv_branch )
        root_img  = get_root_img(
                       iv_url    = lv_url
                       iv_branch = lv_branch )
        path      = ms_markdown-path
        sapevent  = abap_true.

    lv_html = lo_markdown->text( ms_markdown-data ).

    " Output with Emoji
    ri_html->add( '<div class="html">' ).
    ri_html->add( zcl_abappm_markdown_emoji=>create( )->format_emoji( lv_html ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_markdown_source.

    DATA:
      lv_num       TYPE i,
      lv_language  TYPE string,
      lv_codeblock TYPE string,
      lv_markdown  TYPE string,
      lv_markup    TYPE string,
      lt_lines     TYPE TABLE OF string.

    FIELD-SYMBOLS <lv_line> TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    lv_markdown = ms_markdown-data.

    IF iv_raw = abap_true.
      lv_markdown = escape(
        val    = lv_markdown
        format = cl_abap_format=>e_html_text ).
    ENDIF.

    " Same style as diffs
    ri_html->add( '<div class="source">' ).
    ri_html->add( |<table class="diff_tab" id="{ ms_markdown-filename }">| ).

    SPLIT lv_markdown AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    lv_language = 'markdown'.

    LOOP AT lt_lines ASSIGNING <lv_line>.
      lv_num = sy-tabix.

      IF iv_raw = abap_true.
        lv_markup = <lv_line>.
      ELSE.
        " Detect code block
        FIND REGEX '^```\s*(.*)' IN <lv_line> SUBMATCHES lv_codeblock.
        IF sy-subrc = 0.
          lv_language = 'codeblock'. " falls back to txt
        ENDIF.

        lv_markup = zcl_abappm_markdown_syn=>process(
          iv_source   = <lv_line>
          iv_language = lv_language ).

        IF lv_codeblock IS NOT INITIAL.
          lv_language = lv_codeblock.
        ELSE.
          lv_language = 'markdown'.
        ENDIF.
      ENDIF.

      ri_html->add( '<tr class="diff_line">' ).
      ri_html->add( |<td class="num" line-num="{ lv_num }"></td>| ).
      ri_html->add( |<td class="code">{ lv_markup }</td>| ).
      ri_html->add( '</tr>' ).
    ENDLOOP.

    ri_html->add( '</table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_styles.

    DATA lv_css TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    " Emoji Styles
    lv_css = concat_lines_of(
      table = zcl_abappm_markdown_emoji=>create( )->get_emoji_css( )
      sep   = cl_abap_char_utilities=>newline ).

    ri_html->add( '<style>' ).
    ri_html->add( lv_css ).
    ri_html->add( '</style>' ).

    " Markdown Styles
    ri_html->add( '<style>' ).
    ri_html->add( '.markdown' ).
    ri_html->add( '{ background-color: #f2f2f2; padding: 15px; }' ).
    ri_html->add( '.markdown .logo' ).
    ri_html->add( '{ width: 36px; height: 22px; margin-top: -4px; }' ).
    ri_html->add( '.markdown .header,' ).
    ri_html->add( '.markdown .content' ).
    ri_html->add( '{ background-color: #ffffff; border: 1px solid #d8dee4; display: block; }' ).
    ri_html->add( '.markdown .header' ).
    ri_html->add( '{ font-size: larger; margin-bottom: 15px; padding: 15px; }' ).
    ri_html->add( '.markdown .content' ).
    ri_html->add( '{ padding: 25px; }' ).
    ri_html->add( '.markdown .html' ).
    ri_html->add( '{ max-width: 1024px; margin: 0 auto; padding: 25px; }' ).
    " Markdown View
    ri_html->add( '.markdown .source' ).
    ri_html->add( '{ font-family: Consolas,Courier,monospace; font-size: 12pt; padding: 25px;' ).
    ri_html->add( '  max-width: 1024px; margin: 0 auto; }' ).
    ri_html->add( '.markdown .source table' ).
    ri_html->add( '{ border: 1px solid #d8dee4; }' ).
    ri_html->add( '.markdown .source td' ).
    ri_html->add( '{ border-top: 0px; border-bottom: 0px; padding-top: 0; padding-bottom: 0;' ).
    ri_html->add( '  line-height: 20px; vertical-align: top; }' ).
    " Syntax Highlight
    ri_html->add( '.markdown .syntax-hl .heading' ).
    ri_html->add( '{ color: blue; }' ).
    ri_html->add( '.markdown .syntax-hl .link' ).
    ri_html->add( '{ color: purple; }' ).
    ri_html->add( '.markdown .syntax-hl .url' ).
    ri_html->add( '{ color: green; }' ).
    ri_html->add( '.markdown .syntax-hl .html' ).
    ri_html->add( '{ padding: 0; }' ).
    ri_html->add( '.markdown .syntax-hl .bold' ).
    ri_html->add( '{ font-weight: bold; }' ).
    " HTML Tags
    ri_html->add( '.markdown h1,' ).
    ri_html->add( '.markdown h2' ).
    ri_html->add( '{ border-bottom: 1px solid #d8dee4; box-sizing: border-box; }' ).
    ri_html->add( '.markdown img' ).
    ri_html->add( '{ border: 0; box-sizing: border-box; max-width: 100%; vertical-align: middle; }' ).
    ri_html->add( '.markdown table' ).
    ri_html->add( '{ border: 1px solid #ddd; border-radius: 3px; }' ).
    ri_html->add( '.markdown th,' ).
    ri_html->add( '{ color: #4078c0; background-color: #edf2f9; border-bottom-color: #ddd; }' ).
    ri_html->add( '.markdown th,' ).
    ri_html->add( '.markdown td' ).
    ri_html->add( '{ border: 1px solid #ddd; padding: 6px 13px; }' ).
    ri_html->add( '.markdown tr:first-child td' ).
    ri_html->add( '{ border-top: 0; }' ).
    ri_html->add( '.markdown hr' ).
    ri_html->add( '{ background-color: #eee; margin: 24px 0; overflow: hidden; padding: 0; }' ).
    ri_html->add( '.markdown mark' ).
    ri_html->add( '{ background-color: #fff8e0; border-radius: 6px; margin: 0; padding: .2em .4em; }' ).
    ri_html->add( '.markdown blockquote' ).
    ri_html->add( '{ background-color: #eee; border-left: 3px solid #303d36; border-radius: 6px;' ).
    ri_html->add( '  margin: 0 0 16px; padding: 1px 1em; }' ).
    " Code blocks
    ri_html->add( '.markdown pre' ).
    ri_html->add( '{ background-color: #eee; border-radius: 6px; display: block;' ).
    ri_html->add( '  margin-bottom: 16px; margin-top: 0; overflow: auto; overflow-wrap: normal;' ).
    ri_html->add( '  padding: 16px; word-break: normal; box-sizing: border-box;' ).
    ri_html->add( '  font-family: Consolas, Courier, monospace; font-size: 14px; }' ).
    ri_html->add( '.markdown p code' ).
    ri_html->add( '{ background-color: #eee; border-radius: 6px; margin: 0; padding: .2em .4em;' ).
    ri_html->add( '  font-family: Consolas, Courier, monospace; font-size: 14px; }' ).
    ri_html->add( '.markdown pre code' ).
    ri_html->add( '{ background-color: transparent; border-style: initial;' ).
    ri_html->add( '  border-width: 0; box-sizing: border-box; display: inline; margin: 0;' ).
    ri_html->add( '  overflow: visible; word-break: normal; overflow-wrap: normal;' ).
    ri_html->add( '  padding: 0; white-space: pre;' ).
    ri_html->add( '  font-family: Consolas, Courier, monospace; font-size: 14px; }' ).
    ri_html->add( '</style>' ).

  ENDMETHOD.


  METHOD render_top.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="paddings">' ).
    ri_html->add( '<table class="w100"><tr>' ).
    ri_html->add( '<td>' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_package_name( mv_package ) ).
    ri_html->add( '<span class="indent5em">' ).
    ri_html->add( get_package_boxed(
      iv_name  = ms_package_json-name
      iv_value = ms_package_json-version ) ).
    ri_html->add( '</span>' ).
    ri_html->add( '</td>' ).
    ri_html->add( '<td class="right">' ).
    ri_html->add( get_toolbar( )->render( iv_right = abap_true ) ).
    ri_html->add( '</td>' ).
    ri_html->add( '</tr></table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-view_readme_rendered OR c_action-view_readme_source OR c_action-view_readme_raw
        OR c_action-view_dependencies OR c_action-view_json.

        mv_view = ii_event->mv_action.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-edit_readme.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create(
          iv_key       = zcl_abappm_readme=>get_package_key( mv_package )
          iv_edit_mode = abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-edit_json.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create(
          iv_key       = zcl_abappm_package_json=>get_package_key( mv_package )
          iv_edit_mode = abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA:
      lo_file_menu   TYPE REF TO zcl_abapgit_html_toolbar,
      lo_readme_menu TYPE REF TO zcl_abapgit_html_toolbar,
      lv_act         TYPE string.

    FIELD-SYMBOLS <ls_markdown> TYPE ty_markdown.

    lo_readme_menu = zcl_abapgit_html_toolbar=>create( 'readme' )->add(
      iv_txt = 'Rendered'
      iv_chk = boolc( mv_view = c_action-view_readme_rendered OR mv_view IS INITIAL )
      iv_act = c_action-view_readme_rendered
     )->add(
      iv_txt = 'Markdown'
      iv_chk = boolc( mv_view = c_action-view_readme_source )
      iv_act = c_action-view_readme_source
     )->add(
      iv_txt = 'Raw'
      iv_chk = boolc( mv_view = c_action-view_readme_raw )
      iv_act = c_action-view_readme_raw ).

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'main' )->add(
      iv_txt = 'Readme'
      io_sub = lo_readme_menu
    )->add(
      iv_txt = 'Dependencies'
      iv_act = c_action-view_dependencies
    )->add(
      iv_txt = 'Manifest'
      iv_act = c_action-view_json
    )->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( render_styles( ) ).
    ri_html->add( render_top( ) ).

    ri_html->add( '<div class="markdown">' ).
    ri_html->add( render_header( ) ).
    ri_html->add( render_content( ) ).
    ri_html->add( render_footer( ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
