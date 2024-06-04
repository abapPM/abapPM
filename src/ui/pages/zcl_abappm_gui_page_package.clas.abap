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
        view     TYPE string VALUE 'view_rendered',
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
        go_back       TYPE string VALUE 'go_back',
        select_file   TYPE string VALUE 'select_file',
        view_rendered TYPE string VALUE 'view_rendered',
        view_source   TYPE string VALUE 'view_source',
        view_raw      TYPE string VALUE 'view_raw',
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

    DATA mv_package TYPE devclass.
    DATA mv_view TYPE string.
    DATA ms_markdown TYPE ty_markdown.
    DATA mt_markdown TYPE ty_markdown_files.

    METHODS _read_markdown
      IMPORTING
        !iv_package        TYPE devclass
      RETURNING
        VALUE(rt_markdown) TYPE ty_markdown_files.

    METHODS _get_markdown_data
      IMPORTING
        !iv_path       TYPE string
        !iv_filename   TYPE string
      RETURNING
        VALUE(rv_data) TYPE string.

    METHODS _get_markdown_rendered
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS _get_markdown_source
      IMPORTING
        !iv_raw        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS _get_logo
      IMPORTING
        !iv_title      TYPE string OPTIONAL
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS _get_mime
      IMPORTING
        !iv_mime_name   TYPE csequence
      RETURNING
        VALUE(rv_xdata) TYPE xstring.

    METHODS _get_root_href
      IMPORTING
        !iv_url        TYPE string
        !iv_branch     TYPE string
      RETURNING
        VALUE(rv_root) TYPE string.

    METHODS _get_root_img
      IMPORTING
        !iv_url        TYPE string
        !iv_branch     TYPE string
      RETURNING
        VALUE(rv_root) TYPE string.

    METHODS _render_styles
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS _render_header
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS _render_markdown
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS _render_footer
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS _replace_markdown_href
      IMPORTING
        !iv_html       TYPE string
        !iv_root_href  TYPE string
      RETURNING
        VALUE(rv_html) TYPE string.

ENDCLASS.



CLASS zcl_abappm_gui_page_package IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    IF iv_package IS INITIAL.
      zcx_abapgit_exception=>raise( 'Missing package' ).
    ELSE.
      mv_package = iv_package.
    ENDIF.

    mt_markdown = _read_markdown( mv_package ).

    mv_view              = c_default-view.
    ms_markdown-path     = c_default-path.
    ms_markdown-filename = c_default-filename.

    ms_markdown-data     = _get_markdown_data(
      iv_path     = ms_markdown-path
      iv_filename = ms_markdown-filename ).

    gui_services( )->cache_asset(
      iv_type    = 'image'
      iv_subtype = 'png'
      iv_url     = |{ c_markdown-logo }|
      iv_xdata   = _get_mime( c_markdown-mime ) ).

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


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-go_back.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_action-view_rendered OR c_action-view_source OR c_action-view_raw.

        mv_view = ii_event->mv_action.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-select_file.

        CLEAR ms_markdown.
        ms_markdown-path     = ii_event->query( )->get( 'PATH' ).
        ms_markdown-filename = ii_event->query( )->get( 'FILENAME' ).
        ms_markdown-data     = _get_markdown_data(
          iv_path     = ms_markdown-path
          iv_filename = ms_markdown-filename ).

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA:
      lo_file_menu TYPE REF TO zcl_abapgit_html_toolbar,
      lo_view_menu TYPE REF TO zcl_abapgit_html_toolbar,
      lv_act       TYPE string.

    FIELD-SYMBOLS: <ls_markdown> TYPE ty_markdown.

    CREATE OBJECT lo_view_menu EXPORTING iv_id = 'view'.

    lo_view_menu->add(
      iv_txt = 'Rendered'
      iv_chk = boolc( mv_view = c_action-view_rendered OR mv_view IS INITIAL )
      iv_act = c_action-view_rendered ).

    lo_view_menu->add(
      iv_txt = 'Markdown'
      iv_chk = boolc( mv_view = c_action-view_source )
      iv_act = c_action-view_source ).

    lo_view_menu->add(
      iv_txt = 'Raw'
      iv_chk = boolc( mv_view = c_action-view_raw )
      iv_act = c_action-view_raw ).

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'View'
      io_sub = lo_view_menu
    )->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    gui_services( )->register_event_handler( me ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( `<div class="repo">` ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_package_name( mv_package ) ).
    ri_html->add( `</div>` ).

    ri_html->add( _render_styles( ) ).

    ri_html->add( `<div class="markdown">` ).
    ri_html->add( _render_header( ) ).
    ri_html->add( _render_markdown( ) ).
    ri_html->add( _render_footer( ) ).
    ri_html->add( `</div>` ).

  ENDMETHOD.


  METHOD _get_logo.
    rv_html = |<img src="{ c_markdown-logo }" title="{ iv_title }" class="logo">|.
  ENDMETHOD.


  METHOD _get_markdown_data.

    FIELD-SYMBOLS <ls_markdown> TYPE ty_markdown.

    READ TABLE mt_markdown ASSIGNING <ls_markdown>
      WITH TABLE KEY path = iv_path filename = iv_filename.
    IF sy-subrc = 0.
      rv_data = <ls_markdown>-data.
    ENDIF.

  ENDMETHOD.


  METHOD _get_markdown_rendered.

    DATA:
      lo_markdown TYPE REF TO zcl_markdown,
      lo_emoji    TYPE REF TO zcl_markdown_emoji,
      lv_url      TYPE string,
      lv_branch   TYPE string,
      lv_html     TYPE string.

    CHECK ms_markdown-data IS NOT INITIAL.

    " TODO url/branch mapping
    "lv_url = mo_repo->get_url( ).
    "REPLACE REGEX '\.git$' IN lv_url WITH ''.
    "lv_branch = zcl_abapgit_git_branch_list=>get_display_name( mo_repo->get_selected_branch( ) ).

    CREATE OBJECT lo_markdown
      EXPORTING
        root_href = _get_root_href(
                       iv_url    = lv_url
                       iv_branch = lv_branch )
        root_img  = _get_root_img(
                       iv_url    = lv_url
                       iv_branch = lv_branch )
        path      = ms_markdown-path
        sapevent  = abap_true.

    lv_html = lo_markdown->text( ms_markdown-data ).

    " Links to markdown files
    "lv_html = _replace_markdown_href(
    "  iv_html      = lv_html
    "  iv_root_href = |{ lv_url }/blob/{ lv_branch }| ).

    " Emoji
    lo_emoji = zcl_markdown_emoji=>create( ).
    rv_html = lo_emoji->format_emoji( lv_html ).

  ENDMETHOD.


  METHOD _get_markdown_source.

    DATA:
      lv_num       TYPE i,
      lv_language  TYPE string,
      lv_codeblock TYPE string,
      lv_markdown  TYPE string,
      lv_markup    TYPE string,
      lt_lines     TYPE TABLE OF string.

    FIELD-SYMBOLS <lv_line> TYPE string.

    IF iv_raw = abap_true.
      lv_markdown = escape(
        val    = ms_markdown-data
        format = cl_abap_format=>e_html_text ).
    ELSE.
      lv_markdown = ms_markdown-data.
    ENDIF.

    " Same style as diffs
    rv_html = |<table class="diff_tab" id="{ ms_markdown-filename }"\n|.

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

        lv_markup = zcl_markdown_abapgit_ext_syn=>process(
          iv_source   = <lv_line>
          iv_language = lv_language ).

        IF lv_codeblock IS NOT INITIAL.
          lv_language = lv_codeblock.
        ELSE.
          lv_language = 'markdown'.
        ENDIF.
      ENDIF.

      rv_html = rv_html && |  <tr class="diff_line">\n|
        && |    <td class="num" line-num="{ lv_num }"></td>\n|
        && |    <td class="code">{ lv_markup }</td>\n|
        && |  </tr>\n|.
    ENDLOOP.

    rv_html = rv_html && |</table>\n|.

  ENDMETHOD.


  METHOD _get_mime.

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


  METHOD _get_root_href.

    IF iv_url CS 'github.com'.
      rv_root = |{ iv_url }/blob/{ iv_branch }|.
    ELSEIF iv_url CS 'gitlab.com'.
      rv_root = |{ iv_url }/-/blob/{ iv_branch }|.
    ELSE.
      " TODO: Add other git hosts
      rv_root = |{ iv_url }/blob/{ iv_branch }|.
    ENDIF.

  ENDMETHOD.


  METHOD _get_root_img.

    IF iv_url CS 'github.com'.
      rv_root = |{ iv_url }/raw/{ iv_branch }|.
    ELSEIF iv_url CS 'gitlab.com'.
      rv_root = |{ iv_url }/-/raw/{ iv_branch }|.
    ELSE.
      " TODO: Add other git hosts
      rv_root = |{ iv_url }/raw/{ iv_branch }|.
    ENDIF.

  ENDMETHOD.


  METHOD _read_markdown.

    DATA:
      lo_repo     TYPE REF TO zcl_abapgit_repo_online,
      lt_remote   TYPE zif_abapgit_git_definitions=>ty_files_tt,
      ls_markdown TYPE ty_markdown.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_remote.

    TRY.
*        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_package ).
*
*        lt_remote = lo_repo->get_files_remote( ).

        LOOP AT lt_remote ASSIGNING <ls_file> WHERE
          filename CP '*.MD' OR filename CP '*.TXT' OR
          filename CP 'LICENSE*' OR filename CP 'CHANGELOG*'.

          CLEAR ls_markdown.
          ls_markdown-path     = <ls_file>-path.
          ls_markdown-filename = <ls_file>-filename.
          ls_markdown-data     = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_file>-data ).
          INSERT ls_markdown INTO TABLE rt_markdown.
        ENDLOOP.
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD _render_footer.

    ri_html = zcl_abapgit_html=>create( ).

  ENDMETHOD.


  METHOD _render_header.

    ri_html = zcl_abapgit_html=>create( ).

    IF ms_markdown-data IS INITIAL.

      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( 'No markdown found' ).
      ri_html->add( '</div>' ).

    ELSE.

      ri_html->add( '<div class="header">' ).
      ri_html->add( |{ _get_logo( ms_markdown-filename ) }&nbsp;&nbsp;| ).
      " TODO: Split into breadcrum and add links to git server
      " path / path / path / filename
      ri_html->add( |{ ms_markdown-path+1 }{ ms_markdown-filename }| ).
      ri_html->add( '</div>' ).

    ENDIF.

  ENDMETHOD.


  METHOD _render_markdown.

    ri_html = zcl_abapgit_html=>create( ).

    CHECK ms_markdown-data IS NOT INITIAL.

    ri_html->add( '<div class="content syntax-hl">' ).

    CASE mv_view.
      WHEN c_action-view_rendered.
        ri_html->add( '<div class="html">' ).
        ri_html->add( _get_markdown_rendered( ) ).
      WHEN c_action-view_source.
        ri_html->add( '<div class="source">' ).
        ri_html->add( _get_markdown_source( abap_false ) ).
      WHEN c_action-view_raw.
        ri_html->add( '<div class="source">' ).
        ri_html->add( _get_markdown_source( abap_true ) ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD _render_styles.

    DATA lv_css TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    " Emoji Styles
    lv_css = concat_lines_of(
      table = zcl_markdown_abapgit_ext_emoji=>create( )->get_emoji_css( )
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


  METHOD _replace_markdown_href.

    DATA lv_regex TYPE string.

    " Find all links to markdown files and replace with sapevent
    " Based on https://gist.github.com/nopjia/e94b5f822744b60cd106 - g1 = path, g2 = filename, g3 = ext
    lv_regex = replace(
      val  = escape(
        val    = iv_root_href
        format = cl_abap_format=>e_regex )
      sub  = '/'
      with = '\/'
      occ  = 0 )
      && '([^"]*\/)?([^\."]*)(\.[m|M][d|D])'.

    rv_html = replace(
      val   = iv_html
      regex = lv_regex
      with  = |sapevent:select_file?KEY={ mv_package }&PATH=$1&FILENAME=$2$3|
      occ   = 0 ).

  ENDMETHOD.
ENDCLASS.
