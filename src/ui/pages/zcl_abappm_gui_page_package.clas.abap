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

    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_renderable,
      zif_abapgit_gui_menu_provider.

    CONSTANTS:
      BEGIN OF c_default,
        path     TYPE string VALUE '/',
        filename TYPE string VALUE 'README.md',
        view     TYPE string VALUE 'view_readme_rendered',
      END OF c_default.

    CLASS-METHODS create
      IMPORTING
        !package      TYPE devclass
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !package TYPE devclass
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
      package      TYPE devclass,
      view         TYPE string,
      markdown     TYPE ty_markdown,
      package_json TYPE zif_abappm_package_json_types=>ty_package_json.

    METHODS get_toolbar
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_html_toolbar
      RAISING
        zcx_abapgit_exception.

    METHODS get_markdown
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_package_json
      RETURNING
        VALUE(result) TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abapgit_exception.

    METHODS get_json_data
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_mime
      IMPORTING
        !mime_name    TYPE csequence
      RETURNING
        VALUE(result) TYPE xstring.

    METHODS get_root_href
      IMPORTING
        !url          TYPE string
        !branch       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_root_img
      IMPORTING
        !url          TYPE string
        !branch       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_package_boxed
      IMPORTING
        !name         TYPE string
        !value        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_styles
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_top
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_header_content
      IMPORTING
        !html  TYPE REF TO zif_abapgit_html
        !image TYPE string
        !text  TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS render_content
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_markdown
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_markdown_source
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
        !raw  TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

    METHODS render_dependencies
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_json
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_footer
      IMPORTING
        !html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abappm_gui_page_package IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    IF package IS INITIAL.
      zcx_abapgit_exception=>raise( 'Missing package' ).
    ELSE.
      me->package = package.
    ENDIF.

    view              = c_default-view.
    markdown-path     = c_default-path.
    markdown-filename = c_default-filename.
    markdown-data     = get_markdown( ).
    package_json      = get_package_json( ).

    gui_services( )->cache_asset(
      iv_type    = 'image'
      iv_subtype = 'png'
      iv_url     = |{ c_markdown-logo }|
      iv_xdata   = get_mime( c_markdown-mime ) ).

  ENDMETHOD.


  METHOD create.

    DATA(component) = NEW zcl_abappm_gui_page_package( package ).

    result = zcl_abappm_gui_page_hoc=>create(
      page_title         = 'Package View'
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD get_json_data.

    TRY.
        " Always load data since it can be edited in another page
        result = zcl_abappm_package_json=>factory( package )->load( )->get_json( ).
      CATCH zcx_abappm_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_markdown.

    TRY.
        " Always load data since it can be edited in another page
        result = zcl_abappm_readme=>factory( package )->load( )->get( ).
      CATCH zcx_abappm_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_mime.

    DATA:
      size_value TYPE wwwparams-value,
      mime_data  TYPE STANDARD TABLE OF w3mime.

    DATA(key) = VALUE wwwdatatab(
      relid = 'MI'
      objid = mime_name ).

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = key-relid
        objid            = key-objid
        name             = 'filesize'
      IMPORTING
        value            = size_value
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(size) = size_value.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = key
      TABLES
        mime              = mime_data
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT mime_data INTO DATA(mime_line).
      CONCATENATE result mime_line-line INTO result IN BYTE MODE.
    ENDLOOP.

    result = result(size).

  ENDMETHOD.


  METHOD get_package_boxed.

    IF value IS INITIAL.
      result = |<span class="transport-box">{ name }</span>|.
    ELSE.
      result = |<span class="transport-box">{ name }: { value }</span>|.
    ENDIF.

  ENDMETHOD.


  METHOD get_package_json.

    TRY.
        " Always load data since it can be edited in another page
        result = zcl_abappm_package_json=>factory( package )->load( )->get( ).
      CATCH zcx_abappm_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_root_href.

    IF url CS 'github.com'.
      result = |{ url }/blob/{ branch }|.
    ELSEIF url CS 'gitlab.com'.
      result = |{ url }/-/blob/{ branch }|.
    ELSE.
      " TODO: Add other git hosts
      result = |{ url }/blob/{ branch }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_root_img.

    IF url CS 'github.com'.
      result = |{ url }/raw/{ branch }|.
    ELSEIF url CS 'gitlab.com'.
      result = |{ url }/-/raw/{ branch }|.
    ELSE.
      " TODO: Add other git hosts
      result = |{ url }/raw/{ branch }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_toolbar.

    CASE view.
      WHEN c_action-view_readme_rendered OR c_action-view_readme_source OR c_action-view_readme_raw.
        DATA(action) = c_action-edit_readme.
      WHEN c_action-view_dependencies.
        action = c_action-edit_dependencies.
      WHEN c_action-view_json.
        action = c_action-edit_json.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    result = zcl_abapgit_html_toolbar=>create( 'toolbar' )->add(
      iv_txt = 'Edit'
      iv_act = |{ action }?key={ package }| ).

  ENDMETHOD.


  METHOD render_content.

    html->add( '<div class="content syntax-hl">' ).

    CASE view.
      WHEN c_action-view_readme_rendered.
        render_markdown( html ).
      WHEN c_action-view_readme_source.
        render_markdown_source(
          html = html
          raw  = abap_false ).
      WHEN c_action-view_readme_raw.
        render_markdown_source(
          html = html
          raw  = abap_true ).
      WHEN c_action-view_dependencies.
        render_dependencies( html ).
      WHEN c_action-view_json.
        render_json( html ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_dependencies.

    html->add( '<div class="html">' ).

    DATA(none) = abap_true.

    IF package_json-dependencies IS NOT INITIAL.
      html->add( |<h2>Dependencies ({ lines( package_json-dependencies ) })</h2>| ).

      LOOP AT package_json-dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
        none = abap_false.
        html->add( get_package_boxed(
          name  = <dependency>-name
          value = <dependency>-range ) ).
        " TODO: Display status of dependency
        " Is it installed and if yes in which version?
      ENDLOOP.
    ENDIF.

    IF package_json-dev_dependencies IS NOT INITIAL.
      html->add( |<h2>Dev Dependencies ({ lines( package_json-dev_dependencies ) })</h2>| ).

      LOOP AT package_json-dev_dependencies ASSIGNING <dependency>.
        none = abap_false.
        html->add( get_package_boxed(
          name  = <dependency>-name
          value = <dependency>-range ) ).
      ENDLOOP.
    ENDIF.

    IF package_json-peer_dependencies IS NOT INITIAL.
      html->add( |<h2>Peer Dependencies ({ lines( package_json-peer_dependencies ) })</h2>| ).

      LOOP AT package_json-peer_dependencies ASSIGNING <dependency>.
        none = abap_false.
        html->add( get_package_boxed(
          name  = <dependency>-name
          value = <dependency>-range ) ).
        " TODO: Display status of dependency
        " Is it installed and if yes in which version?
      ENDLOOP.
    ENDIF.

    IF package_json-optional_dependencies IS NOT INITIAL.
      html->add( |<h2>Optional Dependencies ({ lines( package_json-optional_dependencies ) })</h2>| ).

      LOOP AT package_json-optional_dependencies ASSIGNING <dependency>.
        none = abap_false.
        html->add( get_package_boxed(
          name  = <dependency>-name
          value = <dependency>-range ) ).
        " TODO: Display status of dependency
        " Is it installed and if yes in which version?
      ENDLOOP.
    ENDIF.

    IF package_json-bundle_dependencies IS NOT INITIAL.
      html->add( |<h2>Bundle Dependencies ({ lines( package_json-bundle_dependencies ) })</h2>| ).

      LOOP AT package_json-bundle_dependencies ASSIGNING FIELD-SYMBOL(<bundle>).
        none = abap_false.
        html->add( get_package_boxed( <bundle> ) ).
      ENDLOOP.
    ENDIF.

    IF none = abap_true.
      html->add( '<h3>This package has no dependencies</h3>' ).
    ENDIF.

    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_footer.

    " Someday...
    html->add( '' ).

  ENDMETHOD.


  METHOD render_header.

    CASE view.
      WHEN c_action-view_readme_rendered
        OR c_action-view_readme_source
        OR c_action-view_readme_raw.

        IF markdown-data IS INITIAL.
          html->add( '<div class="dummydiv success">' ).
          html->add( 'Readme not found' ).
          html->add( '</div>' ).
        ELSE.
          render_header_content(
            html   = html
            image = |<img src="{ c_markdown-logo }" title="{ markdown-filename }" class="logo">  |
            text  = |{ markdown-path+1 }{ markdown-filename }| ).
        ENDIF.

      WHEN c_action-view_dependencies
        OR c_action-view_json.

        render_header_content(
          html   = html
          image = zcl_abapgit_html=>icon( 'file' )
          text  = 'package.abap.json' ).

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD render_header_content.

    html->add( '<div class="header">' ).
    html->add( image ).
    html->add( '<span class="indent5em">' ).
    html->add( text ).
    html->add( '</span>' ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_json.

    DATA(highlighter) = zcl_abapgit_syntax_factory=>create( '*.json' ).

    DATA(json_data) = get_json_data( ).
    SPLIT json_data AT |\n| INTO TABLE DATA(json).

    html->add( '<div class="source">' ).
    html->add( '<pre class="syntax-hl">' ).
    LOOP AT json INTO json_data.
      html->add( highlighter->process_line( json_data ) ).
    ENDLOOP.
    html->add( '</pre>' ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_markdown.

    DATA(url) = package_json-repository-url.

    " Heuristic to determine which branch was used for README urls and images
    IF markdown-data CS '/master/'.
      DATA(branch) = `master`.
    ELSE.
      branch = `main`.
    ENDIF.

    DATA(markdown_service) = NEW zcl_abappm_markdown(
      root_href = get_root_href(
                     url    = url
                     branch = branch )
      root_img  = get_root_img(
                     url    = url
                     branch = branch )
      path      = markdown-path
      sapevent  = abap_true ).

    DATA(markdown_text) = markdown_service->text( markdown-data ).

    " Output with Emoji
    html->add( '<div class="html">' ).
    html->add( zcl_abappm_markdown_emoji=>create( )->format_emoji( markdown_text ) ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_markdown_source.

    DATA(markdown_text) = markdown-data.

    IF raw = abap_true.
      markdown_text = escape(
        val    = markdown_text
        format = cl_abap_format=>e_html_text ).
    ENDIF.

    " Same style as diffs
    html->add( '<div class="source">' ).
    html->add( |<table class="diff_tab" id="{ markdown-filename }">| ).

    SPLIT markdown_text AT cl_abap_char_utilities=>newline INTO TABLE DATA(lines).

    DATA(code_block_language) = `markdown`.

    LOOP AT lines ASSIGNING FIELD-SYMBOL(<line>).
      DATA(line_number) = sy-tabix.

      IF raw = abap_true.
        DATA(markup) = <line>.
      ELSE.
        " Detect code block
        FIND REGEX '^```\s*(.*)' IN <line> SUBMATCHES DATA(codeblock).
        IF sy-subrc = 0.
          code_block_language = 'markdown'. " falls back to txt
        ENDIF.

        markup = zcl_abappm_markdown_syn=>process(
          iv_source   = <line>
          iv_language = code_block_language ).

        IF codeblock IS NOT INITIAL.
          code_block_language = codeblock.
        ELSE.
          code_block_language = 'markdown'.
        ENDIF.
      ENDIF.

      html->add( '<tr class="diff_line">' ).
      html->add( |<td class="num" line-num="{ line_number }"></td>| ).
      html->add( |<td class="code">{ markup }</td>| ).
      html->add( '</tr>' ).
    ENDLOOP.

    html->add( '</table>' ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_styles.

    " Emoji Styles
    DATA(css) = concat_lines_of(
      table = zcl_abappm_markdown_emoji=>create( )->get_emoji_css( )
      sep   = cl_abap_char_utilities=>newline ).

    html->add( '<style>' ).
    html->add( css ).
    html->add( '</style>' ).

    " Markdown Styles
    html->add( '<style>' ).
    html->add( '.markdown' ).
    html->add( '{ background-color: #f2f2f2; padding: 15px; }' ).
    html->add( '.markdown .logo' ).
    html->add( '{ width: 36px; height: 22px; margin-top: -4px; }' ).
    html->add( '.markdown .header,' ).
    html->add( '.markdown .content' ).
    html->add( '{ background-color: #ffffff; border: 1px solid #d8dee4; display: block; }' ).
    html->add( '.markdown .header' ).
    html->add( '{ font-size: larger; margin-bottom: 15px; padding: 15px; }' ).
    html->add( '.markdown .content' ).
    html->add( '{ padding: 25px; }' ).
    html->add( '.markdown .html' ).
    html->add( '{ max-width: 1024px; margin: 0 auto; padding: 25px; }' ).
    " Markdown View
    html->add( '.markdown .source' ).
    html->add( '{ font-family: Consolas,Courier,monospace; font-size: 12pt; padding: 25px;' ).
    html->add( '  max-width: 1024px; margin: 0 auto; }' ).
    html->add( '.markdown .source table' ).
    html->add( '{ border: 1px solid #d8dee4; }' ).
    html->add( '.markdown .source td' ).
    html->add( '{ border-top: 0px; border-bottom: 0px; padding-top: 0; padding-bottom: 0;' ).
    html->add( '  line-height: 20px; vertical-align: top; }' ).
    " Syntax Highlight
    html->add( '.markdown .syntax-hl .heading' ).
    html->add( '{ color: blue; }' ).
    html->add( '.markdown .syntax-hl .link' ).
    html->add( '{ color: purple; }' ).
    html->add( '.markdown .syntax-hl .url' ).
    html->add( '{ color: green; }' ).
    html->add( '.markdown .syntax-hl .html' ).
    html->add( '{ padding: 0; }' ).
    html->add( '.markdown .syntax-hl .bold' ).
    html->add( '{ font-weight: bold; }' ).
    " HTML Tags
    html->add( '.markdown h1,' ).
    html->add( '.markdown h2' ).
    html->add( '{ border-bottom: 1px solid #d8dee4; box-sizing: border-box; }' ).
    html->add( '.markdown img' ).
    html->add( '{ border: 0; box-sizing: border-box; max-width: 100%; vertical-align: middle; }' ).
    html->add( '.markdown table' ).
    html->add( '{ border: 1px solid #ddd; border-radius: 3px; }' ).
    html->add( '.markdown th,' ).
    html->add( '{ color: #4078c0; background-color: #edf2f9; border-bottom-color: #ddd; }' ).
    html->add( '.markdown th,' ).
    html->add( '.markdown td' ).
    html->add( '{ border: 1px solid #ddd; padding: 6px 13px; }' ).
    html->add( '.markdown tr:first-child td' ).
    html->add( '{ border-top: 0; }' ).
    html->add( '.markdown hr' ).
    html->add( '{ background-color: #eee; margin: 24px 0; overflow: hidden; padding: 0; }' ).
    html->add( '.markdown mark' ).
    html->add( '{ background-color: #fff8e0; border-radius: 6px; margin: 0; padding: .2em .4em; }' ).
    html->add( '.markdown blockquote' ).
    html->add( '{ background-color: #eee; border-left: 3px solid #303d36; border-radius: 6px;' ).
    html->add( '  margin: 0 0 16px; padding: 1px 1em; }' ).
    " Code blocks
    html->add( '.markdown pre' ).
    html->add( '{ background-color: #eee; border-radius: 6px; display: block;' ).
    html->add( '  margin-bottom: 16px; margin-top: 0; overflow: auto; overflow-wrap: normal;' ).
    html->add( '  padding: 16px; word-break: normal; box-sizing: border-box; white-space: pre;' ).
    html->add( '  font-family: Consolas, Courier, monospace; font-size: 14px; }' ).
    html->add( '.markdown p code' ).
    html->add( '{ background-color: #eee; border-radius: 6px; margin: 0; padding: .2em .4em;' ).
    html->add( '  font-family: Consolas, Courier, monospace; font-size: 14px; }' ).
    html->add( '.markdown pre code' ).
    html->add( '{ background-color: transparent; border-style: initial;' ).
    html->add( '  border-width: 0; box-sizing: border-box; display: inline; margin: 0;' ).
    html->add( '  overflow: visible; word-break: normal; overflow-wrap: normal;' ).
    html->add( '  padding: 0; white-space: pre;' ).
    html->add( '  font-family: Consolas, Courier, monospace; font-size: 14px; }' ).
    html->add( '</style>' ).

  ENDMETHOD.


  METHOD render_top.

    html->add( '<div class="paddings">' ).
    html->add( '<table class="w100"><tr>' ).
    html->add( '<td>' ).
    html->add( zcl_abapgit_gui_chunk_lib=>render_package_name( package ) ).
    html->add( '<span class="indent5em">' ).
    html->add( get_package_boxed(
      name  = package_json-name
      value = package_json-version ) ).
    html->add( '</span>' ).
    html->add( '</td>' ).
    html->add( '<td class="right">' ).
    html->add( get_toolbar( )->render( iv_right = abap_true ) ).
    html->add( '</td>' ).
    html->add( '</tr></table>' ).
    html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-view_readme_rendered OR c_action-view_readme_source OR c_action-view_readme_raw
        OR c_action-view_dependencies OR c_action-view_json.

        view = ii_event->mv_action.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-edit_readme.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create(
          key       = zcl_abappm_readme=>get_package_key( package )
          edit_mode = abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-edit_json.
        rs_handled-page  = zcl_abappm_gui_page_db_entry=>create(
          key       = zcl_abappm_package_json=>get_package_key( package )
          edit_mode = abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA(readme_menu) = zcl_abapgit_html_toolbar=>create( 'readme' )->add(
      iv_txt = 'Rendered'
      iv_chk = boolc( view = c_action-view_readme_rendered OR view IS INITIAL )
      iv_act = c_action-view_readme_rendered
     )->add(
      iv_txt = 'Markdown'
      iv_chk = boolc( view = c_action-view_readme_source )
      iv_act = c_action-view_readme_source
     )->add(
      iv_txt = 'Raw'
      iv_chk = boolc( view = c_action-view_readme_raw )
      iv_act = c_action-view_readme_raw ).

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'main' )->add(
      iv_txt = 'Readme'
      io_sub = readme_menu
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

    DATA(html) = zcl_abapgit_html=>create( ).

    render_styles( html ).
    render_top( html ).

    html->add( '<div class="markdown">' ).

    render_header( html ).
    render_content( html ).
    render_footer( html ).

    html->add( '</div>' ).

    ri_html = html.

  ENDMETHOD.
ENDCLASS.
