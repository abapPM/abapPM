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
        view     TYPE string VALUE 'view_readme',
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
        view_readme         TYPE string VALUE 'view_readme',
        view_readme_code    TYPE string VALUE 'view_readme_code',
        view_readme_raw     TYPE string VALUE 'view_readme_raw',
        edit_readme         TYPE string VALUE 'edit_readme',
        view_json           TYPE string VALUE 'view_json',
        edit_json           TYPE string VALUE 'edit_json',
        view_dependencies   TYPE string VALUE 'view_dependencies',
        add_dependency      TYPE string VALUE 'add_dependency',
        remove_dependency   TYPE string VALUE 'remove_dependency',
        update_dependencies TYPE string VALUE 'update_dependencies',
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
      END OF ty_markdown.

    DATA:
      package      TYPE devclass,
      view         TYPE string,
      markdown     TYPE ty_markdown,
      package_json TYPE zif_abappm_types=>ty_package_json.

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
        VALUE(result) TYPE zif_abappm_types=>ty_package_json
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

    METHODS render_dependencies_table
      IMPORTING
        !html                TYPE REF TO zif_abapgit_html
        !dependencies        TYPE zif_abappm_types=>ty_dependencies
        !bundle_dependencies TYPE string_table OPTIONAL
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
      mime_data  TYPE STANDARD TABLE OF w3mime WITH KEY line.

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
      WHEN c_action-view_readme OR c_action-view_readme_code OR c_action-view_readme_raw.

        DATA(readme_menu) = zcl_abapgit_html_toolbar=>create( 'readme' )->add(
          iv_txt = 'Rendered'
          iv_chk = boolc( view = c_action-view_readme OR view IS INITIAL )
          iv_act = c_action-view_readme
         )->add(
          iv_txt = 'Markdown'
          iv_chk = boolc( view = c_action-view_readme_code )
          iv_act = c_action-view_readme_code
         )->add(
          iv_txt = 'Raw'
          iv_chk = boolc( view = c_action-view_readme_raw )
          iv_act = c_action-view_readme_raw ).

        result = zcl_abapgit_html_toolbar=>create( 'toolbar' )->add(
          iv_txt = 'View'
          io_sub = readme_menu
        )->add(
          iv_txt = 'Edit'
          iv_act = |{ c_action-edit_readme }?key={ package }| ).

      WHEN c_action-view_dependencies.

        result = zcl_abapgit_html_toolbar=>create( 'toolbar' )->add(
          iv_txt = 'Add'
          iv_act = |{ c_action-add_dependency }?key={ package }|
        )->add(
          iv_txt = 'Remove'
          iv_act = |{ c_action-remove_dependency }?key={ package }|
        )->add(
          iv_txt = 'Update'
          iv_act = |{ c_action-update_dependencies }?key={ package }| ).

      WHEN c_action-view_json.

        result = zcl_abapgit_html_toolbar=>create( 'toolbar' )->add(
          iv_txt = 'Edit'
          iv_act = |{ c_action-edit_json }?key={ package }| ).

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.


  ENDMETHOD.


  METHOD render_content.

    html->add( '<div class="content syntax-hl">' ).

    CASE view.
      WHEN c_action-view_readme.

        render_markdown( html ).

      WHEN c_action-view_readme_code.

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

      render_dependencies_table(
        html                = html
        dependencies        = package_json-dependencies
        bundle_dependencies = package_json-bundle_dependencies ).

      none = abap_false.
    ENDIF.

    IF package_json-dev_dependencies IS NOT INITIAL.
      html->add( |<h2>Dev Dependencies ({ lines( package_json-dev_dependencies ) })</h2>| ).

      render_dependencies_table(
        html         = html
        dependencies = package_json-dev_dependencies ).

      none = abap_false.
    ENDIF.

    IF package_json-peer_dependencies IS NOT INITIAL.
      html->add( |<h2>Peer Dependencies ({ lines( package_json-peer_dependencies ) })</h2>| ).

      render_dependencies_table(
        html         = html
        dependencies = package_json-peer_dependencies ).

      none = abap_false.
    ENDIF.

    IF package_json-optional_dependencies IS NOT INITIAL.
      html->add( |<h2>Optional Dependencies ({ lines( package_json-optional_dependencies ) })</h2>| ).

      render_dependencies_table(
        html         = html
        dependencies = package_json-optional_dependencies ).

      none = abap_false.
    ENDIF.

    IF none = abap_true.
      html->add( '<h3>This package has no dependencies</h3>' ).
    ENDIF.

    html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_dependencies_table.

    DATA installed_package TYPE zif_abappm_package_json=>ty_package.

    DATA(list) = zcl_abappm_package_json=>list( instanciate = abap_true ).

    html->add( '<table width="100%">' ).
    html->add( '<tr>' ).
    html->add( '<th width="30%">Range</th>' ).
    html->add( '<th width="10%">Type</th>' ).
    html->add( '<th width="30%">Package</th>' ).
    html->add( '<th width="15%">Version</th>' ).
    html->add( '<th width="15%">Status</th>' ).
    html->add( '</tr>' ).

    LOOP AT dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
      html->add( '<tr>' ).
      html->td( get_package_boxed(
        name  = <dependency>-name
        value = <dependency>-range ) ).
      IF line_exists( bundle_dependencies[ table_line = <dependency>-name ] ).
        CLEAR installed_package.
        " TODO: which package and version?
        html->td( 'Bundled' ).
        html->td( '' ).
        html->td( '' ).
      ELSEIF line_exists( list[ KEY name COMPONENTS name = <dependency>-name ] ).
        installed_package = list[ KEY name COMPONENTS name = <dependency>-name ].
        html->td( 'Global' ).
        html->td( ii_content = zcl_abapgit_gui_chunk_lib=>render_package_name( installed_package-package ) ).
        html->td( installed_package-version ).
      ENDIF.

      TRY.
          DATA(satisfies) = zcl_abappm_semver_functions=>satisfies(
            version = installed_package-version
            range   = <dependency>-range ).
        CATCH zcx_abappm_semver_error INTO DATA(error).
          zcx_abapgit_exception=>raise_with_text( error ).
      ENDTRY.

      IF satisfies = abap_true.
        html->td( html->icon( 'check/success' ) ).
      ELSE.
        html->td( html->icon( 'bolt/warning' ) ).
      ENDIF.

      html->add( '</tr>' ).
    ENDLOOP.

    html->add( '</table>' ).

  ENDMETHOD.


  METHOD render_footer.

    " Someday...
    html->add( '' ).

  ENDMETHOD.


  METHOD render_header.

    CASE view.
      WHEN c_action-view_readme
        OR c_action-view_readme_code
        OR c_action-view_readme_raw.

        IF markdown-data IS INITIAL.
          html->add( '<div class="dummydiv success">' ).
          html->add( 'Readme not found' ).
          html->add( '</div>' ).
        ELSE.
          render_header_content(
            html   = html
            image = zcl_abapgit_html=>icon( 'markdown' )
            text  = |{ markdown-path+1 }{ markdown-filename }| ).
        ENDIF.

      WHEN c_action-view_dependencies.

        " TODO: Replace with "chain-link" icon
        render_header_content(
          html   = html
          image = zcl_abapgit_html=>icon( 'file-alt' )
          text  = 'Dependencies' ).

      WHEN c_action-view_json.

        render_header_content(
          html   = html
          image = zcl_abapgit_html=>icon( 'code-solid' )
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
    DATA(emoji_styles) = concat_lines_of(
      table = zcl_abappm_markdown_emoji=>create( )->get_emoji_css( )
      sep   = cl_abap_char_utilities=>newline ).

    html->add( '<style>' ).
    html->add( emoji_styles ).
    html->add( '</style>' ).

    " Markdown Styles
    html->add( '<style>' ).
    html->add( zcl_abappm_markdown=>styles( ) ).
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
      WHEN c_action-view_readme OR c_action-view_readme_code OR c_action-view_readme_raw.

        markdown-data = get_markdown( ).

        view = ii_event->mv_action.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-view_dependencies OR c_action-view_json.

        package_json = get_package_json( ).

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

      WHEN c_action-update_dependencies.

        TRY.
            DATA(registry) = zcl_abappm_settings=>factory( )->get( )-registry.

            zcl_abappm_command_update=>run(
              registry = registry
              package  = package ).
          CATCH zcx_abappm_error INTO DATA(error).
            zcx_abapgit_exception=>raise_with_text( error ).
        ENDTRY.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-add_dependency OR c_action-remove_dependency.

        zcx_abapgit_exception=>raise( `The feature has not been implemented yet. `
          && `Edit the manifest and then update the dependencies.` ).

        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'main' )->add(
      iv_txt = 'Readme'
      iv_act = c_action-view_readme
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
