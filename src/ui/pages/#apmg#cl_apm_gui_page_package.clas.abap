CLASS /apmg/cl_apm_gui_page_package DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI Package View
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      /apmg/if_apm_gui_event_handler,
      /apmg/if_apm_gui_hotkeys,
      /apmg/if_apm_gui_renderable,
      /apmg/if_apm_gui_menu_provider.

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
        VALUE(result) TYPE REF TO /apmg/if_apm_gui_renderable
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

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
      package_json TYPE /apmg/if_apm_types=>ty_package_json.

    METHODS get_toolbar
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_html_toolbar
      RAISING
        /apmg/cx_apm_error.

    METHODS get_markdown
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS get_package_json
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_package_json
      RAISING
        /apmg/cx_apm_error.

    METHODS get_json_data
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

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
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_top
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_header
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_header_content
      IMPORTING
        !html  TYPE REF TO /apmg/if_apm_html
        !image TYPE string
        !text  TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS render_content
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_markdown
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_markdown_source
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
        !raw  TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

    METHODS render_dependencies
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_dependencies_table
      IMPORTING
        !html                TYPE REF TO /apmg/if_apm_html
        !dependencies        TYPE /apmg/if_apm_types=>ty_dependencies
        !bundle_dependencies TYPE string_table OPTIONAL
      RAISING
        /apmg/cx_apm_error.

    METHODS render_json
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

    METHODS render_footer
      IMPORTING
        !html TYPE REF TO /apmg/if_apm_html
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_page_package IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_event_handler~on_event.

    " TODO: Add "copy to clipboard" and "download to file"
    " see zcl_fileview_abapgit_ext_ui
    CASE ii_event->mv_action.
      WHEN c_action-view_readme OR c_action-view_readme_code OR c_action-view_readme_raw.

        markdown-data = get_markdown( ).

        view = ii_event->mv_action.
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-view_dependencies OR c_action-view_json.

        package_json = get_package_json( ).

        view = ii_event->mv_action.
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-edit_readme.

        rs_handled-page  = /apmg/cl_apm_gui_page_db_entry=>create(
          key          = /apmg/cl_apm_readme=>get_package_key( package )
          edit_mode    = abap_true
          back_on_save = abap_true ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-new_page_w_bookmark.

      WHEN c_action-edit_json.

        rs_handled-page  = /apmg/cl_apm_gui_page_db_entry=>create(
          key          = /apmg/cl_apm_package_json=>get_package_key( package )
          edit_mode    = abap_true
          back_on_save = abap_true ).
        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-new_page_w_bookmark.

      WHEN c_action-update_dependencies.

        DATA(registry) = /apmg/cl_apm_settings=>factory( )->get( )-registry.

        /apmg/cl_apm_command_update=>run(
          registry = registry
          package  = package ).

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-re_render.

      WHEN c_action-add_dependency OR c_action-remove_dependency.

        /apmg/cl_apm_roadmap=>planned( `The feature has not been implemented yet. `
          && `Edit the manifest and then update the dependencies.` ).

        rs_handled-state = /apmg/cl_apm_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_hotkeys~get_hotkey_actions.

    DATA hotkey_action LIKE LINE OF rt_hotkey_actions.

    hotkey_action-ui_component = 'Package View'.

    hotkey_action-description = |Readme|.
    hotkey_action-action      = c_action-view_readme.
    hotkey_action-hotkey      = |r|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Dependencies|.
    hotkey_action-action      = c_action-view_dependencies.
    hotkey_action-hotkey      = |d|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Manifest|.
    hotkey_action-action      = c_action-view_json.
    hotkey_action-hotkey      = |m|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    " Commands
    hotkey_action-description = |Publish|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_publish.
    hotkey_action-hotkey      = |p|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Unpublish|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_unpublish.
    hotkey_action-hotkey      = |q|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

    hotkey_action-description = |Uninstall|.
    hotkey_action-action      = /apmg/if_apm_gui_router=>c_action-apm_uninstall.
    hotkey_action-hotkey      = |u|.
    INSERT hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_menu_provider~get_menu.

    CONSTANTS:
      c_key          TYPE string VALUE `?key=`,
      c_action_class TYPE string VALUE `action_link`.

    DATA(commands) = /apmg/cl_apm_html_toolbar=>create( 'apm-package-view-commands' ).

    DATA(id) = /apmg/cl_apm_package_json=>get_id_from_package( package ).

    commands->add(
      iv_txt      = 'Publish'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_publish }{ c_key }{ id }|
    )->add(
      iv_txt      = 'Deprecate'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_deprecate }{ c_key }{ id }|
    )->add(
      iv_txt      = 'Undeprecate'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_undeprecate }{ c_key }{ id }|
    )->add(
      iv_txt      = 'Danger'
      iv_typ      = /apmg/if_apm_html=>c_action_type-separator
    )->add(
      iv_txt      = 'Unpublish'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_unpublish }{ c_key }{ id }|
      iv_class    = 'red'
    )->add(
      iv_txt      = 'Uninstall'
      iv_act      = |{ /apmg/if_apm_gui_router=>c_action-apm_uninstall }{ c_key }{ id }|
      iv_class    = 'red' ).

    DATA(toolbar) = /apmg/cl_apm_html_toolbar=>create( 'apm-package-view' )->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'markdown' ) && ' Readme'
      iv_act      = c_action-view_readme
    )->add(
      " TODO: Replace with dependencies icon
      iv_txt      = /apmg/cl_apm_html=>icon( 'code-fork-solid' ) && ' Dependencies'
      iv_act      = c_action-view_dependencies
    )->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'code-solid' ) && ' Manifest'
      iv_act      = c_action-view_json
    )->add(
      iv_txt      = /apmg/cl_apm_html=>icon( 'chevron-right' ) && ' Commands'
      io_sub      = commands
    )->add(
      iv_txt      = 'Back'
      iv_act      = /apmg/if_apm_gui_router=>c_action-go_back ).

    ro_toolbar = toolbar.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    register_handlers( ).

    DATA(html) = /apmg/cl_apm_html=>create( ).

    markdown-data = get_markdown( ).
    package_json  = get_package_json( ).

    render_styles( html ).
    render_top( html ).

    html->add( '<div class="markdown">' ).

    render_header( html ).
    render_content( html ).
    render_footer( html ).

    html->add( '</div>' ).

    ri_html = html.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    IF package IS INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Missing package'.
    ENDIF.

    me->package       = package.
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

    DATA(component) = NEW /apmg/cl_apm_gui_page_package( package ).

    result = /apmg/cl_apm_gui_page_hoc=>create(
      page_title         = 'Package View'
      page_menu_provider = component
      child_component    = component ).

  ENDMETHOD.


  METHOD get_json_data.

    " Always load data since it can be edited in another page
    result = /apmg/cl_apm_package_json=>factory( package )->load( )->get_json( ).

  ENDMETHOD.


  METHOD get_markdown.

    " Always load data since it can be edited in another page
    result = /apmg/cl_apm_readme=>factory( package )->load( )->get( ).

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

    " Always load data since it can be edited in another page
    result = /apmg/cl_apm_package_json=>factory( package )->load( )->get( ).

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

        DATA(readme_menu) = /apmg/cl_apm_html_toolbar=>create( 'apm-package-readme' )->add(
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

        result = /apmg/cl_apm_html_toolbar=>create( 'apm-package-readme-actions' )->add(
          iv_txt = 'View'
          io_sub = readme_menu
        )->add(
          iv_txt = 'Edit'
          iv_act = |{ c_action-edit_readme }?key={ package }| ).

      WHEN c_action-view_dependencies.

        result = /apmg/cl_apm_html_toolbar=>create( 'apm-package-dependencies-actions' )->add(
          iv_txt = 'Add'
          iv_act = |{ c_action-add_dependency }?key={ package }|
        )->add(
          iv_txt = 'Remove'
          iv_act = |{ c_action-remove_dependency }?key={ package }|
        )->add(
          iv_txt = 'Update'
          iv_act = |{ c_action-update_dependencies }?key={ package }| ).

      WHEN c_action-view_json.

        result = /apmg/cl_apm_html_toolbar=>create( 'apm-package-manifest-actions' )->add(
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

    DATA installed_package TYPE /apmg/if_apm_package_json=>ty_package.

    DATA(list) = /apmg/cl_apm_package_json=>list( instanciate = abap_true ).

    html->add( '<table width="100%">' ).
    html->add( '<tr>' ).
    html->add( '<th width="30%">Range</th>' ).
    html->add( '<th width="10%">Type</th>' ).
    html->add( '<th width="40%">Package</th>' ).
    html->add( '<th width="10%">Version</th>' ).
    html->add( '<th width="10%">Status</th>' ).
    html->add( '</tr>' ).

    LOOP AT dependencies ASSIGNING FIELD-SYMBOL(<dependency>).
      html->add( '<tr>' ).
      html->td( get_package_boxed(
        name  = <dependency>-key
        value = <dependency>-range ) ).

      CLEAR installed_package.
      IF line_exists( bundle_dependencies[ table_line = <dependency>-key ] ).
        IF line_exists( list[ name = <dependency>-key parent = package bundle = abap_true ] ) ##PRIMKEY[NAME].
          installed_package = list[ name = <dependency>-key parent = package bundle = abap_true ] ##PRIMKEY[NAME].
        ENDIF.
        html->td( 'Bundled' ).
      ELSEIF line_exists( list[ KEY name COMPONENTS name = <dependency>-key ] ).
        installed_package = list[ KEY name COMPONENTS name = <dependency>-key ].
        html->td( 'Global' ).
      ELSE.
        html->td( '' ).
      ENDIF.

      IF installed_package IS INITIAL.
        html->td( '' ).
        html->td( '' ).
      ELSE.
        html->td( ii_content = /apmg/cl_apm_gui_chunk_lib=>render_package_name( installed_package-package ) ).
        html->td( installed_package-version ).
      ENDIF.

      DATA(satisfies) = /apmg/cl_apm_semver_functions=>satisfies(
        version = installed_package-version
        range   = <dependency>-range ).

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
            image = /apmg/cl_apm_html=>icon( 'markdown' )
            text  = |{ markdown-path+1 }{ markdown-filename }| ).
        ENDIF.

      WHEN c_action-view_dependencies.

        " TODO: Replace with dependencies icon
        render_header_content(
          html   = html
          image = /apmg/cl_apm_html=>icon( 'code-fork-solid' )
          text  = 'Dependencies' ).

      WHEN c_action-view_json.

        render_header_content(
          html   = html
          image = /apmg/cl_apm_html=>icon( 'code-solid' )
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

    DATA(highlighter) = /apmg/cl_apm_highlighter_facto=>create( '*.json' ).

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

    DATA(markdown_service) = NEW /apmg/cl_apm_markdown(
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
    html->add( /apmg/cl_apm_emoji=>create( )->format_emoji( markdown_text ) ).
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
        FIND REGEX '^```\s*(.*)' IN <line> SUBMATCHES DATA(codeblock) ##REGEX_POSIX.
        IF sy-subrc = 0.
          code_block_language = 'markdown'. " falls back to txt
        ENDIF.

        markup = /apmg/cl_apm_markdown_syn=>process(
          source   = <line>
          language = code_block_language ).

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
      table = /apmg/cl_apm_emoji=>create( )->get_emoji_css( )
      sep   = cl_abap_char_utilities=>newline ).

    html->add( '<style>' ).
    html->add( emoji_styles ).
    html->add( '</style>' ).

    " Markdown Styles
    html->add( '<style>' ).
    html->add( /apmg/cl_apm_markdown=>styles( ) ).
    html->add( '</style>' ).

  ENDMETHOD.


  METHOD render_top.

    html->add( '<div class="paddings">' ).

    html->add( '<table class="w100"><tr>' ).
    html->add( '<td>' ).
    html->add( /apmg/cl_apm_gui_chunk_lib=>render_package_name( package ) ).
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
ENDCLASS.
