CLASS /apmg/cl_apm_html DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_html.

    CONSTANTS c_indent_size TYPE i VALUE 2 ##NO_TEXT.

    CLASS-METHODS class_constructor.
    CLASS-METHODS create
      IMPORTING
        !iv_initial_chunk  TYPE any OPTIONAL
      RETURNING
        VALUE(ri_instance) TYPE REF TO /apmg/if_apm_html.
    CLASS-METHODS icon
      IMPORTING
        !iv_name      TYPE string
        !iv_hint      TYPE string OPTIONAL
        !iv_class     TYPE string OPTIONAL
        !iv_onclick   TYPE string OPTIONAL
      RETURNING
        VALUE(rv_str) TYPE string.
    CLASS-METHODS checkbox
      IMPORTING
        !iv_id         TYPE string OPTIONAL
        !iv_checked    TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_html) TYPE string.
    CLASS-METHODS parse_data_attr
      IMPORTING
        !iv_str             TYPE string OPTIONAL
      RETURNING
        VALUE(rs_data_attr) TYPE /apmg/if_apm_html=>ty_data_attr.
    CLASS-METHODS set_debug_mode
      IMPORTING
        !iv_mode TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_max_indent TYPE i VALUE 200.

    TYPES:
      BEGIN OF ty_indent_context,
        no_indent_jscss TYPE abap_bool,
        within_style    TYPE abap_bool,
        within_js       TYPE abap_bool,
        within_textarea TYPE abap_bool,
        within_pre      TYPE abap_bool,
        indent          TYPE i,
        indent_str      TYPE string,
      END OF ty_indent_context,
      BEGIN OF ty_study_result,
        style_open     TYPE abap_bool,
        style_close    TYPE abap_bool,
        script_open    TYPE abap_bool,
        script_close   TYPE abap_bool,
        textarea_open  TYPE abap_bool,
        textarea_close TYPE abap_bool,
        pre_open       TYPE abap_bool,
        pre_close      TYPE abap_bool,
        tag_close      TYPE abap_bool,
        curly_close    TYPE abap_bool,
        openings       TYPE i,
        closings       TYPE i,
        singles        TYPE i,
      END OF ty_study_result.

    CLASS-DATA go_single_tags_re TYPE REF TO cl_abap_regex.

    CLASS-DATA gv_spaces TYPE string.
    CLASS-DATA gv_debug_mode TYPE abap_bool.

    DATA mt_buffer TYPE string_table.

    METHODS indent_line
      CHANGING
        !cs_context TYPE ty_indent_context
        !cv_line    TYPE string.

    METHODS study_line
      IMPORTING
        !iv_line         TYPE string
        !is_context      TYPE ty_indent_context
      RETURNING
        VALUE(rs_result) TYPE ty_study_result.

ENDCLASS.



CLASS /apmg/cl_apm_html IMPLEMENTATION.


  METHOD /apmg/if_apm_html~a.

    DATA: lv_class TYPE string,
          lv_href  TYPE string,
          lv_click TYPE string,
          lv_id    TYPE string,
          lv_act   TYPE string,
          lv_style TYPE string,
          lv_title TYPE string.

    lv_class = iv_class.

    IF iv_opt CA /apmg/if_apm_html=>c_html_opt-strong.
      lv_class = lv_class && ' emphasis'.
    ENDIF.
    IF iv_opt CA /apmg/if_apm_html=>c_html_opt-cancel.
      lv_class = lv_class && ' attention'.
    ENDIF.
    IF iv_opt CA /apmg/if_apm_html=>c_html_opt-crossout.
      lv_class = lv_class && ' crossout grey'.
    ENDIF.
    IF lv_class IS NOT INITIAL.
      SHIFT lv_class LEFT DELETING LEADING space.
      lv_class = | class="{ lv_class }"|.
    ENDIF.

    lv_href = ' href="#"'. " Default, dummy
    lv_act  = iv_act.
    IF ( iv_act IS NOT INITIAL OR iv_typ = /apmg/if_apm_html=>c_action_type-dummy )
        AND iv_opt NA /apmg/if_apm_html=>c_html_opt-crossout.
      CASE iv_typ.
        WHEN /apmg/if_apm_html=>c_action_type-url.
          IF iv_query IS NOT INITIAL.
            lv_act = lv_act && `?` && iv_query.
          ENDIF.
          lv_href  = | href="{ lv_act }"|.
        WHEN /apmg/if_apm_html=>c_action_type-sapevent.
          IF iv_query IS NOT INITIAL.
            lv_act = lv_act && `?` && iv_query.
          ENDIF.
          lv_href  = | href="sapevent:{ lv_act }"|.
        WHEN /apmg/if_apm_html=>c_action_type-onclick.
          lv_href  = ' href="#"'.
          lv_click = | onclick="{ iv_act }"|.
        WHEN /apmg/if_apm_html=>c_action_type-dummy.
          lv_href  = ' href="#"'.
      ENDCASE.
    ENDIF.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_style IS NOT INITIAL.
      lv_style = | style="{ iv_style }"|.
    ENDIF.

    IF iv_title IS NOT INITIAL.
      lv_title = | title="{ iv_title }"|.
    ENDIF.

    " Debug option to display href-link on hover
    IF gv_debug_mode = abap_true.
      lv_title = | title="{ escape(
        val    = lv_href
        format = cl_abap_format=>e_html_attr ) }"|.
    ENDIF.

    rv_str = |<a{ lv_id }{ lv_class }{ lv_href }{ lv_click }{ lv_style }{ lv_title }>|
          && |{ iv_txt }</a>|.

  ENDMETHOD.


  METHOD /apmg/if_apm_html~add.

    DATA: lv_type       TYPE c,
          li_renderable TYPE REF TO /apmg/if_apm_gui_renderable,
          lx_error      TYPE REF TO /apmg/cx_apm_error,
          lo_html       TYPE REF TO /apmg/cl_apm_html.

    FIELD-SYMBOLS: <lt_tab> TYPE string_table.

    lv_type = cl_abap_typedescr=>describe_by_data( ig_chunk )->type_kind.

    CASE lv_type.
      WHEN 'C' OR 'g'.  " Char or string
        APPEND ig_chunk TO mt_buffer.
      WHEN 'h'.         " Table
        ASSIGN ig_chunk TO <lt_tab>. " Assuming table of strings ! Will dump otherwise
        APPEND LINES OF <lt_tab> TO mt_buffer.
      WHEN 'r'.         " Object ref
        ASSERT ig_chunk IS BOUND. " Dev mistake
        TRY.
            lo_html ?= ig_chunk.
          CATCH cx_sy_move_cast_error.
            TRY.
                li_renderable ?= ig_chunk.
                lo_html ?= li_renderable->render( ).
              CATCH cx_sy_move_cast_error.
                ASSERT 1 = 0. " Dev mistake
              CATCH /apmg/cx_apm_error INTO lx_error.
                lo_html ?= create( |<span class="error">Render error: { lx_error->get_text( ) }</span>| ).
            ENDTRY.
        ENDTRY.
        APPEND LINES OF lo_html->mt_buffer TO mt_buffer.
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.

    ri_self = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_html~add_a.

    /apmg/if_apm_html~add( /apmg/if_apm_html~a(
      iv_txt   = iv_txt
      iv_act   = iv_act
      iv_query = iv_query
      iv_typ   = iv_typ
      iv_opt   = iv_opt
      iv_class = iv_class
      iv_id    = iv_id
      iv_style = iv_style
      iv_title = iv_title ) ).

    ri_self = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_html~add_checkbox.

    /apmg/if_apm_html~add( checkbox(
      iv_id      = iv_id
      iv_checked = iv_checked ) ).

    ri_self = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_html~add_icon.

    /apmg/if_apm_html~add( icon(
      iv_name    = iv_name
      iv_class   = iv_class
      iv_hint    = iv_hint
      iv_onclick = iv_onclick ) ).

    ri_self = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_html~div.
    /apmg/if_apm_html~wrap(
      iv_tag   = 'div'
      iv_content = iv_content
      ii_content = ii_content
      is_data_attr  = is_data_attr
      it_data_attrs = it_data_attrs
      iv_id    = iv_id
      iv_class = iv_class ).
    ri_self = me.
  ENDMETHOD.


  METHOD /apmg/if_apm_html~icon.

    rv_str = icon(
      iv_name    = iv_name
      iv_hint    = iv_hint
      iv_class   = iv_class
      iv_onclick = iv_onclick ).

  ENDMETHOD.


  METHOD /apmg/if_apm_html~is_empty.
    rv_yes = boolc( lines( mt_buffer ) = 0 ).
  ENDMETHOD.


  METHOD /apmg/if_apm_html~render.

    DATA: ls_context TYPE ty_indent_context,
          lt_temp    TYPE string_table.

    FIELD-SYMBOLS: <lv_line>   LIKE LINE OF lt_temp,
                   <lv_line_c> LIKE LINE OF lt_temp.

    IF iv_no_line_breaks = abap_true.
      CONCATENATE LINES OF mt_buffer INTO rv_html.
    ELSE.
      ls_context-no_indent_jscss = iv_no_indent_jscss.

      LOOP AT mt_buffer ASSIGNING <lv_line>.
        APPEND <lv_line> TO lt_temp ASSIGNING <lv_line_c>.
        indent_line( CHANGING cs_context = ls_context cv_line = <lv_line_c> ).
      ENDLOOP.

      CONCATENATE LINES OF lt_temp INTO rv_html SEPARATED BY cl_abap_char_utilities=>newline.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_html~set_title.
    /apmg/if_apm_html~mv_chunk_title = iv_title.
    ri_self = me.
  ENDMETHOD.


  METHOD /apmg/if_apm_html~td.
    /apmg/if_apm_html~wrap(
      iv_format_single_line = iv_format_single_line
      iv_tag   = 'td'
      iv_content = iv_content
      ii_content = ii_content
      iv_id    = iv_id
      iv_class = iv_class
      is_data_attr  = is_data_attr
      it_data_attrs = it_data_attrs
      iv_hint  = iv_hint ).
    ri_self = me.
  ENDMETHOD.


  METHOD /apmg/if_apm_html~th.
    /apmg/if_apm_html~wrap(
      iv_format_single_line = iv_format_single_line
      iv_tag   = 'th'
      iv_content = iv_content
      ii_content = ii_content
      iv_id    = iv_id
      iv_class = iv_class
      is_data_attr  = is_data_attr
      it_data_attrs = it_data_attrs
      iv_hint  = iv_hint ).
    ri_self = me.
  ENDMETHOD.


  METHOD /apmg/if_apm_html~wrap.

    DATA lv_open_tag TYPE string.
    DATA lv_close_tag TYPE string.
    DATA ls_data_attr LIKE LINE OF it_data_attrs.

    DATA: lv_class     TYPE string,
          lv_id        TYPE string,
          lv_data_attr TYPE string,
          lv_title     TYPE string.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_class IS NOT INITIAL.
      lv_class = | class="{ iv_class }"|.
    ENDIF.

    IF iv_hint IS NOT INITIAL.
      lv_title = | title="{ iv_hint }"|.
    ENDIF.

    IF is_data_attr IS NOT INITIAL.
      lv_data_attr = | data-{ is_data_attr-name }="{ is_data_attr-value }"|.
    ENDIF.

    LOOP AT it_data_attrs INTO ls_data_attr.
      lv_data_attr = lv_data_attr && | data-{ ls_data_attr-name }="{ ls_data_attr-value }"|.
    ENDLOOP.

    lv_open_tag = |<{ iv_tag }{ lv_id }{ lv_class }{ lv_data_attr }{ lv_title }>|.
    lv_close_tag = |</{ iv_tag }>|.

    IF ii_content IS NOT BOUND AND iv_content IS INITIAL.
      lv_open_tag = lv_open_tag && lv_close_tag.
      CLEAR lv_close_tag.
    ENDIF.

    IF iv_format_single_line = abap_true AND iv_content IS NOT INITIAL.
      /apmg/if_apm_html~add( lv_open_tag && iv_content && lv_close_tag ).
    ELSE.
      /apmg/if_apm_html~add( lv_open_tag ).
      IF ii_content IS BOUND.
        /apmg/if_apm_html~add( ii_content ).
      ELSEIF iv_content IS NOT INITIAL.
        /apmg/if_apm_html~add( iv_content ).
      ENDIF.
      IF lv_close_tag IS NOT INITIAL.
        /apmg/if_apm_html~add( lv_close_tag ).
      ENDIF.
    ENDIF.

    ri_self = me.

  ENDMETHOD.


  METHOD checkbox.

    DATA: lv_checked TYPE string.

    IF iv_checked = abap_true.
      lv_checked = |checked|.
    ENDIF.

    rv_html = |<input type="checkbox" { lv_checked } |.
    IF iv_id IS NOT INITIAL.
      rv_html = rv_html && |id="{ iv_id }"|.
    ENDIF.

    rv_html = rv_html && `/>`.

  ENDMETHOD.


  METHOD class_constructor.

    CREATE OBJECT go_single_tags_re
      EXPORTING
        pattern     = '<(AREA|BASE|BR|COL|COMMAND|EMBED|HR|IMG|INPUT|LINK|META|PARAM|SOURCE|!)'
        ignore_case = abap_false.

    gv_spaces = repeat(
      val = ` `
      occ = c_max_indent ).

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ri_instance TYPE /apmg/cl_apm_html.
    IF iv_initial_chunk IS NOT INITIAL.
      ri_instance->add( iv_initial_chunk ).
    ENDIF.
  ENDMETHOD.


  METHOD icon.

    DATA: lv_hint    TYPE string,
          lv_name    TYPE string,
          lv_color   TYPE string,
          lv_class   TYPE string,
          lv_onclick TYPE string.

    SPLIT iv_name AT '/' INTO lv_name lv_color.

    IF iv_hint IS NOT INITIAL.
      lv_hint  = | title="{ iv_hint }"|.
    ENDIF.
    IF iv_onclick IS NOT INITIAL.
      lv_onclick = | onclick="{ iv_onclick }"|.
    ENDIF.
    IF iv_class IS NOT INITIAL.
      lv_class = | { iv_class }|.
    ENDIF.
    IF lv_color IS NOT INITIAL.
      lv_color = | { lv_color }|.
    ENDIF.

    rv_str = |<i class="icon icon-{ lv_name }{ lv_color }|.
    rv_str = |{ rv_str }{ lv_class }"{ lv_onclick }{ lv_hint }></i>|.

  ENDMETHOD.


  METHOD indent_line.

    DATA: ls_study  TYPE ty_study_result,
          lv_spaces TYPE i.

    ls_study = study_line(
      is_context = cs_context
      iv_line    = cv_line ).

    " No indent for textarea tags
    IF ls_study-textarea_open = abap_true.
      cs_context-within_textarea = abap_true.
      RETURN.
    ELSEIF ls_study-textarea_close = abap_true.
      cs_context-within_textarea = abap_false.
      RETURN.
    ELSEIF cs_context-within_textarea = abap_true.
      RETURN.
    ENDIF.

    " No indent for pre tags
    IF ls_study-pre_open = abap_true.
      cs_context-within_pre = abap_true.
      RETURN.
    ELSEIF ls_study-pre_close = abap_true.
      cs_context-within_pre = abap_false.
      RETURN.
    ELSEIF cs_context-within_pre = abap_true.
      RETURN.
    ENDIF.

    " First closing tag - shift back exceptionally
    IF ( ls_study-script_close = abap_true
        OR ls_study-style_close = abap_true
        OR ls_study-curly_close = abap_true
        OR ls_study-tag_close = abap_true )
        AND cs_context-indent > 0.
      lv_spaces = ( cs_context-indent - 1 ) * c_indent_size.
      IF lv_spaces <= c_max_indent.
        cv_line  = gv_spaces(lv_spaces) && cv_line.
      ELSE.
        cv_line = gv_spaces && cv_line.
      ENDIF.
    ELSE.
      cv_line = cs_context-indent_str && cv_line.
    ENDIF.

    " Context status update
    CASE abap_true.
      WHEN ls_study-script_open.
        cs_context-within_js    = abap_true.
        cs_context-within_style = abap_false.
      WHEN ls_study-style_open.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_true.
      WHEN ls_study-script_close OR ls_study-style_close.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_false.
        ls_study-closings       = ls_study-closings + 1.
    ENDCASE.

    " More-less logic chosen due to possible double tags in a line '<a><b>'
    IF ls_study-openings <> ls_study-closings.
      IF ls_study-openings > ls_study-closings.
        cs_context-indent = cs_context-indent + 1.
      ELSEIF cs_context-indent > 0. " AND ls_study-openings < ls_study-closings
        cs_context-indent = cs_context-indent - 1.
      ENDIF.
      lv_spaces = cs_context-indent * c_indent_size.
      IF lv_spaces <= c_max_indent.
        cs_context-indent_str = gv_spaces(lv_spaces).
      ELSE.
        cv_line = gv_spaces && cv_line.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD parse_data_attr.

    SPLIT iv_str AT '=' INTO rs_data_attr-name rs_data_attr-value.
    IF rs_data_attr-name IS INITIAL.
      CLEAR rs_data_attr.
    ENDIF.

  ENDMETHOD.


  METHOD set_debug_mode.
    gv_debug_mode = iv_mode.
  ENDMETHOD.


  METHOD study_line.

    DATA: lv_line TYPE string,
          lv_len  TYPE i.

    lv_line = to_upper( shift_left( val = iv_line
                                    sub = ` ` ) ).
    lv_len  = strlen( lv_line ).

    " Some assumptions for simplification and speed
    " - style & scripts tag should be opened/closed in a separate line
    " - style & scripts opening and closing in one line is possible but only once

    " TODO & Issues
    " - What if the string IS a well formed html already not just single line ?

    IF is_context-within_js = abap_true OR is_context-within_style = abap_true.

      IF is_context-within_js = abap_true AND lv_len >= 8 AND lv_line(8) = '</SCRIPT'.
        rs_result-script_close = abap_true.
      ELSEIF is_context-within_style = abap_true AND lv_len >= 7 AND lv_line(7) = '</STYLE'.
        rs_result-style_close = abap_true.
      ENDIF.

      IF is_context-no_indent_jscss = abap_false.
        IF lv_len >= 1 AND lv_line(1) = '}'.
          rs_result-curly_close = abap_true.
        ENDIF.

        FIND ALL OCCURRENCES OF '{' IN lv_line MATCH COUNT rs_result-openings.
        FIND ALL OCCURRENCES OF '}' IN lv_line MATCH COUNT rs_result-closings.
      ENDIF.

    ELSE.
      IF lv_len >= 7 AND lv_line(7) = '<SCRIPT'.
        FIND FIRST OCCURRENCE OF '</SCRIPT' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-script_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 6 AND lv_line(6) = '<STYLE'.
        FIND FIRST OCCURRENCE OF '</STYLE' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-style_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 2 AND lv_line(2) = '</'.
        rs_result-tag_close = abap_true.
      ENDIF.

      FIND ALL OCCURRENCES OF '<'  IN lv_line MATCH COUNT rs_result-openings.
      FIND ALL OCCURRENCES OF '</' IN lv_line MATCH COUNT rs_result-closings.
      IF rs_result-closings <> rs_result-openings.
* if everything is closings, there are no single tags
        FIND ALL OCCURRENCES OF REGEX go_single_tags_re IN lv_line MATCH COUNT rs_result-singles.
      ENDIF.
      rs_result-openings = rs_result-openings - rs_result-closings - rs_result-singles.

    ENDIF.

    " Textarea (same assumptions as above)
    IF is_context-within_textarea = abap_true AND lv_len >= 10 AND lv_line(10) = '</TEXTAREA'.
      rs_result-textarea_close = abap_true.
    ELSEIF is_context-within_textarea = abap_false AND lv_len >= 9 AND lv_line(9) = '<TEXTAREA'.
      FIND FIRST OCCURRENCE OF '</TEXTAREA' IN lv_line.
      IF sy-subrc > 0. " Not found
        rs_result-textarea_open = abap_true.
      ENDIF.
    ENDIF.

    " Pre (same assumptions as above)
    IF is_context-within_pre = abap_true AND lv_len >= 5 AND lv_line(5) = '</PRE'.
      rs_result-pre_close = abap_true.
    ELSEIF is_context-within_pre = abap_false AND lv_len >= 4 AND lv_line(4) = '<PRE'.
      FIND FIRST OCCURRENCE OF '</PRE' IN lv_line.
      IF sy-subrc > 0. " Not found
        rs_result-pre_open = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
