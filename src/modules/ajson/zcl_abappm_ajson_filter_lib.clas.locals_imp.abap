**********************************************************************
*  FILTER EMPTY VALUES
**********************************************************************

class lcl_empty_filter definition final.
  public section.
    interfaces ZIF_ABAPPM_AJSON_FILTER.
endclass.

class lcl_empty_filter implementation.
  method ZIF_ABAPPM_AJSON_FILTER~KEEP_NODE.

    rv_keep = boolc(
      ( iv_visit = ZIF_ABAPPM_AJSON_FILTER=>VISIT_TYPE-VALUE and is_node-value is not initial ) or
      ( iv_visit <> ZIF_ABAPPM_AJSON_FILTER=>VISIT_TYPE-VALUE and is_node-children > 0 ) ).
    " children = 0 on open for initially empty nodes and on close for filtered ones

  endmethod.
endclass.

**********************************************************************
*  FILTER PREDEFINED PATHS
**********************************************************************

class lcl_paths_filter definition final.
  public section.
    interfaces ZIF_ABAPPM_AJSON_FILTER.
    methods constructor
      importing
        it_skip_paths type string_table optional
        iv_skip_paths type string optional
        iv_pattern_search type abap_bool
      raising
        ZCX_ABAPPM_AJSON_ERROR.
  private section.
    data mt_skip_paths type hashed table of string with unique key table_line.
    data mv_pattern_search type abap_bool.
endclass.

class lcl_paths_filter implementation.

  method ZIF_ABAPPM_AJSON_FILTER~KEEP_NODE.

    data lv_full_path type string.
    field-symbols <p> like line of mt_skip_paths.

    lv_full_path = is_node-path && is_node-name.

    if mv_pattern_search = abap_true.
      rv_keep = abap_true.
      loop at mt_skip_paths assigning <p>.
        if lv_full_path cp <p>.
          rv_keep = abap_false.
          exit.
        endif.
      endloop.
    else.
      read table mt_skip_paths with key table_line = lv_full_path transporting no fields.
      rv_keep = boolc( sy-subrc <> 0 ).
    endif.

  endmethod.

  method constructor.

    data lv_s type string.
    data lt_tab type string_table.
    field-symbols <s> type string.

    if boolc( iv_skip_paths is initial ) = boolc( it_skip_paths is initial ). " XOR
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( 'no filter path specified' ).
    endif.

    loop at it_skip_paths into lv_s.
      lv_s = to_lower( lv_s ).
      append lv_s to lt_tab.
    endloop.

    if iv_skip_paths is not initial.
      split iv_skip_paths at ',' into table lt_tab.
      loop at lt_tab assigning <s>.
        if <s> is initial.
          delete lt_tab index sy-tabix.
          continue.
        endif.
        <s> = condense( to_lower( <s> ) ).
      endloop.
    endif.

    sort lt_tab by table_line.
    delete adjacent duplicates from lt_tab.

    mt_skip_paths = lt_tab.
    mv_pattern_search = iv_pattern_search.

  endmethod.

endclass.

**********************************************************************
* MULTI FILTER
**********************************************************************

class lcl_and_filter definition final.
  public section.
    interfaces ZIF_ABAPPM_AJSON_FILTER.
    methods constructor
      importing
        it_filters type ZIF_ABAPPM_AJSON_FILTER=>TY_FILTER_TAB
      raising
        ZCX_ABAPPM_AJSON_ERROR.
  private section.
    data mt_filters type ZIF_ABAPPM_AJSON_FILTER=>TY_FILTER_TAB.
endclass.

class lcl_and_filter implementation.

  method ZIF_ABAPPM_AJSON_FILTER~KEEP_NODE.

    data li_filter like line of mt_filters.

    rv_keep = abap_true.
    loop at mt_filters into li_filter.
      rv_keep = li_filter->keep_node(
        is_node  = is_node
        iv_visit = iv_visit ).
      if rv_keep = abap_false.
        return.
      endif.
    endloop.

  endmethod.

  method constructor.

    data li_filter like line of it_filters.

    loop at it_filters into li_filter where table_line is bound.
      append li_filter to mt_filters.
    endloop.

  endmethod.

endclass.
