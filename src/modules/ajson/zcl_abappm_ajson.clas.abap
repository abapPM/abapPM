class ZCL_ABAPPM_AJSON definition
  public
  create public .

  public section.

    interfaces ZIF_ABAPPM_AJSON .

    aliases:
      is_empty for ZIF_ABAPPM_AJSON~IS_EMPTY,
      exists for ZIF_ABAPPM_AJSON~EXISTS,
      members for ZIF_ABAPPM_AJSON~MEMBERS,
      get for ZIF_ABAPPM_AJSON~GET,
      get_boolean for ZIF_ABAPPM_AJSON~GET_BOOLEAN,
      get_integer for ZIF_ABAPPM_AJSON~GET_INTEGER,
      get_number for ZIF_ABAPPM_AJSON~GET_NUMBER,
      get_date for ZIF_ABAPPM_AJSON~GET_DATE,
      get_timestamp for ZIF_ABAPPM_AJSON~GET_TIMESTAMP,
      get_string for ZIF_ABAPPM_AJSON~GET_STRING,
      slice for ZIF_ABAPPM_AJSON~SLICE,
      to_abap for ZIF_ABAPPM_AJSON~TO_ABAP,
      array_to_string_table for ZIF_ABAPPM_AJSON~ARRAY_TO_STRING_TABLE.

    aliases:
      clear for ZIF_ABAPPM_AJSON~CLEAR,
      set for ZIF_ABAPPM_AJSON~SET,
      setx for ZIF_ABAPPM_AJSON~SETX,
      set_boolean for ZIF_ABAPPM_AJSON~SET_BOOLEAN,
      set_string for ZIF_ABAPPM_AJSON~SET_STRING,
      set_integer for ZIF_ABAPPM_AJSON~SET_INTEGER,
      set_date for ZIF_ABAPPM_AJSON~SET_DATE,
      set_timestamp for ZIF_ABAPPM_AJSON~SET_TIMESTAMP,
      set_null for ZIF_ABAPPM_AJSON~SET_NULL,
      delete for ZIF_ABAPPM_AJSON~DELETE,
      touch_array for ZIF_ABAPPM_AJSON~TOUCH_ARRAY,
      push for ZIF_ABAPPM_AJSON~PUSH,
      stringify for ZIF_ABAPPM_AJSON~STRINGIFY.

    aliases:
      clone for ZIF_ABAPPM_AJSON~CLONE,
      filter for ZIF_ABAPPM_AJSON~FILTER,
      map for ZIF_ABAPPM_AJSON~MAP.

    aliases:
      mt_json_tree for ZIF_ABAPPM_AJSON~MT_JSON_TREE,
      keep_item_order for ZIF_ABAPPM_AJSON~KEEP_ITEM_ORDER,
      format_datetime for ZIF_ABAPPM_AJSON~FORMAT_DATETIME,
      to_abap_corresponding_only for ZIF_ABAPPM_AJSON~TO_ABAP_CORRESPONDING_ONLY,
      freeze for ZIF_ABAPPM_AJSON~FREEZE.

    class-methods parse
      importing
        !iv_json            type string
        !iv_freeze          type abap_bool default abap_false
        !ii_custom_mapping  type ref to ZIF_ABAPPM_AJSON_MAPPING optional
        !iv_keep_item_order type abap_bool default abap_false
      returning
        value(ro_instance) type ref to ZCL_ABAPPM_AJSON
      raising
        ZCX_ABAPPM_AJSON_ERROR .

    class-methods create_empty " Might be deprecated, prefer using new( ) or create object
      importing
        !ii_custom_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING optional
        iv_keep_item_order type abap_bool default abap_false
        iv_format_datetime type abap_bool default abap_true
        iv_to_abap_corresponding_only type abap_bool default abap_false
      returning
        value(ro_instance) type ref to ZCL_ABAPPM_AJSON.

    " Experimental ! May change
    class-methods create_from " TODO, rename to 'from' ?
      importing
        !ii_source_json type ref to ZIF_ABAPPM_AJSON
        !ii_filter type ref to ZIF_ABAPPM_AJSON_FILTER optional " Might be deprecated, use filter() instead
        !ii_mapper type ref to ZIF_ABAPPM_AJSON_MAPPING optional " Might be deprecated, use map() instead
      returning
        value(ro_instance) type ref to ZCL_ABAPPM_AJSON
      raising
        ZCX_ABAPPM_AJSON_ERROR .

    methods constructor
      importing
        iv_keep_item_order type abap_bool default abap_false
        iv_format_datetime type abap_bool default abap_true
        iv_to_abap_corresponding_only type abap_bool default abap_false.
    class-methods new
      importing
        iv_keep_item_order type abap_bool default abap_false
        iv_format_datetime type abap_bool default abap_true
        iv_to_abap_corresponding_only type abap_bool default abap_false
      returning
        value(ro_instance) type ref to ZCL_ABAPPM_AJSON.

  protected section.

  private section.

    class-data go_float_regex type ref to cl_abap_regex.

    data ms_opts type ZIF_ABAPPM_AJSON=>TY_OPTS.
    data mi_custom_mapping type ref to ZIF_ABAPPM_AJSON_MAPPING. " DEPRECATED, will be removed

    methods get_item
      importing
        iv_path        type string
      returning
        value(rv_item) type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    methods prove_path_exists
      importing
        iv_path              type string
      returning
        value(rr_end_node) type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE
      raising
        ZCX_ABAPPM_AJSON_ERROR.
    methods delete_subtree
      importing
        iv_path           type string
        iv_name           type string
        ir_parent         type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE optional
      returning
        value(rs_top_node) type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    methods read_only_watchdog
      raising
        ZCX_ABAPPM_AJSON_ERROR.
ENDCLASS.



CLASS ZCL_ABAPPM_AJSON IMPLEMENTATION.


  method constructor.
    ms_opts-keep_item_order = iv_keep_item_order.
    ms_opts-to_abap_corresponding_only = iv_to_abap_corresponding_only.
    format_datetime( iv_format_datetime ).
  endmethod.


  method create_empty.
    create object ro_instance
      exporting
        iv_to_abap_corresponding_only = iv_to_abap_corresponding_only
        iv_format_datetime = iv_format_datetime
        iv_keep_item_order = iv_keep_item_order.
    ro_instance->mi_custom_mapping = ii_custom_mapping.
  endmethod.


  method create_from.

    data lo_mutator_queue type ref to lcl_mutator_queue.

    if ii_source_json is not bound.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( 'Source not bound' ).
    endif.

    create object ro_instance
      exporting
        iv_to_abap_corresponding_only = ii_source_json->opts( )-to_abap_corresponding_only
        iv_format_datetime = ii_source_json->opts( )-format_datetime
        iv_keep_item_order = ii_source_json->opts( )-keep_item_order.

    if ii_filter is not bound and ii_mapper is not bound.
      ro_instance->mt_json_tree = ii_source_json->mt_json_tree.
    else.
      create object lo_mutator_queue.
      if ii_mapper is bound.
        " Mapping goes first. But maybe it should be a freely definable queue of processors ?
        lo_mutator_queue->add( lcl_mapper_runner=>new( ii_mapper ) ).
      endif.
      if ii_filter is bound.
        lo_mutator_queue->add( lcl_filter_runner=>new( ii_filter ) ).
      endif.
      lo_mutator_queue->lif_mutator_runner~run(
        exporting
          it_source_tree = ii_source_json->mt_json_tree
        importing
          et_dest_tree = ro_instance->mt_json_tree ).
    endif.

  endmethod.


  method delete_subtree.

    data lv_parent_path type string.
    data lr_parent like ir_parent.

    read table mt_json_tree into rs_top_node
      with key
        path = iv_path
        name = iv_name.
    if sy-subrc <> 0.
      return. " Not found ? nothing to delete !
    endif.

    delete mt_json_tree index sy-tabix. " where path = iv_path and name = iv_name.

    if rs_top_node-children > 0. " only for objects and arrays
      lv_parent_path = iv_path && iv_name && '/*'.
      delete mt_json_tree where path cp lv_parent_path.
    endif.

    " decrement parent children
    if ir_parent is supplied.
      ir_parent->children = ir_parent->children - 1.
    else.
      lr_parent = get_item( iv_path ).
      if lr_parent is not initial.
        lr_parent->children = lr_parent->children - 1.
      endif.
    endif.

  endmethod.


  method get_item.

    field-symbols <item> like line of mt_json_tree.
    data ls_path_name type ZIF_ABAPPM_AJSON_TYPES=>TY_PATH_NAME.
    ls_path_name = lcl_utils=>split_path( iv_path ).

    read table mt_json_tree
      assigning <item>
      with key
        path = ls_path_name-path
        name = ls_path_name-name.
    if sy-subrc = 0.
      get reference of <item> into rv_item.
    endif.

  endmethod.


  method new.
    create object ro_instance
      exporting
        iv_to_abap_corresponding_only = iv_to_abap_corresponding_only
        iv_format_datetime = iv_format_datetime
        iv_keep_item_order = iv_keep_item_order.
  endmethod.


  method parse.

    data lo_parser type ref to lcl_json_parser.

    create object ro_instance.
    create object lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse(
      iv_json            = iv_json
      iv_keep_item_order = iv_keep_item_order ).
    ro_instance->mi_custom_mapping = ii_custom_mapping.
    ro_instance->ms_opts-keep_item_order = iv_keep_item_order.

    if iv_freeze = abap_true.
      ro_instance->freeze( ).
    endif.

  endmethod.


  method prove_path_exists.

    data lt_path type string_table.
    data lr_node_parent like rr_end_node.
    data lv_cur_path type string.
    data lv_cur_name type string.
    data ls_new_node like line of mt_json_tree.

    split iv_path at '/' into table lt_path.
    delete lt_path where table_line is initial.

    do.
      lr_node_parent = rr_end_node.
      read table mt_json_tree reference into rr_end_node
        with key
          path = lv_cur_path
          name = lv_cur_name.
      if sy-subrc <> 0. " New node, assume it is always object as it has a named child, use touch_array to init array
        clear ls_new_node.
        if lr_node_parent is not initial. " if has parent
          lr_node_parent->children = lr_node_parent->children + 1.
          if lr_node_parent->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-ARRAY.
            ls_new_node-index = lcl_utils=>validate_array_index(
              iv_path  = lv_cur_path
              iv_index = lv_cur_name ).
          endif.
        endif.
        ls_new_node-path = lv_cur_path.
        ls_new_node-name = lv_cur_name.
        ls_new_node-type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-OBJECT.
        insert ls_new_node into table mt_json_tree reference into rr_end_node.
      endif.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      read table lt_path index sy-index into lv_cur_name.
      if sy-subrc <> 0.
        exit. " no more segments
      endif.
    enddo.

  endmethod.


  method read_only_watchdog.
    if ms_opts-read_only = abap_true.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( 'This json instance is read only' ).
    endif.
  endmethod.


  method ZIF_ABAPPM_AJSON~ARRAY_TO_STRING_TABLE.

    data lv_normalized_path type string.
    data lr_node type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    if lr_node is initial.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( |Path not found: { iv_path }| ).
    endif.
    if lr_node->type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-ARRAY.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( |Array expected at: { iv_path }| ).
    endif.

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      case <item>-type.
        when ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NUMBER or ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-STRING.
          append <item>-value to rt_string_table.
        when ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NULL.
          append '' to rt_string_table.
        when ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-BOOLEAN.
          data lv_tmp type string.
          if <item>-value = 'true'.
            lv_tmp = abap_true.
          else.
            clear lv_tmp.
          endif.
          append lv_tmp to rt_string_table.
        when others.
          ZCX_ABAPPM_AJSON_ERROR=>RAISE( |Cannot convert [{ <item>-type
            }] to string at [{ <item>-path }{ <item>-name }]| ).
      endcase.
    endloop.

  endmethod.


  method ZIF_ABAPPM_AJSON~CLEAR.

    read_only_watchdog( ).
    clear mt_json_tree.

  endmethod.


  method ZIF_ABAPPM_AJSON~CLONE.
    ri_json = create_from( me ).
  endmethod.


  method ZIF_ABAPPM_AJSON~DELETE.

    read_only_watchdog( ).

    data ls_split_path type ZIF_ABAPPM_AJSON_TYPES=>TY_PATH_NAME.
    ls_split_path = lcl_utils=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    ri_json = me.

  endmethod.


  method ZIF_ABAPPM_AJSON~EXISTS.
    rv_exists = boolc( get_item( iv_path ) is not initial ).
  endmethod.


  method ZIF_ABAPPM_AJSON~FILTER.
    ri_json = create_from(
      ii_source_json = me
      ii_filter      = ii_filter ).
  endmethod.


  method ZIF_ABAPPM_AJSON~FORMAT_DATETIME.
    ms_opts-format_datetime = iv_use_iso.
    ri_json = me.
  endmethod.


  method ZIF_ABAPPM_AJSON~FREEZE.
    ms_opts-read_only = abap_true.
  endmethod.


  method ZIF_ABAPPM_AJSON~GET.

    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    lr_item = get_item( iv_path ).
    if lr_item is not initial.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~GET_BOOLEAN.

    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    lr_item = get_item( iv_path ).
    if lr_item is initial or lr_item->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NULL.
      return.
    elseif lr_item->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-BOOLEAN.
      rv_value = boolc( lr_item->value = 'true' ).
    elseif lr_item->value is not initial.
      rv_value = abap_true.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~GET_DATE.

    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    data lv_y type c length 4.
    data lv_m type c length 2.
    data lv_d type c length 2.

    lr_item = get_item( iv_path ).

    if lr_item is not initial and lr_item->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-STRING.
      find first occurrence of regex '^(\d{4})-(\d{2})-(\d{2})(T|$)' "#EC NOTEXT
        in lr_item->value
        submatches lv_y lv_m lv_d.
      concatenate lv_y lv_m lv_d into rv_value.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~GET_INTEGER.

    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    lr_item = get_item( iv_path ).
    if lr_item is not initial and lr_item->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NUMBER.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~GET_NODE_TYPE.

    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    lr_item = get_item( iv_path ).
    if lr_item is not initial.
      rv_node_type = lr_item->type.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~GET_NUMBER.

    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    lr_item = get_item( iv_path ).
    if lr_item is not initial and lr_item->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NUMBER.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~GET_STRING.

    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    lr_item = get_item( iv_path ).
    if lr_item is not initial and lr_item->type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NULL.
      rv_value = lr_item->value.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~GET_TIMESTAMP.

    data lo_to_abap type ref to lcl_json_to_abap.
    data lr_item type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.

    lr_item = get_item( iv_path ).

    if lr_item is initial.
      return.
    endif.

    create object lo_to_abap.

    try.
      rv_value = lo_to_abap->to_timestamp( lr_item->value ).
    catch ZCX_ABAPPM_AJSON_ERROR.
      return.
    endtry.

  endmethod.


  method ZIF_ABAPPM_AJSON~IS_EMPTY.
    rv_yes = boolc( lines( mt_json_tree ) = 0 ).
  endmethod.


  method ZIF_ABAPPM_AJSON~KEEP_ITEM_ORDER.
    ms_opts-keep_item_order = abap_true.
    ri_json = me.
  endmethod.


  method ZIF_ABAPPM_AJSON~MAP.
    ri_json = create_from(
      ii_source_json = me
      ii_mapper      = ii_mapper ).
  endmethod.


  method ZIF_ABAPPM_AJSON~MEMBERS.

    data lv_normalized_path type string.
    field-symbols <item> like line of mt_json_tree.

    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).

    loop at mt_json_tree assigning <item> where path = lv_normalized_path.
      append <item>-name to rt_members.
    endloop.

  endmethod.


  method ZIF_ABAPPM_AJSON~OPTS.
    rs_opts = ms_opts.
  endmethod.


  method ZIF_ABAPPM_AJSON~PUSH.

    data lr_parent type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    data lr_new_node type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.

    read_only_watchdog( ).

    lr_parent = get_item( iv_path ).

    if lr_parent is initial.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( |Path [{ iv_path }] does not exist| ).
    endif.

    if lr_parent->type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-ARRAY.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( |Path [{ iv_path }] is not array| ).
    endif.

    data lt_new_nodes type ZIF_ABAPPM_AJSON_TYPES=>TY_NODES_TT.
    data ls_new_path type ZIF_ABAPPM_AJSON_TYPES=>TY_PATH_NAME.
    data lv_new_index type i.

    lv_new_index     = lr_parent->children + 1.
    ls_new_path-path = lcl_utils=>normalize_path( iv_path ).
    ls_new_path-name = |{ lv_new_index }|.

    lt_new_nodes = lcl_abap_to_json=>convert(
      is_opts            = ms_opts
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    read table lt_new_nodes index 1 reference into lr_new_node. " assume first record is the array item - not ideal !
    assert sy-subrc = 0.
    lr_new_node->index = lv_new_index.

    " update data
    lr_parent->children = lv_new_index.
    insert lines of lt_new_nodes into table mt_json_tree.

    ri_json = me.

  endmethod.


  method ZIF_ABAPPM_AJSON~SET.

    data ls_split_path type ZIF_ABAPPM_AJSON_TYPES=>TY_PATH_NAME.
    data lr_parent type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    data ls_deleted_node type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    data lv_item_order type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE-ORDER.

    read_only_watchdog( ).

    ri_json = me.

    if iv_val is initial and iv_ignore_empty = abap_true and iv_node_type is initial.
      return. " nothing to assign
    endif.

    if iv_node_type is not initial
      and iv_node_type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-BOOLEAN and iv_node_type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NULL
      and iv_node_type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-NUMBER and iv_node_type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-STRING.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( |Unexpected type { iv_node_type }| ).
    endif.

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      if iv_node_type is not initial.
        mt_json_tree = lcl_abap_to_json=>insert_with_type(
          is_opts            = ms_opts
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      else.
        mt_json_tree = lcl_abap_to_json=>convert(
          is_opts            = ms_opts
          iv_data            = iv_val
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      endif.
      return.
    endif.

    " Ensure whole path exists
    lr_parent = prove_path_exists( ls_split_path-path ).
    assert lr_parent is not initial.

    " delete if exists with subtree
    ls_deleted_node = delete_subtree(
      ir_parent = lr_parent
      iv_path   = ls_split_path-path
      iv_name   = ls_split_path-name ).
    lv_item_order = ls_deleted_node-order.

    " convert to json
    data lt_new_nodes type ZIF_ABAPPM_AJSON_TYPES=>TY_NODES_TT.
    data lv_array_index type i.

    if lr_parent->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-ARRAY.
      lv_array_index = lcl_utils=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    elseif lr_parent->type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-OBJECT
      and lv_item_order = 0 and ms_opts-keep_item_order = abap_true.
      lv_item_order = lr_parent->children + 1.
    endif.

    if iv_node_type is not initial.
      lt_new_nodes = lcl_abap_to_json=>insert_with_type(
        is_opts            = ms_opts
        iv_item_order      = lv_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    else.
      lt_new_nodes = lcl_abap_to_json=>convert(
        is_opts            = ms_opts
        iv_item_order      = lv_item_order
        iv_data            = iv_val
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    endif.

    " update nodes
    if lines( lt_new_nodes ) > 0.
      lr_parent->children = lr_parent->children + 1.
      insert lines of lt_new_nodes into table mt_json_tree.
    endif.

  endmethod.


  method ZIF_ABAPPM_AJSON~SETX.

    data lv_path type string.
    data lv_val type string.
    data lv_int type i.
    data lv_dec type decfloat34.
    data lv_last type i.

    if iv_param is initial.
      ri_json = me.
      return.
    endif.

    split iv_param at ':' into lv_path lv_val.
    condense lv_path.
    condense lv_val.

    if lv_val is initial.
      ri_json = me.
      return. " Hmm ? or empty string ? or null ?
    endif.

    if go_float_regex is not bound.
      create object go_float_regex exporting pattern = '^([1-9][0-9]*|0)\.[0-9]+$'.
      " expects fractional, because ints are detected separately
    endif.

    if lv_val = 'null'.
      ZIF_ABAPPM_AJSON~SET_NULL( lv_path ).
    elseif lv_val = 'true'.
      ZIF_ABAPPM_AJSON~SET_BOOLEAN(
        iv_path = lv_path
        iv_val  = abap_true ).
    elseif lv_val = 'false'.
      ZIF_ABAPPM_AJSON~SET_BOOLEAN(
        iv_path = lv_path
        iv_val  = abap_false ).
    elseif lv_val co '0123456789'.
      lv_int = lv_val.
      ZIF_ABAPPM_AJSON~SET_INTEGER(
        iv_path = lv_path
        iv_val  = lv_int ).
    elseif lv_val co '0123456789.' and go_float_regex->create_matcher( text = lv_val )->match( ) = abap_true.
      lv_dec = lv_val.
      ZIF_ABAPPM_AJSON~SET(
        iv_path = lv_path
        iv_val  = lv_dec ).
    elseif lv_val+0(1) = '{' or lv_val+0(1) = '['.
      "Expect object/array, but no further checks, parser will catch errors
      ZIF_ABAPPM_AJSON~SET(
        iv_path = lv_path
        iv_val  = parse(
          iv_json = lv_val
          iv_keep_item_order = ms_opts-keep_item_order ) ).
    else. " string
      lv_last = strlen( lv_val ) - 1.
      if lv_val+0(1) = '"' and lv_val+lv_last(1) = '"'.
        lv_val = substring(
          val = lv_val
          off = 1
          len = lv_last - 1 ).
      endif.
      ZIF_ABAPPM_AJSON~SET_STRING(
        iv_path = lv_path
        iv_val  = lv_val ).
    endif.

    ri_json = me.

  endmethod.


  method ZIF_ABAPPM_AJSON~SET_BOOLEAN.

    ri_json = me.

    data lv_bool type abap_bool.
    lv_bool = boolc( iv_val is not initial ).
    ZIF_ABAPPM_AJSON~SET(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_bool ).

  endmethod.


  method ZIF_ABAPPM_AJSON~SET_DATE.

    ri_json = me.

    data lv_val type string.
    lv_val = lcl_abap_to_json=>format_date( iv_val ).

    ZIF_ABAPPM_AJSON~SET(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method ZIF_ABAPPM_AJSON~SET_INTEGER.

    ri_json = me.

    ZIF_ABAPPM_AJSON~SET(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  endmethod.


  method ZIF_ABAPPM_AJSON~SET_NULL.

    ri_json = me.

    data lv_null_ref type ref to data.
    ZIF_ABAPPM_AJSON~SET(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  endmethod.


  method ZIF_ABAPPM_AJSON~SET_STRING.

    ri_json = me.

    data lv_val type string.
    lv_val = iv_val.
    ZIF_ABAPPM_AJSON~SET(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  endmethod.


  method ZIF_ABAPPM_AJSON~SET_TIMESTAMP.

    ri_json = me.

    data lv_timestamp_iso type string.
    lv_timestamp_iso = lcl_abap_to_json=>format_timestamp( iv_val ).

    ZIF_ABAPPM_AJSON~SET(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_timestamp_iso ).

  endmethod.


  method ZIF_ABAPPM_AJSON~SLICE.

    data lo_section         type ref to ZCL_ABAPPM_AJSON.
    data ls_item            like line of mt_json_tree.
    data lv_normalized_path type string.
    data ls_path_parts      type ZIF_ABAPPM_AJSON_TYPES=>TY_PATH_NAME.
    data lv_path_len        type i.
    data lv_path_pattern    type string.

    create object lo_section.
    lv_normalized_path = lcl_utils=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = lcl_utils=>split_path( lv_normalized_path ).

    read table mt_json_tree into ls_item
      with key path = ls_path_parts-path name = ls_path_parts-name.
    if sy-subrc <> 0.
      return.
    endif.

    clear: ls_item-path, ls_item-name, ls_item-order. " this becomes a new root
    insert ls_item into table lo_section->mt_json_tree.

    lv_path_pattern = lv_normalized_path && `*`.

    loop at mt_json_tree into ls_item where path cp lv_path_pattern.

      ls_item-path = substring( val = ls_item-path off = lv_path_len - 1 ). " less closing '/'
      insert ls_item into table lo_section->mt_json_tree.

    endloop.

    ri_json = lo_section.

  endmethod.


  method ZIF_ABAPPM_AJSON~STRINGIFY.

    rv_json = lcl_json_serializer=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = ms_opts-keep_item_order
      iv_indent          = iv_indent ).

  endmethod.


  method ZIF_ABAPPM_AJSON~TOUCH_ARRAY.

    data lr_node type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    data ls_deleted_node type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
    data ls_new_node like line of mt_json_tree.
    data ls_split_path type ZIF_ABAPPM_AJSON_TYPES=>TY_PATH_NAME.

    read_only_watchdog( ).

    ls_split_path = lcl_utils=>split_path( iv_path ).
    if ls_split_path is initial. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-ARRAY.
      insert ls_new_node into table mt_json_tree.
      return.
    endif.

    if iv_clear = abap_true.
      ls_deleted_node = delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    else.
      lr_node = get_item( iv_path ).
    endif.

    if lr_node is initial. " Or node was cleared

      data lr_parent type ref to ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.
      lr_parent = prove_path_exists( ls_split_path-path ).
      assert lr_parent is not initial.

      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-ARRAY.

      if ms_opts-keep_item_order = abap_true.
        if ls_deleted_node is not initial.
          ls_new_node-order = ls_deleted_node-order.
        else.
          ls_new_node-order = lr_parent->children.
        endif.
      endif.

      insert ls_new_node into table mt_json_tree.

    elseif lr_node->type <> ZIF_ABAPPM_AJSON_TYPES=>NODE_TYPE-ARRAY.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( |Path [{ iv_path }] already used and is not array| ).
    endif.

    ri_json = me.

  endmethod.


  method ZIF_ABAPPM_AJSON~TO_ABAP.

    data lo_to_abap type ref to lcl_json_to_abap.

    clear ev_container.
    create object lo_to_abap
      exporting
        iv_corresponding  = boolc( iv_corresponding = abap_true or ms_opts-to_abap_corresponding_only = abap_true )
        ii_custom_mapping = mi_custom_mapping.

    lo_to_abap->to_abap(
      exporting
        it_nodes    = ZIF_ABAPPM_AJSON~MT_JSON_TREE
      changing
        c_container = ev_container ).

  endmethod.


  method ZIF_ABAPPM_AJSON~TO_ABAP_CORRESPONDING_ONLY.
    ms_opts-to_abap_corresponding_only = iv_enable.
    ri_json = me.
  endmethod.
ENDCLASS.
