class ltcl_error definition
  for testing
  risk level harmless
  duration short
  final.

  private section.

    methods raise for testing.
    methods raise_w_location for testing.
    methods raise_w_node for testing.
    methods set_location for testing.

endclass.

class ltcl_error implementation.

  method raise.

    data lx type ref to ZCX_ABAPPM_AJSON_ERROR.
    data lv_msg type string.

    lv_msg = repeat( val = 'a' occ = 50 ) && repeat( val = 'b' occ = 50 ) && '123'.

    try.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( lv_msg ).
      cl_abap_unit_assert=>fail( ).
    catch ZCX_ABAPPM_AJSON_ERROR into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = lv_msg
        act = lx->get_text( ) ).
    endtry.

  endmethod.

  method raise_w_location.

    data lx type ref to ZCX_ABAPPM_AJSON_ERROR.

    try.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( iv_msg = 'a' iv_location = 'b' ).
      cl_abap_unit_assert=>fail( ).
    catch ZCX_ABAPPM_AJSON_ERROR into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'a @b'
        act = lx->get_text( ) ).
    endtry.

  endmethod.

  method raise_w_node.

    data lx type ref to ZCX_ABAPPM_AJSON_ERROR.
    data ls_node type ZIF_ABAPPM_AJSON_TYPES=>TY_NODE.

    ls_node-path = '/x/'.
    ls_node-name = 'y'.

    try.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( iv_msg = 'a' is_node = ls_node ).
      cl_abap_unit_assert=>fail( ).
    catch ZCX_ABAPPM_AJSON_ERROR into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'a @/x/y'
        act = lx->get_text( ) ).
    endtry.

  endmethod.

  method set_location.

    data lx type ref to ZCX_ABAPPM_AJSON_ERROR.

    try.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( iv_msg = 'a' iv_location = 'b' ).
      cl_abap_unit_assert=>fail( ).
    catch ZCX_ABAPPM_AJSON_ERROR into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = lx->location
        act = 'b' ).
      lx->set_location( 'c' ).
      cl_abap_unit_assert=>assert_equals(
        exp = lx->location
        act = 'c' ).
      cl_abap_unit_assert=>assert_equals(
        exp = 'a @c'
        act = lx->get_text( ) ).
    endtry.

    try.
      ZCX_ABAPPM_AJSON_ERROR=>RAISE( iv_msg = 'a' ).
      cl_abap_unit_assert=>fail( ).
    catch ZCX_ABAPPM_AJSON_ERROR into lx.
      cl_abap_unit_assert=>assert_equals(
        exp = lx->location
        act = '' ).
      lx->set_location( 'c' ).
      cl_abap_unit_assert=>assert_equals(
        exp = lx->location
        act = 'c' ).
      cl_abap_unit_assert=>assert_equals(
        exp = 'a @c'
        act = lx->get_text( ) ).
    endtry.

  endmethod.

endclass.
