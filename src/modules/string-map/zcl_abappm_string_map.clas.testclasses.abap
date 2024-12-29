CLASS ltcl_string_map DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_struc,
        a TYPE string,
        b TYPE abap_bool,
        c TYPE i,
      END OF ty_struc.

    METHODS get_set_has FOR TESTING.
    METHODS size_empty_clear FOR TESTING.
    METHODS delete FOR TESTING.
    METHODS keys_values FOR TESTING.
    METHODS case_insensitive FOR TESTING.
    METHODS set_clike FOR TESTING.
    METHODS setx FOR TESTING.

    METHODS strict FOR TESTING.
    METHODS freeze FOR TESTING.

    METHODS from_struc FOR TESTING.
    METHODS from_to_struc_negative FOR TESTING.
    METHODS from_entries FOR TESTING.
    METHODS from_string FOR TESTING.
    METHODS from_map FOR TESTING.
    METHODS merge FOR TESTING.

    METHODS to_struc FOR TESTING.
    METHODS to_string FOR TESTING.
    METHODS to_entries FOR TESTING.

    METHODS create_from FOR TESTING.
    METHODS case_insensitive_create FOR TESTING.
    METHODS list_mode FOR TESTING.

ENDCLASS.

CLASS ltcl_string_map IMPLEMENTATION.

  METHOD create_from.

    DATA lx_e TYPE REF TO cx_root.
    DATA lo_src TYPE REF TO zcl_abappm_string_map.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    DATA: BEGIN OF ls_dummy, a TYPE string VALUE '1', END OF ls_dummy.

    lo_src = zcl_abappm_string_map=>create( ).
    lo_src->set(
      iv_key = 'A'
      iv_val = '1' ).

    TRY.
        zcl_abappm_string_map=>create( iv_from = 12345 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Incorrect input for string_map=>create, typekind I'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        zcl_abappm_string_map=>create( iv_from = me ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Incorrect string map instance to copy from'
          act = lx_e->get_text( ) ).
    ENDTRY.

    " From obj
    lo_cut = zcl_abappm_string_map=>create( iv_from = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '1'
      act = lo_cut->get( 'A' ) ).

    " From tab
    lo_cut = zcl_abappm_string_map=>create( iv_from = lo_src->mt_entries ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '1'
      act = lo_cut->get( 'A' ) ).

    " From struc
    lo_cut = zcl_abappm_string_map=>create( iv_from = ls_dummy ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '1'
      act = lo_cut->get( 'A' ) ).

    " From string
    lo_cut = zcl_abappm_string_map=>create( iv_from = 'x=y' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'x' )
      exp = 'y' ).

    " From another map
    lo_cut = zcl_abappm_string_map=>create( iv_from = zcl_abappm_string_map=>create( iv_from = 'x=1' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'x' )
      exp = '1' ).

  ENDMETHOD.

  METHOD freeze.

    DATA lt_entries TYPE zcl_abappm_string_map=>tty_entries.
    DATA ls_dummy TYPE syst.
    DATA lx_e TYPE REF TO cx_root.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    FIELD-SYMBOLS <l> LIKE LINE OF lt_entries.

    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' )->freeze( ).

    TRY.
        lo_cut->set(
          iv_key = 'A'
          iv_val = '2' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->set(
          iv_key = 'B'
          iv_val = '2' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->delete( 'A' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->clear( ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.

        APPEND INITIAL LINE TO lt_entries ASSIGNING <l>.
        <l>-k = 'a'.
        lo_cut->from_entries( lt_entries ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->from_struc( ls_dummy ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->from_string( 'x=y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->from_map( zcl_abappm_string_map=>create( iv_from = 'x=y' ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'String map is read only'
          act = lx_e->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_set_has.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = ''
      act = lo_cut->get( 'C' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'C' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->has( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'a' ) ). " case sensitive

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'newvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'newvalue'
      act = lo_cut->get( 'A' ) ).

  ENDMETHOD.

  METHOD size_empty_clear.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->is_empty( ) ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).

    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'newvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->is_empty( ) ).

    lo_cut->clear( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = lo_cut->is_empty( ) ).

  ENDMETHOD.

  METHOD delete.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    lo_cut->delete( iv_key = 'A' ).

    cl_abap_unit_assert=>assert_equals(
      exp = ''
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = lo_cut->has( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

  ENDMETHOD.

  METHOD keys_values.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    DATA lt_exp TYPE string_table.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'B'
      iv_val = 'bvalue' ).

    CLEAR lt_exp.
    APPEND 'A' TO lt_exp.
    APPEND 'B' TO lt_exp.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp
      act = lo_cut->keys( ) ).

    CLEAR lt_exp.
    APPEND 'avalue' TO lt_exp.
    APPEND 'bvalue' TO lt_exp.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp
      act = lo_cut->values( ) ).

  ENDMETHOD.

  METHOD to_struc.

    DATA ls_struc_act TYPE ty_struc.
    DATA ls_struc_exp TYPE ty_struc.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'a'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'X' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '123' ).

    lo_cut->to_struc( CHANGING cs_container = ls_struc_act ).

    ls_struc_exp-a = 'avalue'.
    ls_struc_exp-b = abap_true.
    ls_struc_exp-c = 123.

    cl_abap_unit_assert=>assert_equals(
      exp = ls_struc_exp
      act = ls_struc_act ).

  ENDMETHOD.

  METHOD from_struc.

    DATA ls_struc TYPE ty_struc.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    ls_struc-a = 'avalue'.
    ls_struc-b = abap_true.
    ls_struc-c = 123.

    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    lo_cut = lo_cut->from_struc( ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      exp = 4
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'X'
      act = lo_cut->get( 'B' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '123'
      act = lo_cut->get( 'C' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'xyz'
      act = lo_cut->get( 'z' ) ).

  ENDMETHOD.

  METHOD strict.

    DATA ls_struc_act TYPE ty_struc.
    DATA ls_struc_exp TYPE ty_struc.
    DATA lx_e TYPE REF TO cx_root.
    DATA lo_struc TYPE REF TO zcl_abappm_string_map.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'a'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'X' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '123' ).
    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    ls_struc_exp-a = 'avalue'.
    ls_struc_exp-b = abap_true.
    ls_struc_exp-c = 123.

    TRY.
        lo_cut->to_struc( CHANGING cs_container = ls_struc_act ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Component Z not found in target'
          act = lx_e->get_text( ) ).
    ENDTRY.

    lo_struc = lo_cut->strict( abap_false ).
    lo_struc->to_struc( CHANGING cs_container = ls_struc_act ).

    cl_abap_unit_assert=>assert_equals(
      exp = ls_struc_exp
      act = ls_struc_act ).

  ENDMETHOD.

  METHOD from_to_struc_negative.

    DATA lt_dummy TYPE string_table.
    DATA lx_e TYPE REF TO cx_root.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    TRY.
        lo_cut->from_struc( lt_dummy ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Only structures supported'
          act = lx_e->get_text( ) ).
    ENDTRY.

    TRY.
        lo_cut->to_struc( CHANGING cs_container = lt_dummy ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lx_e.
        cl_abap_unit_assert=>assert_equals(
          exp = 'Only structures supported'
          act = lx_e->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD from_entries.

    TYPES:
      BEGIN OF ty_pair,
        key TYPE string,
        val TYPE string,
      END OF ty_pair.

    DATA lt_entries TYPE TABLE OF ty_pair.
    DATA ls_entry LIKE LINE OF lt_entries.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    ls_entry-key = 'A'.
    ls_entry-val = 'avalue'.
    APPEND ls_entry TO lt_entries.

    ls_entry-key = 'B'.
    ls_entry-val = '123'.
    APPEND ls_entry TO lt_entries.

    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    lo_cut = lo_cut->from_entries( lt_entries ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '123'
      act = lo_cut->get( 'B' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'xyz'
      act = lo_cut->get( 'z' ) ).

  ENDMETHOD.

  METHOD case_insensitive.

    DATA lt_exp_keys TYPE string_table.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( iv_case_insensitive = abap_true ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = 'bvalue' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'A' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'avalue'
      act = lo_cut->get( 'a' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'bvalue'
      act = lo_cut->get( 'b' ) ).

    cl_abap_unit_assert=>assert_true( lo_cut->has( 'A' ) ).
    cl_abap_unit_assert=>assert_true( lo_cut->has( 'a' ) ).
    cl_abap_unit_assert=>assert_true( lo_cut->has( 'B' ) ).
    cl_abap_unit_assert=>assert_true( lo_cut->has( 'b' ) ).
    cl_abap_unit_assert=>assert_false( lo_cut->has( 'c' ) ).

    APPEND 'A' TO lt_exp_keys.
    APPEND 'B' TO lt_exp_keys.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp_keys
      act = lo_cut->keys( ) ).

    lo_cut->delete( 'a' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).

    lo_cut->delete( 'B' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

  ENDMETHOD.

  METHOD case_insensitive_create.

    DATA lo_src TYPE REF TO zcl_abappm_string_map.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.

    lo_src = zcl_abappm_string_map=>create( ).
    lo_src->set(
      iv_key = 'A'
      iv_val = '1' ).
    lo_src->set(
      iv_key = 'b'
      iv_val = '2' ).
    lo_src->freeze( ).

    lo_cut = zcl_abappm_string_map=>create(
      iv_from             = lo_src
      iv_case_insensitive = abap_true ).

    DATA lt_exp_keys TYPE string_table.
    APPEND 'A' TO lt_exp_keys.
    APPEND 'B' TO lt_exp_keys.

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp_keys
      act = lo_cut->keys( ) ).

  ENDMETHOD.

  METHOD set_clike.

    DATA lv_char TYPE c LENGTH 10.
    DATA lv_numc TYPE n LENGTH 4.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'A'
      iv_val = 'avalue' ).
    lo_cut->set(
      iv_key = `B`
      iv_val = `bvalue` ).

    lv_char = 'C'.
    lo_cut->set(
      iv_key = lv_char
      iv_val = lv_char ).

    lv_numc = '123'.
    lo_cut->set(
      iv_key = lv_numc
      iv_val = lv_numc ).

    cl_abap_unit_assert=>assert_equals(
      exp = 4
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'C'
      act = lo_cut->get( 'C' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = '0123'
      act = lo_cut->get( '0123' ) ).

  ENDMETHOD.

  METHOD from_string.

    DATA lx_e TYPE REF TO lcx_error.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->set(
      iv_key = 'z'
      iv_val = 'xyz' ).

    lo_cut = lo_cut->from_string( 'a = avalue, b = some data, c = space   space' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = 'avalue' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = 'some data' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'c' )
      exp = 'space   space' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'z' )
      exp = 'xyz' ).

    TRY.
        lo_cut->from_string( `x=y,  ` ).
      CATCH lcx_error INTO lx_e.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_e->get_text( )
          exp = 'Empty key*' ).
    ENDTRY.

  ENDMETHOD.

  METHOD to_string.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->from_string( 'a = avalue, b = some data, c = space   space' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->to_string( )
      exp = 'a=avalue,b=some data,c=space   space' ).

  ENDMETHOD.

  METHOD from_map.

    DATA lo_src TYPE REF TO zcl_abappm_string_map.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.

    lo_src = zcl_abappm_string_map=>create( ).
    lo_src->set(
      iv_key = 'a'
      iv_val = '1' ).
    lo_src->set(
      iv_key = 'b'
      iv_val = '2' ).
    lo_src->freeze( ).

    " Empty map
    lo_cut = zcl_abappm_string_map=>create( ).
    lo_cut = lo_cut->from_map( lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = '2' ).

    " Existing values + overwrite
    lo_cut = zcl_abappm_string_map=>create( ).
    lo_cut->set(
      iv_key = 'a'
      iv_val = 'x' ).
    lo_cut->set(
      iv_key = 'c'
      iv_val = '3' ).

    lo_cut->from_map( lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = '2' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'c' )
      exp = '3' ).

  ENDMETHOD.

  METHOD merge.

    DATA lo_src TYPE REF TO zcl_abappm_string_map.
    DATA lo_cut TYPE REF TO zcl_abappm_string_map.

    lo_cut = zcl_abappm_string_map=>create( ).
    lo_cut->set(
      iv_key = 'a'
      iv_val = '1' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = '2' ).

    lo_src = zcl_abappm_string_map=>create( ).
    lo_src->set(
      iv_key = 'b'
      iv_val = '20' ).
    lo_src->set(
      iv_key = 'c'
      iv_val = '30' ).

    lo_cut->merge( lo_src ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = '20' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'c' )
      exp = '30' ).

    " Case 2
    lo_cut = zcl_abappm_string_map=>create( iv_case_insensitive = abap_true ).
    lo_cut->set(
      iv_key = 'a'
      iv_val = '1' ).
    lo_cut->set(
      iv_key = 'b'
      iv_val = '2' ).

    lo_src = zcl_abappm_string_map=>create( ).
    lo_src->set(
      iv_key = 'B'
      iv_val = '200' ).
    lo_src->set(
      iv_key = 'D'
      iv_val = '400' ).

    lo_cut->merge( lo_src ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->size( )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'a' )
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'b' )
      exp = '200' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( 'd' )
      exp = '400' ).

  ENDMETHOD.


  METHOD to_entries.

    TYPES:
      BEGIN OF lty_str,
        a TYPE string,
        b TYPE string,
      END OF lty_str,
      lty_str_t TYPE STANDARD TABLE OF lty_str,
      BEGIN OF lty_char,
        a TYPE c LENGTH 10,
        b TYPE c LENGTH 10,
      END OF lty_char,
      lty_char_t TYPE STANDARD TABLE OF lty_char,
      BEGIN OF lty_bad1,
        a TYPE c LENGTH 10,
      END OF lty_bad1,
      lty_bad1_t TYPE STANDARD TABLE OF lty_bad1,
      BEGIN OF lty_bad2,
        a TYPE i,
        b TYPE i,
      END OF lty_bad2,
      lty_bad2_t TYPE STANDARD TABLE OF lty_bad2.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.
    lo_cut = zcl_abappm_string_map=>create( `x=1,y=2` ).

    DATA lt_str_act TYPE lty_str_t.
    DATA lt_str_exp TYPE lty_str_t.
    DATA ls_str LIKE LINE OF lt_str_act.
    DATA lt_char_act TYPE lty_char_t.
    DATA lt_char_exp TYPE lty_char_t.
    DATA ls_char LIKE LINE OF lt_char_act.

    ls_str-a = 'x'.
    ls_str-b = '1'.
    APPEND ls_str TO lt_str_exp.
    ls_str-a = 'y'.
    ls_str-b = '2'.
    APPEND ls_str TO lt_str_exp.
    lo_cut->to_entries( CHANGING ct_entries = lt_str_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_str_act
      exp = lt_str_exp ).

    ls_char-a = 'x'.
    ls_char-b = '1'.
    APPEND ls_char TO lt_char_exp.
    ls_char-a = 'y'.
    ls_char-b = '2'.
    APPEND ls_char TO lt_char_exp.
    lo_cut->to_entries( CHANGING ct_entries = lt_char_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_char_act
      exp = lt_char_exp ).

    DATA lx TYPE REF TO lcx_error.
    DATA lt_bad1 TYPE lty_bad1_t.
    TRY.
        lo_cut->to_entries( CHANGING ct_entries = lt_bad1 ).
        cl_abap_unit_assert=>fail( ).
      CATCH lcx_error INTO lx.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx->get_text( )
          exp = '*number*' ).
    ENDTRY.

    DATA lt_bad2 TYPE lty_bad2_t.
    TRY.
        lo_cut->to_entries( CHANGING ct_entries = lt_bad2 ).
        cl_abap_unit_assert=>fail( ).
      CATCH lcx_error INTO lx.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx->get_text( )
          exp = '*type*' ).
    ENDTRY.

    DATA lt_bad3 TYPE string_table.
    TRY.
        lo_cut->to_entries( CHANGING ct_entries = lt_bad3 ).
        cl_abap_unit_assert=>fail( ).
      CATCH lcx_error INTO lx.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx->get_text( )
          exp = '*table line*' ).
    ENDTRY.

  ENDMETHOD.

  METHOD setx.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.

    lo_cut = zcl_abappm_string_map=>create( ).

    lo_cut->setx( 'a:1' ).
    lo_cut->setx( |b : 2| ).
    lo_cut->setx( ':c' ).
    lo_cut->setx( '' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_cut->get( 'a' )
      act = '1' ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_cut->get( 'b' )
      act = '2' ).

  ENDMETHOD.

  METHOD list_mode.

    DATA lo_cut TYPE REF TO zcl_abappm_string_map.

    lo_cut = zcl_abappm_string_map=>create( ).
    lo_cut->setx( 'a:1' ).
    lo_cut->setx( 'a:2' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lo_cut->size( ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_cut->get( 'a' )
      act = '2' ).

    lo_cut = zcl_abappm_string_map=>create( iv_list_mode = abap_true ).
    lo_cut->setx( 'a:1' ).
    lo_cut->setx( 'a:2' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lo_cut->size( ) ).

    DATA lt_entries TYPE zcl_abappm_string_map=>tty_entries.
    DATA ls_item LIKE LINE OF lt_entries.
    lt_entries = lo_cut->mt_entries.
    SORT lt_entries BY k v.

    READ TABLE lt_entries INDEX 1 INTO ls_item.
    cl_abap_unit_assert=>assert_equals(
      exp = '1'
      act = ls_item-v ).

    READ TABLE lt_entries INDEX 2 INTO ls_item.
    cl_abap_unit_assert=>assert_equals(
      exp = '2'
      act = ls_item-v ).

    lo_cut->delete( 'a' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lo_cut->size( ) ).

  ENDMETHOD.


ENDCLASS.
