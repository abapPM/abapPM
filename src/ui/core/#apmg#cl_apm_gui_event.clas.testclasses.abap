CLASS ltcl_event DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS query_wrong_data FOR TESTING RAISING /apmg/cx_apm_error.
    METHODS form_wrong_data FOR TESTING RAISING /apmg/cx_apm_error.
    METHODS query FOR TESTING RAISING /apmg/cx_apm_error.
    METHODS form_data FOR TESTING RAISING /apmg/cx_apm_error.
    METHODS immutability FOR TESTING RAISING /apmg/cx_apm_error.

ENDCLASS.

CLASS ltcl_event IMPLEMENTATION.

  METHOD query_wrong_data.

    DATA li_cut TYPE REF TO /apmg/if_apm_gui_event.
    DATA lo_map TYPE REF TO /apmg/cl_apm_string_map.

    CREATE OBJECT li_cut TYPE /apmg/cl_apm_gui_event
      EXPORTING
        iv_action  = 'XXX'
        iv_getdata = 'not_a_param'.

    lo_map = li_cut->query( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD form_wrong_data.

    DATA li_cut TYPE REF TO /apmg/if_apm_gui_event.
    DATA lo_map TYPE REF TO /apmg/cl_apm_string_map.

    CREATE OBJECT li_cut TYPE /apmg/cl_apm_gui_event
      EXPORTING
        iv_action = 'XXX'.

    lo_map = li_cut->form_data( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD query.

    DATA li_cut TYPE REF TO /apmg/if_apm_gui_event.
    DATA lo_map TYPE REF TO /apmg/cl_apm_string_map.

    CREATE OBJECT li_cut TYPE /apmg/cl_apm_gui_event
      EXPORTING
        iv_action  = 'XXX'
        iv_getdata = 'a=b&b=c'.

    " Cross check just in case
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->form_data( )->size( )
      exp = 0 ).

    lo_map = li_cut->query( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'a' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'b' )
      exp = 'c' ).

    " Case insensitivity
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'A' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'B' )
      exp = 'c' ).

  ENDMETHOD.

  METHOD form_data.

    DATA li_cut TYPE REF TO /apmg/if_apm_gui_event.
    DATA lo_map TYPE REF TO /apmg/cl_apm_string_map.
    DATA lt_postdata TYPE /apmg/if_apm_html_viewer=>ty_post_data.

    APPEND 'a=b&b=c' TO lt_postdata.
    CREATE OBJECT li_cut TYPE /apmg/cl_apm_gui_event
      EXPORTING
        iv_action   = 'XXX'
        it_postdata = lt_postdata.

    " Cross check just in case
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->query( )->size( )
      exp = 0 ).

    lo_map = li_cut->form_data( ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'a' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'b' )
      exp = 'c' ).

    " Case insensitivity
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->size( )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'A' )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_map->get( 'B' )
      exp = 'c' ).

  ENDMETHOD.

  METHOD immutability.

    DATA li_cut TYPE REF TO /apmg/if_apm_gui_event.
    DATA lo_x TYPE REF TO cx_root.

    CREATE OBJECT li_cut TYPE /apmg/cl_apm_gui_event
      EXPORTING
        iv_getdata = 'a=b&b=c'
        iv_action  = 'XXX'.

    TRY.
        li_cut->form_data( )->set(
          iv_key = 'x'
          iv_val = 'y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lo_x.
        cl_abap_unit_assert=>assert_char_cp(
          act = lo_x->get_text( )
          exp = 'String map is read only' ).
    ENDTRY.

    TRY.
        li_cut->query( )->set(
          iv_key = 'x'
          iv_val = 'y' ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root INTO lo_x.
        cl_abap_unit_assert=>assert_char_cp(
          act = lo_x->get_text( )
          exp = 'String map is read only' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_html_action_utils DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.
    METHODS parse_fields_simple_case FOR TESTING.
    METHODS parse_fields_advanced_case FOR TESTING.
    METHODS parse_fields_unescape FOR TESTING.
    METHODS parse_fields_unescape_nbsp FOR TESTING.
    METHODS parse_fields_german_umlauts FOR TESTING.
    METHODS parse_fields_wrong_format FOR TESTING.
    METHODS parse_post_form_data FOR TESTING.
    METHODS parse_fields_webgui FOR TESTING.
    METHODS parse_fields_special_chars FOR TESTING.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF c_german_umlaut_as_hex,
                 lower_case_ae TYPE xstring VALUE 'C3A4',
                 lower_case_oe TYPE xstring VALUE 'C3B6',
                 lower_case_ue TYPE xstring VALUE 'C3BC',
               END OF c_german_umlaut_as_hex.

    CLASS-DATA: BEGIN OF gs_german_umlaut_as_char,
                  lower_case_ae TYPE string,
                  lower_case_oe TYPE string,
                  lower_case_ue TYPE string,
                END OF gs_german_umlaut_as_char.

    DATA mo_cut TYPE REF TO /apmg/cl_apm_gui_event.
    DATA mv_given_parse_string TYPE string.
    DATA mt_parsed_fields TYPE tihttpnvp.

    METHODS setup.
    METHODS _given_string_is
      IMPORTING
        iv_string TYPE string.
    METHODS _when_fields_are_parsed_upper.
    METHODS _when_fields_are_parsed.
    METHODS _then_fields_should_be
      IMPORTING
        iv_index TYPE i
        iv_name  TYPE string
        iv_value TYPE string.
    METHODS _then_field_count_should_be
      IMPORTING
        iv_count TYPE i.

    CLASS-METHODS _hex_to_char
      IMPORTING
        iv_x        TYPE xstring
      RETURNING
        VALUE(rv_s) TYPE string.

ENDCLASS.

CLASS /apmg/cl_apm_gui_event DEFINITION LOCAL FRIENDS ltcl_html_action_utils.

CLASS ltcl_html_action_utils IMPLEMENTATION.

  METHOD class_constructor.

    gs_german_umlaut_as_char-lower_case_ae = _hex_to_char( c_german_umlaut_as_hex-lower_case_ae ).
    gs_german_umlaut_as_char-lower_case_oe = _hex_to_char( c_german_umlaut_as_hex-lower_case_oe ).
    gs_german_umlaut_as_char-lower_case_ue = _hex_to_char( c_german_umlaut_as_hex-lower_case_ue ).

  ENDMETHOD.

  METHOD setup.

    mo_cut = /apmg/cl_apm_gui_event=>new( 'test_action' ).

  ENDMETHOD.


  METHOD parse_fields_simple_case.

    _given_string_is( `committer_name=Gustav Gans` ).

    _when_fields_are_parsed_upper( ).

    _then_fields_should_be( iv_index = 1
                            iv_name = `COMMITTER_NAME`
                            iv_value = `Gustav Gans` ).

  ENDMETHOD.

  METHOD parse_fields_advanced_case.

    _given_string_is( `committer_name=Albert Schweitzer&`
                   && `committer_email=albert.schweitzer@googlemail.com&`
                   && `comment=dummy comment&`
                   && `body=Message body<<new>><<new>>with line break<<new>>&`
                   && `author_name=Karl Klammer&`
                   && `author_email=karl@klammer.com` ).

    _when_fields_are_parsed_upper( ).

    _then_fields_should_be( iv_index = 1
                            iv_name  = `COMMITTER_NAME`
                            iv_value = `Albert Schweitzer` ).

    _then_fields_should_be( iv_index = 2
                            iv_name  = `COMMITTER_EMAIL`
                            iv_value = `albert.schweitzer@googlemail.com` ).

    _then_fields_should_be( iv_index = 3
                            iv_name  = `COMMENT`
                            iv_value = `dummy comment` ).

    _then_fields_should_be( iv_index = 4
                            iv_name  = `BODY`
                            iv_value = `Message body<<new>><<new>>with line break<<new>>` ).

    _then_fields_should_be( iv_index = 5
                            iv_name  = `AUTHOR_NAME`
                            iv_value = `Karl Klammer` ).

    _then_fields_should_be( iv_index = 6
                            iv_name  = `AUTHOR_EMAIL`
                            iv_value = `karl@klammer.com` ).

  ENDMETHOD.

  METHOD parse_fields_unescape.

    " file status = '?', used in staging page
    _given_string_is( '/SRC/ZFOOBAR.PROG.ABAP=%3F' ).

    _when_fields_are_parsed_upper( ).
    _then_field_count_should_be( 1 ).

    _then_fields_should_be(
      iv_index = 1
      iv_name  = '/SRC/ZFOOBAR.PROG.ABAP'
      iv_value = '?' ).

  ENDMETHOD.

  METHOD parse_fields_unescape_nbsp.

    " non-breaking space (&nbsp;)
    _given_string_is( '/src/ztest_rfc.fugr.xml=%3F&/src/ztest_rfc'
                   && /apmg/cl_apm_gui_event=>gv_non_breaking_space
                   && /apmg/cl_apm_gui_event=>gv_non_breaking_space
                   && /apmg/cl_apm_gui_event=>gv_non_breaking_space
                   && 'rf.sush.xml=A' ).

    _when_fields_are_parsed( ).
    _then_field_count_should_be( 2 ).

    _then_fields_should_be(
      iv_index = 1
      iv_name  = '/src/ztest_rfc.fugr.xml'
      iv_value = '?' ).

    _then_fields_should_be(
      iv_index = 2
      iv_name  = '/src/ztest_rfc   rf.sush.xml'
      iv_value = 'A' ).

  ENDMETHOD.

  METHOD parse_fields_german_umlauts.

    DATA: lv_ae       TYPE string,
          lv_oe       TYPE string,
          lv_ue       TYPE string,
          lv_ae_oe_ue TYPE string.


    lv_ae = gs_german_umlaut_as_char-lower_case_ae.
    lv_oe = gs_german_umlaut_as_char-lower_case_oe.
    lv_ue = gs_german_umlaut_as_char-lower_case_ue.

    lv_ae_oe_ue = lv_ae && lv_oe && lv_ue.

    _given_string_is( |committer_name=Christian G{ lv_ue }nter&|
                   && |committer_email=guenne@googlemail.com&|
                   && |comment={ lv_ae_oe_ue }&|
                   && |body=Message body<<new>><<new>>with line break<<new>>and umlauts. { lv_ae_oe_ue }&|
                   && |author_name=Gerd Schr{ lv_oe }der&|
                   && |author_email=gerd@schroeder.com| ).

    _when_fields_are_parsed_upper( ).

    _then_fields_should_be( iv_index = 1
                            iv_name  = `COMMITTER_NAME`
                            iv_value = |Christian G{ lv_ue }nter| ).

    _then_fields_should_be( iv_index = 2
                            iv_name  = `COMMITTER_EMAIL`
                            iv_value = `guenne@googlemail.com` ).

    _then_fields_should_be( iv_index = 3
                            iv_name  = `COMMENT`
                            iv_value = lv_ae_oe_ue ).

    _then_fields_should_be( iv_index = 4
                            iv_name  = `BODY`
                            iv_value = |Message body<<new>><<new>>with line break<<new>>and umlauts. { lv_ae_oe_ue }| ).

    _then_fields_should_be( iv_index = 5
                            iv_name  = `AUTHOR_NAME`
                            iv_value = |Gerd Schr{ lv_oe }der| ).

    _then_fields_should_be( iv_index = 6
                            iv_name  = `AUTHOR_EMAIL`
                            iv_value = `gerd@schroeder.com` ).

  ENDMETHOD.

  METHOD _given_string_is.

    mv_given_parse_string = iv_string.

  ENDMETHOD.

  METHOD _when_fields_are_parsed_upper.

    mt_parsed_fields = mo_cut->parse_fields_upper_case_name( mv_given_parse_string ).

  ENDMETHOD.

  METHOD _when_fields_are_parsed.

    mt_parsed_fields = mo_cut->parse_fields( mv_given_parse_string ).

  ENDMETHOD.

  METHOD _then_fields_should_be.

    FIELD-SYMBOLS: <ls_parsed_field> LIKE LINE OF mt_parsed_fields.

    READ TABLE mt_parsed_fields ASSIGNING <ls_parsed_field> INDEX iv_index.

    cl_abap_unit_assert=>assert_subrc(
      exp = 0
      msg = |No parsed field found at index { iv_index }| ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_parsed_field>-name
      exp = iv_name
      msg = |Name at index { iv_index } should be { iv_name }| ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_parsed_field>-value
      exp = iv_value
      msg = |Value at index { iv_index } should be { iv_value }| ).

  ENDMETHOD.

  METHOD _then_field_count_should_be.

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_parsed_fields )
      exp = iv_count
      msg = |Field count { lines( mt_parsed_fields ) } should be { iv_count }| ).

  ENDMETHOD.

  METHOD _hex_to_char.

    DATA lo_conv TYPE REF TO cl_abap_conv_in_ce.

    lo_conv = cl_abap_conv_in_ce=>create( ).
    lo_conv->convert( EXPORTING input = iv_x IMPORTING data = rv_s ).

  ENDMETHOD.

  METHOD parse_fields_wrong_format.

    _given_string_is( `some_query_string_without_param_structure` ).
    _when_fields_are_parsed_upper( ).
    _then_field_count_should_be( 0 ).

    _given_string_is( `some_query_string_without_param_structure&a=b` ).
    _when_fields_are_parsed_upper( ).
    _then_field_count_should_be( 1 ).
    _then_fields_should_be(
      iv_index = 1
      iv_name  = 'A'
      iv_value = 'b' ).

  ENDMETHOD.

  METHOD parse_post_form_data.

    DATA lt_post_data TYPE /apmg/if_apm_html_viewer=>ty_post_data.
    DATA lv_line LIKE LINE OF lt_post_data.
    DATA lv_long_name LIKE LINE OF lt_post_data.
    DATA lv_size TYPE i.

    DESCRIBE FIELD lv_line LENGTH lv_size IN CHARACTER MODE.
    lv_long_name = repeat(
      val = 'x'
      occ = lv_size - 4 ).
    lv_line = 'a=b&' && lv_long_name.

    APPEND lv_line TO lt_post_data.
    APPEND '=y' TO lt_post_data.

    mt_parsed_fields = mo_cut->parse_post_form_data( lt_post_data ).
    _then_field_count_should_be( 2 ).
    _then_fields_should_be(
      iv_index = 1
      iv_name  = 'a'
      iv_value = 'b' ).
    _then_fields_should_be(
      iv_index = 2
      iv_name  = |{ lv_long_name }|
      iv_value = 'y' ).

    mt_parsed_fields = mo_cut->parse_post_form_data(
      it_post_data = lt_post_data
      iv_upper_cased = abap_true ).
    _then_field_count_should_be( 2 ).
    _then_fields_should_be(
      iv_index = 1
      iv_name  = 'A'
      iv_value = 'b' ).
    _then_fields_should_be(
      iv_index = 2
      iv_name  = |{ to_upper( lv_long_name ) }|
      iv_value = 'y' ).

  ENDMETHOD.


  METHOD parse_fields_webgui.

    _given_string_is( `KEY=000000000019&PATH=%2fsrc%2f&FILENAME=%2fnsp%2ftest_ddls_bug2.ddls.asddls` ).
    _when_fields_are_parsed( ).
    _then_field_count_should_be( 3 ).

    _then_fields_should_be(
      iv_index = 1
      iv_name  = 'KEY'
      iv_value = '000000000019' ).

    _then_fields_should_be(
      iv_index = 2
      iv_name  = 'PATH'
      iv_value = '/src/' ).

    _then_fields_should_be(
      iv_index = 3
      iv_name  = 'FILENAME'
      iv_value = '/nsp/test_ddls_bug2.ddls.asddls' ).

  ENDMETHOD.

  METHOD parse_fields_special_chars.

    DATA lv_string TYPE string.

    " URL encoded data
    lv_string = `TEST=!"#$%25%26'()*+,-./09:;<%3d>?@AZ[\]^_``az{|}~¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿`.

    _given_string_is( lv_string ).
    _when_fields_are_parsed( ).
    _then_field_count_should_be( 1 ).

    _then_fields_should_be(
      iv_index = 1
      iv_name  = 'TEST'
      iv_value = `!"#$%&'()*+,-./09:;<=>?@AZ[\]^_``az{|}~¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿` ).

  ENDMETHOD.

ENDCLASS.
