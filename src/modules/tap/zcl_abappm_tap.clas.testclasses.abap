CLASS ltcl_tap DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    CONSTANTS snap_id TYPE string VALUE 'SNAPSHOT_TEST'.

    CONSTANTS:
      BEGIN OF const,
        i    TYPE i VALUE 123,
        i8   TYPE int8 VALUE 1234567890123456,
        c    TYPE c LENGTH 3 VALUE 'abc',
        n    TYPE n LENGTH 5 VALUE '00014',
        str  TYPE string VALUE 'tap tap tap',
        d    TYPE d VALUE '20221126',
        t    TYPE t VALUE '123456',
        x    TYPE x LENGTH 4 VALUE '4D617263',
        xstr TYPE xstring VALUE '4265726E627264',
      END OF const.

    DATA cut TYPE REF TO zcl_abappm_tap.

    DATA:
      i          TYPE i,
      i8         TYPE int8,
      c          TYPE c LENGTH 3,
      n          TYPE n LENGTH 5,
      str        TYPE string,
      d          TYPE d,
      t          TYPE t,
      x          TYPE x LENGTH 4,
      xstr       TYPE xstring,
      dref       TYPE REF TO data,
      oref       TYPE REF TO object,
      tab        TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      tab_sorted TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY,
      tab_hashed TYPE HASHED TABLE OF string WITH UNIQUE DEFAULT KEY,
      BEGIN OF str_flat,
        f1 TYPE c,
        f2 TYPE i,
      END OF str_flat,
      BEGIN OF str_deep,
        f1 TYPE string,
        f2 LIKE STANDARD TABLE OF str_flat,
      END OF str_deep.

    METHODS:
      setup,
      actual FOR TESTING,
      equals FOR TESTING,
      differs FOR TESTING,
      index FOR TESTING,
      snap FOR TESTING.

ENDCLASS.

CLASS ltcl_tap IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( logging  = abap_false snapshot = abap_false ).
  ENDMETHOD.

  METHOD actual.

    FIELD-SYMBOLS <any_tab> TYPE ANY TABLE.

    " Literals
    cl_abap_unit_assert=>assert_bound( cut->_( 123 ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( 'abc' ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( `Test` ) ).

    " Constants
    cl_abap_unit_assert=>assert_bound( cut->_( abap_true ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( const-i ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( const-str ) ).

    " Variables
    cl_abap_unit_assert=>assert_bound( cut->_( i ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( str ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( xstr ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( str_flat ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( tab ) ).

    " Field Symbols
    ASSIGN i TO FIELD-SYMBOL(<any>).
    cl_abap_unit_assert=>assert_bound( cut->_( <any> ) ).
    ASSIGN tab TO <any_tab>.
    cl_abap_unit_assert=>assert_bound( cut->_( <any_tab> ) ).

  ENDMETHOD.

  METHOD equals.

    FIELD-SYMBOLS <any_tab> TYPE ANY TABLE.

    " Literals
    cl_abap_unit_assert=>assert_bound( cut->_( 123 )->equals( 123 ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( 'abc' )->equals( 'abc' ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( `Test` )->equals( `Test` ) ).

    " Constants
    cl_abap_unit_assert=>assert_bound( cut->_( abap_true )->equals( abap_true ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( const-i )->equals( const-i ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( const-str )->equals( const-str ) ).

    " Variables
    i = 456.
    str = 'tappy'.
    xstr = const-xstr.
    str_flat-f1 = 'Z'.
    str_flat-f2 = 789.
    INSERT str INTO TABLE tab.

    cl_abap_unit_assert=>assert_bound( cut->_( i )->equals( 456 ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( str )->equals( 'tappy' ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( xstr )->equals( xstr ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( str_flat )->equals( str_flat ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( tab )->equals( tab ) ).

    " Field Symbols
    ASSIGN i TO FIELD-SYMBOL(<any>).
    cl_abap_unit_assert=>assert_bound( cut->_( <any> )->equals( i ) ).
    ASSIGN tab TO <any_tab>.
    cl_abap_unit_assert=>assert_bound( cut->_( <any_tab> )->equals( tab ) ).

  ENDMETHOD.

  METHOD differs.

    FIELD-SYMBOLS <any_tab> TYPE ANY TABLE.

    " Literals
    cl_abap_unit_assert=>assert_bound( cut->_( 123 )->differs( 12 ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( 'abc' )->differs( 'ab' ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( `Test` )->differs( `Test ` ) ).

    " Constants
    cl_abap_unit_assert=>assert_bound( cut->_( abap_true )->differs( '' ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( const-i )->differs( 0 ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( const-str )->differs( 'Test' ) ).

    " Variables
    i = 456.
    str = 'tappy'.
    xstr = const-xstr.
    str_flat-f1 = 'Z'.
    str_flat-f2 = 789.
    INSERT str INTO TABLE tab.

    cl_abap_unit_assert=>assert_bound( cut->_( i )->differs( 26 ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( str )->differs( 'tap' ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( xstr )->differs( xstr && const-x ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( str_flat )->differs( 'Z789' ) ).
    cl_abap_unit_assert=>assert_bound( cut->_( tab )->differs( tab_hashed ) ).

    " Field Symbols
    ASSIGN i TO FIELD-SYMBOL(<any>).
    cl_abap_unit_assert=>assert_bound( cut->_( <any> )->differs( 11 ) ).
    ASSIGN tab TO <any_tab>.
    cl_abap_unit_assert=>assert_bound( cut->_( <any_tab> )->differs( tab ) ).

  ENDMETHOD.

  METHOD index.

    INSERT `1` INTO TABLE tab.
    INSERT `2` INTO TABLE tab.
    INSERT `3` INTO TABLE tab.

    LOOP AT tab INTO str.
      cl_abap_unit_assert=>assert_bound( cut->_( str )->tabix( sy-tabix ) ).
    ENDLOOP.

    READ TABLE tab INTO str INDEX 2.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_bound( cut->_( str )->eq( '2' )->tabix( 2 ) ).

  ENDMETHOD.


  METHOD snap.

    cut->snap_begin( snap_id ).
    DO 10 TIMES.
      cut->snap_write( |{ sy-index }| ).
    ENDDO.
    cut->snap_end( snap_id ).

  ENDMETHOD.

ENDCLASS.
