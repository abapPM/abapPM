CLASS lcl_tar_helpers DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS from_octal
      IMPORTING
        !octal        TYPE string
      RETURNING
        VALUE(result) TYPE i.

    CLASS-METHODS to_octal
      IMPORTING
        !number       TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS from_xstring
      IMPORTING
        !data         TYPE xstring
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS to_xstring
      IMPORTING
        !data         TYPE simple
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

  PRIVATE SECTION.

    CLASS-DATA:
      convert_in  TYPE REF TO cl_abap_conv_in_ce,
      convert_out TYPE REF TO cl_abap_conv_out_ce.

ENDCLASS.

CLASS lcl_tar_helpers IMPLEMENTATION.

  METHOD from_octal.

    DATA(offset) = 0.

    DO strlen( octal ) TIMES.
      result = result * 8 + octal+offset(1).
      offset = offset + 1.
    ENDDO.

  ENDMETHOD.

  METHOD to_octal.

    DATA(temp_number) = CONV i( number ).

    WHILE temp_number > 0.
      result      = |{ temp_number MOD 8 }{ result }|.
      temp_number = temp_number DIV 8.
    ENDWHILE.

    IF result IS INITIAL.
      result = '0'.
    ENDIF.

  ENDMETHOD.

  METHOD from_xstring.

    IF convert_in IS INITIAL.
      convert_in = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    ENDIF.

    TRY.
        convert_in->convert(
          EXPORTING
            input = data
            n     = xstrlen( data )
          IMPORTING
            data  = result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting from xstring' ).
    ENDTRY.

  ENDMETHOD.

  METHOD to_xstring.

    IF convert_out IS INITIAL.
      convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    ENDIF.

    DATA(string_data) = CONV string( data ).

    TRY.
        convert_out->convert(
          EXPORTING
            data   = string_data
          IMPORTING
            buffer = result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting to xstring' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_pax DEFINITION.

* Pax format stores keyword lists in ustar blocks
* https://pubs.opengroup.org/onlinepubs/009695399/utilities/pax.html
*
* A keyword list consists of records constructed as follows:
* "%d %s=%s\n", <length>, <keyword>, <value>

  PUBLIC SECTION.

    CLASS-METHODS decode_keywords
      IMPORTING
        block         TYPE xstring
      RETURNING
        VALUE(result) TYPE zcl_abappm_tar=>ty_keywords.

    CLASS-METHODS encode_keywords
      IMPORTING
        keywords      TYPE zcl_abappm_tar=>ty_keywords
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS merge_keywords
      IMPORTING
        global        TYPE zcl_abappm_tar=>ty_keywords
        extended      TYPE zcl_abappm_tar=>ty_keywords
      RETURNING
        VALUE(result) TYPE zcl_abappm_tar=>ty_keywords.

ENDCLASS.

CLASS lcl_pax IMPLEMENTATION.

  METHOD decode_keywords.

    DATA pax_records TYPE string_table.

    DATA(pax_data) = cl_binary_convert=>xstring_utf8_to_string( block ).

    SPLIT pax_data AT cl_abap_char_utilities=>newline INTO TABLE pax_records.

    LOOP AT pax_records ASSIGNING FIELD-SYMBOL(<record>).
      SPLIT <record> AT ` ` INTO DATA(octal_len) DATA(key_val).
      DATA(len) = lcl_tar_helpers=>from_octal( octal_len ) - 1.
      IF strlen( key_val ) <> len.
        ASSERT 0 = 0. " ignore this inconsistency
      ENDIF.

      SPLIT key_val AT `=` INTO DATA(key) DATA(value).
      DATA(keyword) = VALUE zcl_abappm_tar=>ty_keyword(
        keyword = key
        value   = value ).
      INSERT keyword INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD encode_keywords.

    DATA pax_records TYPE string_table.
    DATA block TYPE x LENGTH zcl_abappm_tar=>c_blocksize.

    LOOP AT keywords ASSIGNING FIELD-SYMBOL(<keyword>).
      DATA(pax_record) = |{ <keyword>-keyword }={ <keyword>-value }|.
      DATA(len) = strlen( pax_record ) + 1. " +1 for newline
      DATA(octal_len) = lcl_tar_helpers=>to_octal( len ).
      pax_record = octal_len && pax_record.
      INSERT pax_record INTO TABLE pax_records.
    ENDLOOP.

    DATA(pax_data) = concat_lines_of(
      table = pax_records
      sep   = cl_abap_char_utilities=>newline ).

    pax_data = pax_data && cl_abap_char_utilities=>newline.

    result = cl_binary_convert=>string_to_xstring_utf8( pax_data ).

    len = zcl_abappm_tar=>c_blocksize - xstrlen( result ).

    IF len < 0.
      " TODO: What if the keywords don't fit into a blocK?
      ASSERT 1 = 2.
    ELSE.
      result = result && block(len).
    ENDIF.

  ENDMETHOD.

  METHOD merge_keywords.

    result = global.

    LOOP AT extended ASSIGNING FIELD-SYMBOL(<extended>).
      READ TABLE result ASSIGNING FIELD-SYMBOL(<result>)
        WITH TABLE KEY keyword = <extended>-keyword.
      IF sy-subrc = 0.
        <result>-value = <extended>-value.
      ELSE.
        INSERT <extended> INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_7zip DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS decode_longlink
      IMPORTING
        block_1       TYPE xstring
        block_2       TYPE xstring
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

ENDCLASS.

CLASS lcl_7zip IMPLEMENTATION.

  METHOD decode_longlink.

    CONSTANTS c_longlink TYPE string VALUE `././@LongLink`.

    DATA(header) = CONV zcl_abappm_tar=>ty_header( lcl_tar_helpers=>from_xstring( block_1 ) ).

    IF header-name = c_longlink.
      result = lcl_tar_helpers=>from_xstring( block_2 ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
