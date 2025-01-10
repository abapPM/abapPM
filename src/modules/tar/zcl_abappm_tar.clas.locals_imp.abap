CLASS lcl_pax DEFINITION.

* Pax format stores keyword lists in ustar blocks
  " https://pubs.opengroup.org/onlinepubs/009695399/utilities/pax.html
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

CLASS zcl_abappm_tar DEFINITION LOCAL FRIENDS lcl_pax.

CLASS lcl_pax IMPLEMENTATION.

  METHOD decode_keywords.

    DATA pax_records TYPE string_table.

    DATA(pax_data) = cl_binary_convert=>xstring_utf8_to_string( block ).

    SPLIT pax_data AT cl_abap_char_utilities=>newline INTO TABLE pax_records.

    LOOP AT pax_records ASSIGNING FIELD-SYMBOL(<record>).
      SPLIT <record> AT ` ` INTO DATA(octal_len) DATA(key_val).
      DATA(len) = zcl_abappm_tar=>_from_octal( octal_len ) - 1.
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
      DATA(octal_len) = zcl_abappm_tar=>_to_octal( len ).
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

CLASS zcl_abappm_tar DEFINITION LOCAL FRIENDS lcl_7zip.

CLASS lcl_7zip IMPLEMENTATION.

  METHOD decode_longlink.

    CONSTANTS c_longlink TYPE string VALUE `././@LongLink`.

    DATA header TYPE zcl_abappm_tar=>ty_header.

    header = zcl_abappm_tar=>_from_xstring( block_1 ).

    IF header-name = c_longlink.
      result = zcl_abappm_tar=>_from_xstring( block_2 ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
