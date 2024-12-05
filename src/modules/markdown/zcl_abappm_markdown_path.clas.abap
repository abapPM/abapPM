CLASS zcl_abappm_markdown_path DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Markdown Path
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    METHODS normalize
      IMPORTING
        iv_path          TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_slash TYPE string VALUE '/'.
    CONSTANTS c_dot TYPE string VALUE '.'.

    METHODS posix_normalize
      IMPORTING
        iv_path             TYPE string
        iv_allow_above_root TYPE string
      RETURNING
        VALUE(rv_result)    TYPE string.

    METHODS char_at
      IMPORTING
        iv_val           TYPE string
        iv_off           TYPE i
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS slice
      IMPORTING
        iv_val           TYPE string
        iv_start         TYPE i
        iv_end           TYPE i
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS last_index_of
      IMPORTING
        iv_val           TYPE string
        iv_sub           TYPE string
      RETURNING
        VALUE(rv_result) TYPE i.
ENDCLASS.



CLASS zcl_abappm_markdown_path IMPLEMENTATION.


  METHOD char_at.
    rv_result = substring( val = iv_val off = iv_off len = 1 ).
  ENDMETHOD.


  METHOD last_index_of.
    rv_result = find( val = iv_val sub = iv_sub occ = -1 ).
  ENDMETHOD.


  METHOD normalize.

    DATA lv_path TYPE string.
    DATA lv_is_absolute TYPE abap_bool.
    DATA lv_trailing_separator TYPE abap_bool.

    lv_path = iv_path.
    IF lv_path IS INITIAL.
      rv_result = '.'.
      RETURN.
    ENDIF.

    lv_is_absolute        = boolc( substring( val = lv_path len = 1 ) = c_slash ).
    lv_trailing_separator = boolc( substring( val = reverse( lv_path ) len = 1 ) = c_slash ).

    lv_path = posix_normalize(
      iv_path             = lv_path
      iv_allow_above_root = boolc( lv_is_absolute = abap_false ) ).

    IF lv_path IS INITIAL AND lv_is_absolute = abap_false.
      lv_path = '.'.
    ENDIF.
    IF lv_path IS NOT INITIAL AND lv_trailing_separator = abap_true.
      lv_path = lv_path && '/'.
    ENDIF.
    IF lv_is_absolute = abap_true.
      lv_path = '/' && lv_path.
    ENDIF.

    rv_result = lv_path.

  ENDMETHOD.


  METHOD posix_normalize.

    DATA lv_out TYPE string.
    DATA lt_out TYPE string_table.
    DATA lv_res TYPE string.
    DATA lv_last_segment_length TYPE i.
    DATA lv_last_slash_index TYPE i.
    DATA lv_last_slash TYPE i VALUE -1.
    DATA lv_dots TYPE i.
    DATA lv_code TYPE c LENGTH 1.
    DATA lv_i TYPE i.

    DO strlen( iv_path ) + 1 TIMES.
      IF lv_i < strlen( iv_path ).
        lv_code = char_at( iv_val = iv_path iv_off = lv_i ).
      ELSEIF lv_code = c_slash.
        EXIT.
      ELSE.
        lv_code = c_slash.
      ENDIF.

      lv_out = |{ lv_i } { lv_res }|.
      INSERT lv_out INTO TABLE lt_out.
      IF lv_code = c_slash.
        IF lv_last_slash = lv_i - 1 OR lv_dots = 1.
          ASSERT 0 = 0. " NOP
        ELSEIF lv_last_slash <> lv_i - 1 AND lv_dots = 2.
          IF strlen( lv_res ) < 2 OR
            lv_last_segment_length <> 2 OR
            char_at( iv_val = lv_res iv_off = strlen( lv_res ) - 1 ) <> c_dot OR
            char_at( iv_val = lv_res iv_off = strlen( lv_res ) - 2 ) <> c_dot.
            IF strlen( lv_res ) > 2.
              lv_last_slash_index = last_index_of( iv_val = lv_res iv_sub = c_slash ).
              IF lv_last_slash_index <> strlen( lv_res ) - 1.
                lv_out = |{ lv_i } { lv_res } #1|.
                INSERT lv_out INTO TABLE lt_out.
                IF lv_last_slash_index = -1.
                  lv_res = ''.
                  lv_last_segment_length = 0.
                ELSE.
                  lv_res = slice( iv_val = lv_res iv_start = 0 iv_end = lv_last_slash_index ).
                  lv_last_segment_length = strlen( lv_res ) - 1 - last_index_of( iv_val = lv_res iv_sub = c_slash ).
                ENDIF.
                lv_last_slash = lv_i.
                lv_dots = 0.
                lv_i = lv_i + 1.
                CONTINUE.
              ENDIF.
            ELSEIF strlen( lv_res ) = 2 OR strlen( lv_res ) = 1.
              lv_res = ''.
              lv_last_segment_length = 0.
              lv_last_slash = lv_i.
              lv_dots = 0.
              lv_i = lv_i + 1.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF iv_allow_above_root = abap_true.
            lv_out = |{ lv_i } { lv_res } #3|.
            INSERT lv_out INTO TABLE lt_out.
            IF strlen( lv_res ) > 0.
              lv_res = lv_res && c_slash && c_dot && c_dot.
            ELSE.
              lv_res = c_dot && c_dot.
            ENDIF.
            lv_last_segment_length = 2.
          ENDIF.
        ELSE.
          lv_out = |{ lv_i } { lv_res } #2|.
          INSERT lv_out INTO TABLE lt_out.
          IF strlen( lv_res ) > 0.
            lv_res = lv_res && c_slash && slice( iv_val = iv_path iv_start = lv_last_slash + 1 iv_end = lv_i ).
          ELSE.
            lv_res = slice( iv_val = iv_path iv_start = lv_last_slash + 1 iv_end = lv_i ).
          ENDIF.
          lv_last_segment_length = lv_i - lv_last_slash - 1.
        ENDIF.
        lv_last_slash = lv_i.
        lv_dots = 0.
      ELSEIF lv_code = c_dot AND lv_dots <> -1.
        lv_dots = lv_dots + 1.
      ELSE.
        lv_dots = -1.
      ENDIF.

      lv_i = lv_i + 1.
    ENDDO.

    rv_result = lv_res.

  ENDMETHOD.


  METHOD slice.
    IF iv_end <= iv_start.
      rv_result = ''.
    ELSEIF iv_end >= 0.
      rv_result = substring( val = iv_val off = iv_start len = iv_end - iv_start ).
    ELSE.
      rv_result = substring( val = iv_val off = iv_start len = strlen( iv_val ) - iv_end - iv_start ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
