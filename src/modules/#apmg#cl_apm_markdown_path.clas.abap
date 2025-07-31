CLASS /apmg/cl_apm_markdown_path DEFINITION
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
        path          TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_slash TYPE string VALUE '/'.
    CONSTANTS c_dot TYPE string VALUE '.'.

    METHODS posix_normalize
      IMPORTING
        path             TYPE string
        allow_above_root TYPE abap_bool
      RETURNING
        VALUE(result)    TYPE string.

    METHODS char_at
      IMPORTING
        val           TYPE string
        off           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS slice
      IMPORTING
        val           TYPE string
        start         TYPE i
        end           TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS last_index_of
      IMPORTING
        val           TYPE string
        sub           TYPE string
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS /apmg/cl_apm_markdown_path IMPLEMENTATION.


  METHOD char_at.
    result = substring( val = val off = off len = 1 ).
  ENDMETHOD.


  METHOD last_index_of.
    result = find( val = val sub = sub occ = -1 ).
  ENDMETHOD.


  METHOD normalize.

    DATA:
      is_absolute        TYPE abap_bool,
      trailing_separator TYPE abap_bool,
      allow_above_root   TYPE abap_bool.

    result = path.
    IF result IS INITIAL.
      result = '.'.
      RETURN.
    ENDIF.

    is_absolute        = xsdbool( substring( val = result len = 1 ) = c_slash ).
    trailing_separator = xsdbool( substring( val = reverse( result ) len = 1 ) = c_slash ).
    allow_above_root   = xsdbool( is_absolute = abap_false ).

    result = posix_normalize(
      path             = result
      allow_above_root = allow_above_root ).

    IF result IS INITIAL AND is_absolute = abap_false.
      result = '.'.
    ENDIF.
    IF result IS NOT INITIAL AND trailing_separator = abap_true.
      result = result && '/'.
    ENDIF.
    IF is_absolute = abap_true.
      result = '/' && result.
    ENDIF.

  ENDMETHOD.


  METHOD posix_normalize.

    DATA:
      out                 TYPE string,
      out_tab             TYPE string_table,
      last_segment_length TYPE i,
      last_slash_index    TYPE i,
      last_slash          TYPE i VALUE -1,
      dots                TYPE i,
      code                TYPE c LENGTH 1,
      i                   TYPE i.

    DO strlen( path ) + 1 TIMES.
      IF strlen( path ) > i.
        code = char_at( val = path off = i ).
      ELSEIF code = c_slash.
        EXIT.
      ELSE.
        code = c_slash.
      ENDIF.

      out = |{ i } { result }|.
      INSERT out INTO TABLE out_tab.
      IF code = c_slash.
        IF i - 1 = last_slash OR dots = 1.
          ASSERT 0 = 0. " NOP
        ELSEIF i - 1 <> last_slash AND dots = 2.
          IF strlen( result ) < 2 OR
            last_segment_length <> 2 OR
            char_at( val = result off = strlen( result ) - 1 ) <> c_dot OR
            char_at( val = result off = strlen( result ) - 2 ) <> c_dot.
            IF strlen( result ) > 2.
              last_slash_index = last_index_of( val = result sub = c_slash ).
              IF strlen( result ) - 1 <> last_slash_index.
                out = |{ i } { result } #1|.
                INSERT out INTO TABLE out_tab.
                IF last_slash_index = -1.
                  result = ''.
                  last_segment_length = 0.
                ELSE.
                  result = slice( val = result start = 0 end = last_slash_index ).
                  last_segment_length = strlen( result ) - 1 - last_index_of( val = result sub = c_slash ).
                ENDIF.
                last_slash = i.
                dots = 0.
                i = i + 1.
                CONTINUE.
              ENDIF.
            ELSEIF strlen( result ) = 2 OR strlen( result ) = 1.
              result = ''.
              last_segment_length = 0.
              last_slash = i.
              dots = 0.
              i = i + 1.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF allow_above_root = abap_true.
            out = |{ i } { result } #3|.
            INSERT out INTO TABLE out_tab.
            IF strlen( result ) > 0.
              result = result && c_slash && c_dot && c_dot.
            ELSE.
              result = c_dot && c_dot.
            ENDIF.
            last_segment_length = 2.
          ENDIF.
        ELSE.
          out = |{ i } { result } #2|.
          INSERT out INTO TABLE out_tab.
          IF strlen( result ) > 0.
            result = result && c_slash && slice( val = path start = last_slash + 1 end = i ).
          ELSE.
            result = slice( val = path start = last_slash + 1 end = i ).
          ENDIF.
          last_segment_length = i - last_slash - 1.
        ENDIF.
        last_slash = i.
        dots = 0.
      ELSEIF code = c_dot AND dots <> -1.
        dots = dots + 1.
      ELSE.
        dots = -1.
      ENDIF.

      i = i + 1.
    ENDDO.

  ENDMETHOD.


  METHOD slice.
    IF end <= start.
      result = ''.
    ELSEIF end >= 0.
      result = substring( val = val off = start len = end - start ).
    ELSE.
      result = substring( val = val off = start len = strlen( val ) - end - start ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
