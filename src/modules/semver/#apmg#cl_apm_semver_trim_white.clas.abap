CLASS /apmg/cl_apm_semver_trim_white DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* SemVer Trim Comparator Whitespace
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
* Mirrors node-semver internal/trim-comparator-whitespace.js boundaries,
* not version validation. Only one matched whitespace character is removed.
************************************************************************
  PUBLIC SECTION.

    TYPES ty_whitespace_characters TYPE STANDARD TABLE OF i WITH KEY table_line.

    CLASS-DATA whitespace_characters TYPE ty_whitespace_characters READ-ONLY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS normalize_whitespace
      IMPORTING
        text          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS trim_comparator_whitespace
      IMPORTING
        range         TYPE string
      RETURNING
        VALUE(result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA whitespace TYPE string.

    DATA text TYPE string.
    DATA length TYPE i.
    DATA:
      BEGIN OF digit_runs,
        start          TYPE i VALUE -1,
        end            TYPE i VALUE -1,
        previous_start TYPE i VALUE -1,
        previous_end   TYPE i VALUE -1,
      END OF digit_runs.

    METHODS scan
      RETURNING
        VALUE(result) TYPE string.

    METHODS character_at
      IMPORTING
        position      TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS is_whitespace
      IMPORTING
        character     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_version_prefix
      IMPORTING
        character     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_digit
      IMPORTING
        character     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_letter
      IMPORTING
        character     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_identifier_character
      IMPORTING
        character     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_version_start
      IMPORTING
        character     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS find_digits_end
      IMPORTING
        start         TYPE i
        limit         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_version_number_end
      IMPORTING
        start         TYPE i
        loose         TYPE abap_bool
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_prerelease_identifier_end
      IMPORTING
        start         TYPE i
        loose         TYPE abap_bool
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_prerelease_end
      IMPORTING
        start         TYPE i
        loose         TYPE abap_bool
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_build_identifier_end
      IMPORTING
        start         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_build_end
      IMPORTING
        start         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_loose_full_version_end
      IMPORTING
        start         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_partial_component_end
      IMPORTING
        start         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_partial_version_end
      IMPORTING
        start         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_version_end
      IMPORTING
        start         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS find_removable_whitespace
      IMPORTING
        start         TYPE i
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS /apmg/cl_apm_semver_trim_white IMPLEMENTATION.


  METHOD character_at.
    IF position >= 0 AND position < length.
      result = text+position(1).
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.
    " Compared to ECMAscript,
    " - ABAP REGEX includes \u200b but not \u205f nor \ufeff
    " - ABAP PCRE includes \u0085 and \u180e but not \ufeff
    " https://www.pcre.org/original/doc/html/pcrepattern.html
    DATA(whitespace_chars) =
      ` ,\t,\n,\v,\f,\r,\u00a0,\u1680,\u2000,\u2001,\u2002,\u2003,\u2004,`
      && `\u2005,\u2006,\u2007,\u2008,\u2009,\u200a,\u2028,\u2029,\u202f,`
      && `\u3000,`
      && `\u200b`. " added for POSIX REGEX
    "&& `\u0085,\u180e` " added for PCRE

    SPLIT whitespace_chars AT ',' INTO TABLE DATA(characters).
    LOOP AT characters INTO DATA(character).
      DATA(char) = ``.
      " The constants contain JavaScript escapes, not ABAP control characters.
      CASE character.
        WHEN `\t`.
          char = cl_abap_conv_in_ce=>uccp( '0009' ).
        WHEN `\n`.
          char = cl_abap_conv_in_ce=>uccp( '000A' ).
        WHEN `\v`.
          char = cl_abap_conv_in_ce=>uccp( '000B' ).
        WHEN `\f`.
          char = cl_abap_conv_in_ce=>uccp( '000C' ).
        WHEN `\r`.
          char = cl_abap_conv_in_ce=>uccp( '000D' ).
        WHEN OTHERS.
          IF strlen( character ) = 6 AND character(2) = `\u`.
            DATA(codepoint) = to_upper( substring( val = character off = 2 len = 4 ) ).
            char = cl_abap_conv_in_ce=>uccp( codepoint ).
          ELSE.
            char = character.
          ENDIF.
      ENDCASE.

      whitespace = whitespace && char.

      DATA(code) = cl_abap_conv_out_ce=>uccpi( char ).
      INSERT code INTO TABLE whitespace_characters.
    ENDLOOP.

    SORT whitespace_characters.
  ENDMETHOD.


  METHOD find_build_end.
    result = start.
    DATA(character) = character_at( start ).
    CHECK character = '+'.
    DATA(identifier_start) = start + 1.
    result = find_build_identifier_end( identifier_start ).
    IF result = identifier_start.
      result = start.
      RETURN.
    ENDIF.
    character = character_at( result ).
    WHILE character = '.'.
      DATA(next_start) = result + 1.
      DATA(next) = find_build_identifier_end( next_start ).
      IF next = next_start.
        RETURN.
      ENDIF.
      result = next.
      character = character_at( result ).
    ENDWHILE.
  ENDMETHOD.


  METHOD find_build_identifier_end.
    result = start.
    WHILE result - start < /apmg/if_apm_semver_constants=>max_safe_build_length
        AND is_identifier_character( character_at( result ) ) = abap_true.
      result = result + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD find_digits_end.
    result = start.
    CHECK is_digit( character_at( start ) ) = abap_true.
    IF start >= digit_runs-start AND start < digit_runs-end.
      result = nmin( val1 = start + limit val2 = digit_runs-end ).
    ELSEIF start >= digit_runs-previous_start AND start < digit_runs-previous_end.
      result = nmin( val1 = start + limit val2 = digit_runs-previous_end ).
    ELSE.
      result = start + 1.
      WHILE is_digit( character_at( result ) ) = abap_true.
        result = result + 1.
      ENDWHILE.
      digit_runs-previous_start = digit_runs-start.
      digit_runs-previous_end = digit_runs-end.
      digit_runs-start = start.
      digit_runs-end = result.
      result = nmin( val1 = start + limit val2 = result ).
    ENDIF.
  ENDMETHOD.


  METHOD find_loose_full_version_end.
    result = start.
    DO 3 TIMES.
      DATA(component) = sy-index.
      DATA(next) = find_version_number_end( start = result loose = abap_true ).
      IF next = result.
        result = start.
        RETURN.
      ENDIF.
      result = next.
      IF component < 3.
        IF character_at( result ) <> '.'.
          result = start.
          RETURN.
        ENDIF.
        result = result + 1.
      ENDIF.
    ENDDO.
    result = find_prerelease_end( start = result loose = abap_true ).
    result = find_build_end( result ).
  ENDMETHOD.


  METHOD find_partial_component_end.
    result = start.
    DATA(character) = character_at( start ).
    IF is_digit( character ) = abap_true.
      result = find_version_number_end( start = start loose = abap_false ).
    ELSEIF is_version_start( character ) = abap_true.
      result = start + 1.
    ENDIF.
  ENDMETHOD.


  METHOD find_partial_version_end.
    result = find_partial_component_end( start ).
    DO 2 TIMES.
      DATA(character) = character_at( result ).
      IF character <> '.'.
        RETURN.
      ENDIF.
      DATA(next_start) = result + 1.
      DATA(next) = find_partial_component_end( next_start ).
      IF next = next_start.
        RETURN.
      ENDIF.
      result = next.
    ENDDO.
    result = find_prerelease_end( start = result loose = abap_false ).
    result = find_build_end( result ).
  ENDMETHOD.


  METHOD find_prerelease_end.
    result = start.
    DATA(hyphen) = xsdbool( character_at( start ) = '-' ).
    CHECK loose = abap_true OR hyphen = abap_true.
    DATA(identifier_start) = start.
    IF hyphen = abap_true.
      identifier_start = identifier_start + 1.
    ENDIF.
    result = find_prerelease_identifier_end( start = identifier_start loose = loose ).
    IF result = identifier_start.
      IF loose = abap_false OR hyphen = abap_false.
        result = start.
        RETURN.
      ENDIF.
      result = find_prerelease_identifier_end( start = start loose = loose ).
    ENDIF.
    DATA(character) = character_at( result ).
    WHILE character = '.'.
      DATA(next_start) = result + 1.
      DATA(next) = find_prerelease_identifier_end( start = next_start loose = loose ).
      IF next = next_start.
        RETURN.
      ENDIF.
      result = next.
      character = character_at( result ).
    ENDWHILE.
  ENDMETHOD.


  METHOD find_prerelease_identifier_end.
    DATA(digits_end) = find_digits_end( start = start limit = /apmg/if_apm_semver_constants=>max_length ).
    DATA(character) = character_at( digits_end ).
    IF is_letter( character ) = abap_true OR character = '-'.
      result = digits_end + 1.
      DATA(limit) = result + /apmg/if_apm_semver_constants=>max_safe_build_length.
      WHILE result < limit AND is_identifier_character( character_at( result ) ) = abap_true.
        result = result + 1.
      ENDWHILE.
    ELSE.
      result = find_version_number_end( start = start loose = loose ).
    ENDIF.
  ENDMETHOD.


  METHOD find_removable_whitespace.
    DATA(position) = start.
    IF is_whitespace( character_at( position ) ) = abap_true.
      position = position + 1.
    ENDIF.
    DATA(character) = character_at( position ).
    IF character = '<' OR character = '>'.
      position = position + 1.
    ENDIF.
    character = character_at( position ).
    IF character = '='.
      position = position + 1.
    ENDIF.
    result = COND #( WHEN is_whitespace( character_at( position ) ) = abap_true THEN position ELSE -1 ).
  ENDMETHOD.


  METHOD find_version_end.
    result = find_loose_full_version_end( start ).
    IF result = start.
      result = find_partial_version_end( start ).
    ENDIF.
  ENDMETHOD.


  METHOD find_version_number_end.
    IF loose = abap_false AND character_at( start ) = '0'.
      result = start + 1.
    ELSE.
      DATA(limit) = /apmg/if_apm_semver_constants=>max_length.
      IF loose = abap_false.
        limit = limit + 1.
      ENDIF.
      result = find_digits_end( start = start limit = limit ).
    ENDIF.
  ENDMETHOD.


  METHOD is_digit.
    result = xsdbool( character IS NOT INITIAL AND character CO '0123456789' ).
  ENDMETHOD.


  METHOD is_identifier_character.
    result = xsdbool( is_digit( character ) = abap_true OR is_letter( character ) = abap_true OR character = '-' ).
  ENDMETHOD.


  METHOD is_letter.
    result = xsdbool( character IS NOT INITIAL AND character CO 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ).
  ENDMETHOD.


  METHOD is_version_prefix.
    result = xsdbool( character = 'v' OR character = '=' OR is_whitespace( character ) = abap_true ).
  ENDMETHOD.


  METHOD is_version_start.
    result = xsdbool( is_digit( character ) = abap_true OR character = 'x' OR character = 'X' OR character = '*' ).
  ENDMETHOD.


  METHOD is_whitespace.
    result = xsdbool( character IS NOT INITIAL AND whitespace CS character ).
  ENDMETHOD.


  METHOD normalize_whitespace.
    result = text.
    DO strlen( whitespace ) TIMES.
      DATA(offset) = sy-index - 1.
      result = replace(
        val  = result
        sub  = whitespace+offset(1)
        with = ` `
        occ  = 0 ).
    ENDDO.
  ENDMETHOD.


  METHOD scan.
    DATA copy_from TYPE i.
    DATA cursor TYPE i.

    WHILE cursor < length.
      DATA(match_start) = cursor.
      DATA(character) = character_at( cursor ).
      IF character = '<' OR character = '>'.
        cursor = cursor + 1.
      ELSEIF is_version_prefix( character ) = abap_false AND is_version_start( character ) = abap_false.
        cursor = cursor + 1.
        CONTINUE.
      ENDIF.

      WHILE cursor < length AND is_version_prefix( character_at( cursor ) ) = abap_true.
        cursor = cursor + 1.
      ENDWHILE.
      CHECK is_version_start( character_at( cursor ) ) = abap_true.

      DATA(position) = find_removable_whitespace( match_start ).
      IF position <> -1.
        DATA(copy_length) = position - copy_from.
        result = result && substring( val = text off = copy_from len = copy_length ).
        copy_from = position + 1.
      ENDIF.
      cursor = find_version_end( cursor ).
    ENDWHILE.
    result = result && substring( val = text off = copy_from ).
  ENDMETHOD.


  METHOD trim_comparator_whitespace.
    DATA(scanner) = NEW /apmg/cl_apm_semver_trim_white( ).
    scanner->text = range.
    scanner->length = strlen( range ).
    result = scanner->scan( ).
  ENDMETHOD.
ENDCLASS.
