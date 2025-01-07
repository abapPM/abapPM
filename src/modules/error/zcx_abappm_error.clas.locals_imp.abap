CLASS lcl_error_longtext DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS remove_newlines
      IMPORTING
        longtext      TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS to_string
      IMPORTING
        lines         TYPE tline_tab
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.

    CLASS-METHODS remove_empty_section
      IMPORTING
        !tabix_from TYPE i
        !tabix_to   TYPE i
      CHANGING
        !itf        TYPE tline_tab.

    CLASS-METHODS replace_section_head_with_text
      CHANGING
        !itf TYPE tline.

ENDCLASS.

CLASS lcl_error_longtext IMPLEMENTATION.

  METHOD remove_newlines.

    result = longtext.
    REPLACE ALL OCCURRENCES OF ` ` && cl_abap_char_utilities=>cr_lf IN result WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN result WITH ` `.
    REPLACE ALL OCCURRENCES OF ` ` && cl_abap_char_utilities=>newline IN result WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN result WITH ` `.

  ENDMETHOD.

  METHOD to_string.

    CONSTANTS c_format_section TYPE string VALUE 'U1'.

    DATA:
      stream       TYPE TABLE OF tdline,
      stream_lines TYPE TABLE OF string,
      itf          TYPE tline_tab,
      has_content  TYPE abap_bool,
      tabix_from   TYPE syst-tabix,
      tabix_to     TYPE syst-tabix.

    itf = lines.

    " We replace the U1 format because that preserves the section header of longtexts
    LOOP AT itf ASSIGNING FIELD-SYMBOL(<section>) WHERE tdformat = c_format_section.

      CLEAR:
        has_content,
        tabix_to.

      tabix_from = sy-tabix.

      LOOP AT itf ASSIGNING FIELD-SYMBOL(<section_item>) FROM sy-tabix + 1.

        IF <section_item>-tdformat = c_format_section.
          tabix_to = sy-tabix.
          EXIT.
        ELSEIF <section_item>-tdline IS NOT INITIAL.
          has_content = abap_true.
        ENDIF.

      ENDLOOP.

      IF has_content = abap_false.
        remove_empty_section(
          EXPORTING
            tabix_from = tabix_from
            tabix_to   = tabix_to
          CHANGING
            itf        = itf ).
        CONTINUE.
      ENDIF.

      replace_section_head_with_text( CHANGING itf = <section> ).

    ENDLOOP.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        lf           = 'X'
      IMPORTING
        stream_lines = stream_lines
      TABLES
        itf_text     = itf
        text_stream  = stream.

    result = concat_lines_of(
      table = stream_lines
      sep   = cl_abap_char_utilities=>newline ).

  ENDMETHOD.

  METHOD remove_empty_section.

    IF tabix_to BETWEEN tabix_from AND lines( itf ).
      DELETE itf FROM tabix_from TO tabix_to.
    ELSE.
      DELETE itf FROM tabix_from.
    ENDIF.

  ENDMETHOD.

  METHOD replace_section_head_with_text.

    CONSTANTS:
      BEGIN OF c_section_text,
        cause           TYPE string VALUE `Cause`,
        system_response TYPE string VALUE `System response`,
        what_to_do      TYPE string VALUE `Procedure`,
        sys_admin       TYPE string VALUE `System administration`,
      END OF c_section_text,
      BEGIN OF c_section_token,
        cause           TYPE string VALUE `&CAUSE&`,
        system_response TYPE string VALUE `&SYSTEM_RESPONSE&`,
        what_to_do      TYPE string VALUE `&WHAT_TO_DO&`,
        sys_admin       TYPE string VALUE `&SYS_ADMIN&`,
      END OF c_section_token.

    CASE itf-tdline.
      WHEN c_section_token-cause.
        itf-tdline = c_section_text-cause.
      WHEN c_section_token-system_response.
        itf-tdline = c_section_text-system_response.
      WHEN c_section_token-what_to_do.
        itf-tdline = c_section_text-what_to_do.
      WHEN c_section_token-sys_admin.
        itf-tdline = c_section_text-sys_admin.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
