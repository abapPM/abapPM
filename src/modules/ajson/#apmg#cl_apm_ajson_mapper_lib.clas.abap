CLASS /apmg/cl_apm_ajson_mapper_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF rename_by,
        attr_name TYPE i VALUE 0,
        full_path TYPE i VALUE 1,
        pattern   TYPE i VALUE 2,
        " regex type i value 3, " TODO add if needed in future
      END OF rename_by.

    CLASS-METHODS create_upper_case
      RETURNING
        VALUE(ri_mapper) TYPE REF TO /apmg/if_apm_ajson_mapper.

    CLASS-METHODS create_lower_case
      RETURNING
        VALUE(ri_mapper) TYPE REF TO /apmg/if_apm_ajson_mapper.

    CLASS-METHODS create_rename
      IMPORTING
        it_rename_map    TYPE /apmg/if_apm_ajson_mapper=>tty_rename_map
        iv_rename_by     TYPE i DEFAULT rename_by-attr_name
      RETURNING
        VALUE(ri_mapper) TYPE REF TO /apmg/if_apm_ajson_mapper.

    CLASS-METHODS create_compound_mapper
      IMPORTING
        ii_mapper1       TYPE REF TO /apmg/if_apm_ajson_mapper OPTIONAL
        ii_mapper2       TYPE REF TO /apmg/if_apm_ajson_mapper OPTIONAL
        ii_mapper3       TYPE REF TO /apmg/if_apm_ajson_mapper OPTIONAL
        it_more          TYPE /apmg/if_apm_ajson_mapper=>ty_table_of OPTIONAL
      RETURNING
        VALUE(ri_mapper) TYPE REF TO /apmg/if_apm_ajson_mapper.

    CLASS-METHODS create_to_snake_case
      RETURNING
        VALUE(ri_mapper) TYPE REF TO /apmg/if_apm_ajson_mapper.

    CLASS-METHODS create_to_camel_case
      IMPORTING
        iv_first_json_upper TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_mapper)    TYPE REF TO /apmg/if_apm_ajson_mapper.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /apmg/cl_apm_ajson_mapper_lib IMPLEMENTATION.


  METHOD create_compound_mapper.

    DATA lt_queue TYPE /apmg/if_apm_ajson_mapper=>ty_table_of.

    APPEND ii_mapper1 TO lt_queue.
    APPEND ii_mapper2 TO lt_queue.
    APPEND ii_mapper3 TO lt_queue.
    APPEND LINES OF it_more TO lt_queue.
    DELETE lt_queue WHERE table_line IS INITIAL.

    CREATE OBJECT ri_mapper TYPE lcl_compound_mapper
      EXPORTING
        it_queue = lt_queue.

  ENDMETHOD.


  METHOD create_lower_case.

    CREATE OBJECT ri_mapper TYPE lcl_mapping_to_lower.

  ENDMETHOD.


  METHOD create_rename.

    CREATE OBJECT ri_mapper TYPE lcl_rename
      EXPORTING
        it_rename_map = it_rename_map
        iv_rename_by  = iv_rename_by.

  ENDMETHOD.


  METHOD create_to_camel_case.

    CREATE OBJECT ri_mapper TYPE lcl_to_camel
      EXPORTING
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.


  METHOD create_to_snake_case.

    CREATE OBJECT ri_mapper TYPE lcl_to_snake.

  ENDMETHOD.


  METHOD create_upper_case.

    CREATE OBJECT ri_mapper TYPE lcl_mapping_to_upper.

  ENDMETHOD.
ENDCLASS.
