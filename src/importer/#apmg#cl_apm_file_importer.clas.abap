CLASS /apmg/cl_apm_file_importer DEFINITION PUBLIC FINAL CREATE PUBLIC.

************************************************************************
* apm Files for Importer
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_file_importer.

    METHODS constructor
      IMPORTING
        !item  TYPE /apmg/if_apm_importer=>ty_item
        !files TYPE zif_abapgit_git_definitions=>ty_files_tt.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      item  TYPE /apmg/if_apm_object=>ty_item,
      files TYPE zif_abapgit_git_definitions=>ty_files_tt.

    METHODS get_file_name
      IMPORTING
        !extension    TYPE string
        !extra        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_file_content
      IMPORTING
        !extension    TYPE string
        !extra        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_file_importer IMPLEMENTATION.


  METHOD /apmg/if_apm_file_importer~get_abap.

    DATA(code) = get_file_content(
      extra     = extra
      extension = 'abap' ).

    SPLIT code AT cl_abap_char_utilities=>newline INTO TABLE result.

  ENDMETHOD.


  METHOD /apmg/if_apm_file_importer~get_json.

    result = get_file_content( 'json' ).

  ENDMETHOD.


  METHOD /apmg/if_apm_file_importer~get_xml.

    result = get_file_content( 'xml' ).

  ENDMETHOD.


  METHOD /apmg/if_apm_file_importer~get_xml_parsed.

    TRY.
        result = NEW zcl_abapgit_xml_input(
          iv_xml      = get_file_content( 'xml' )
          iv_filename = get_file_name( 'xml' ) ).

      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
    me->item  = item.
    me->files = files.
  ENDMETHOD.


  METHOD get_file_content.

    DATA(filename) = get_file_name(
      extra     = extra
      extension = extension ).

    READ TABLE files ASSIGNING FIELD-SYMBOL(<file>) WITH KEY file COMPONENTS filename = filename.
    IF sy-subrc = 0.
      TRY.
          " TODO: Replace with ZCL_CONVERT
          result = zcl_abapgit_convert=>xstring_to_string_utf8( <file>-data ).

        CATCH zcx_abapgit_exception INTO DATA(error).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
      ENDTRY.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = |File { filename } not found|.
    ENDIF.

  ENDMETHOD.


  METHOD get_file_name.

    IF extra IS INITIAL.
      result = |{ item-obj_name }.{ item-obj_type }.{ extension }|.
    ELSE.
      result = |{ item-obj_name }.{ item-obj_type }.{ extra }.{ extension }|.
    ENDIF.

    result = condense( to_lower( result ) ).

  ENDMETHOD.
ENDCLASS.
