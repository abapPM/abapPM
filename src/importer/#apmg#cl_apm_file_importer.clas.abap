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

    METHODS get_file
      IMPORTING
        !extension    TYPE string
        !extra        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_file_importer IMPLEMENTATION.


  METHOD /apmg/if_apm_file_importer~get_abap.

    DATA(file_data) = get_file(
      extra     = extra
      extension = 'abap' ).

    " TODO: Replace with ZCL_CONVERT
    TRY.
        DATA(code) = zcl_abapgit_convert=>xstring_to_string_utf8( file_data ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

    SPLIT code AT cl_abap_char_utilities=>newline INTO TABLE result.

  ENDMETHOD.


  METHOD /apmg/if_apm_file_importer~get_json.

    DATA(file_data) = get_file( 'json' ).

    " TODO: Replace with ZCL_CONVERT
    TRY.
        result = zcl_abapgit_convert=>xstring_to_string_utf8( file_data ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_file_importer~get_xml.

    DATA(file_data) = get_file( 'xml' ).

    " TODO: Replace with ZCL_CONVERT
    TRY.
        result = zcl_abapgit_convert=>xstring_to_string_utf8( file_data ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
    me->item  = item.
    me->files = files.
  ENDMETHOD.


  METHOD get_file.

    IF extra IS INITIAL.
      DATA(filename) = |{ item-obj_name }.{ item-obj_type }.{ extension }|.
    ELSE.
      filename = |{ item-obj_name }.{ item-obj_type }.{ extra }.{ extension }|.
    ENDIF.

    READ TABLE files ASSIGNING FIELD-SYMBOL(<file>)
      WITH KEY file COMPONENTS filename = condense( to_lower( filename ) ).
    IF sy-subrc = 0.
      " TODO: Replace with ZCL_CONVERT
      TRY.
          result = zcl_abapgit_convert=>xstring_to_string_utf8( <file>-data ).
        CATCH zcx_abapgit_exception INTO DATA(error).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
      ENDTRY.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = |File { filename } not found|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
