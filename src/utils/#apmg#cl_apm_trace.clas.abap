CLASS /apmg/cl_apm_trace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Trace
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS cdata
      IMPORTING
        !cdata TYPE csequence.

    CLASS-METHODS xdata
      IMPORTING
        !xdata TYPE xsequence.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /apmg/cl_apm_trace IMPLEMENTATION.


  METHOD cdata.

    GET PARAMETER ID 'ZAPM_TRACE' FIELD DATA(trace).
    GET PARAMETER ID 'ZAPM_TRACE_DIR' FIELD DATA(dir).
    CHECK trace = abap_true.

    GET TIME STAMP FIELD DATA(timestamp).

    TRY.
        zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
          iv_path = |{ dir }/apm-trace-{ timestamp }.log|
          iv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( cdata ) ).
      CATCH zcx_abapgit_exception /apmg/cx_apm_error.
        ASSERT 0 = 0.
    ENDTRY.

  ENDMETHOD.


  METHOD xdata.

    GET PARAMETER ID 'ZAPM_TRACE' FIELD DATA(trace).
    GET PARAMETER ID 'ZAPM_TRACE_DIR' FIELD DATA(dir).
    CHECK trace = abap_true.

    GET TIME STAMP FIELD DATA(timestamp).

    TRY.
        zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
          iv_path = |{ dir }/apm-trace-{ timestamp }.bin|
          iv_xstr = xdata ).
      CATCH zcx_abapgit_exception /apmg/cx_apm_error.
        ASSERT 0 = 0.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
