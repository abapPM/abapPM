CLASS /apmg/cl_apm_http_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* HTTP Agent
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_http_agent.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_http_agent.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA global_headers TYPE REF TO /apmg/cl_apm_string_map.

    CLASS-METHODS attach_payload
      IMPORTING
        request TYPE REF TO if_http_request
        payload TYPE any
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_http_agent IMPLEMENTATION.


  METHOD attach_payload.

    DATA(payload_type) = cl_abap_typedescr=>describe_by_data( payload ).

    CASE payload_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_xstring.
        request->set_data( payload ).
      WHEN cl_abap_typedescr=>typekind_string.
        request->set_cdata( payload ).
      WHEN cl_abap_typedescr=>typekind_char.
        request->set_cdata( |{ payload }| ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Unexpected payload type { payload_type->absolute_name }|.
    ENDCASE.

  ENDMETHOD.


  METHOD constructor.

    global_headers = NEW #( ).

  ENDMETHOD.


  METHOD create.

    result = NEW /apmg/cl_apm_http_agent( ).

  ENDMETHOD.


  METHOD /apmg/if_apm_http_agent~global_headers.

    result = global_headers.

  ENDMETHOD.


  METHOD /apmg/if_apm_http_agent~request.

    DATA:
      http_client TYPE REF TO if_http_client,
      status_code TYPE i,
      message     TYPE string.

    " TODO: Add proxy support
    cl_http_client=>create_by_url(
      EXPORTING
        url    = url
        ssl_id = ssl_id
      IMPORTING
        client = http_client ).

    http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    http_client->request->set_method( method ).

    IF query IS BOUND.
      LOOP AT query->mt_entries ASSIGNING FIELD-SYMBOL(<entry>).
        http_client->request->set_form_field(
          name  = <entry>-k
          value = <entry>-v ).
      ENDLOOP.
    ENDIF.

    LOOP AT global_headers->mt_entries ASSIGNING <entry>.
      http_client->request->set_header_field(
        name  = <entry>-k
        value = <entry>-v ).
    ENDLOOP.

    IF headers IS BOUND.
      LOOP AT headers->mt_entries ASSIGNING <entry>.
        http_client->request->set_header_field(
          name  = <entry>-k
          value = <entry>-v ).
      ENDLOOP.
    ENDIF.

    IF method = /apmg/if_apm_http_agent=>c_method-post
      OR method = /apmg/if_apm_http_agent=>c_method-put
      OR method = /apmg/if_apm_http_agent=>c_method-patch.
      attach_payload(
        request = http_client->request
        payload = payload ).
    ENDIF.

    http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc = 0.
      http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      http_client->get_last_error(
        IMPORTING
          code    = status_code
          message = message ).

      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |HTTP error: [{ status_code }] { message }|.
    ENDIF.

    result = lcl_http_response=>create( http_client ).

  ENDMETHOD.
ENDCLASS.
