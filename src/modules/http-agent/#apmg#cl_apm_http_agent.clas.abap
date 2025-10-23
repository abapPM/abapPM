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
      IMPORTING
        !proxy_host    TYPE string OPTIONAL
        !proxy_service TYPE string OPTIONAL
        !proxy_user    TYPE string OPTIONAL
        !proxy_passwd  TYPE string OPTIONAL
      RETURNING
        VALUE(result)  TYPE REF TO /apmg/if_apm_http_agent.

    METHODS constructor
      IMPORTING
        !proxy_host    TYPE string OPTIONAL
        !proxy_service TYPE string OPTIONAL
        !proxy_user    TYPE string OPTIONAL
        !proxy_passwd  TYPE string OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      proxy_host     TYPE string,
      proxy_service  TYPE string,
      proxy_user     TYPE string,
      proxy_passwd   TYPE string,
      global_headers TYPE REF TO /apmg/cl_apm_string_map.

    CLASS-METHODS attach_payload
      IMPORTING
        request TYPE REF TO if_http_request
        payload TYPE any
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_http_agent IMPLEMENTATION.


  METHOD /apmg/if_apm_http_agent~global_headers.

    result = global_headers.

  ENDMETHOD.


  METHOD /apmg/if_apm_http_agent~request.

    DATA:
      http_client TYPE REF TO if_http_client,
      status_code TYPE i,
      message     TYPE string.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = url
        ssl_id             = ssl_id
        proxy_host         = proxy_host
        proxy_service      = proxy_service
        proxy_user         = proxy_user
        proxy_passwd       = proxy_passwd
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        pse_not_found      = 4
        pse_not_distrib    = 5
        pse_errors         = 6
        OTHERS             = 7 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

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

    me->proxy_host    = proxy_host.
    me->proxy_service = proxy_service.
    me->proxy_user    = proxy_user.
    me->proxy_passwd  = proxy_passwd.

    global_headers = NEW #( ).

  ENDMETHOD.


  METHOD create.

    result = NEW /apmg/cl_apm_http_agent(
      proxy_host    = proxy_host
      proxy_service = proxy_service
      proxy_user    = proxy_user
      proxy_passwd  = proxy_passwd ).

  ENDMETHOD.
ENDCLASS.
