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
        !rfc_destination TYPE rfcdest OPTIONAL
        !ssl_id          TYPE ssfapplssl DEFAULT 'ANONYM'
        !proxy_host      TYPE string OPTIONAL
        !proxy_service   TYPE string OPTIONAL
        !proxy_user      TYPE string OPTIONAL
        !proxy_password  TYPE string OPTIONAL
      RETURNING
        VALUE(result)    TYPE REF TO /apmg/if_apm_http_agent.

    METHODS constructor
      IMPORTING
        !rfc_destination TYPE rfcdest
        !ssl_id          TYPE ssfapplssl
        !proxy_host      TYPE string
        !proxy_service   TYPE string
        !proxy_user      TYPE string
        !proxy_password  TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      rfc_destination TYPE rfcdest,
      ssl_id          TYPE ssfapplssl,
      proxy_host      TYPE string,
      proxy_service   TYPE string,
      proxy_user      TYPE string,
      proxy_password  TYPE string,
      global_headers  TYPE REF TO /apmg/cl_apm_string_map.

    METHODS get_http_client
      IMPORTING
        !url          TYPE string
      RETURNING
        VALUE(result) TYPE REF TO if_http_client
      RAISING
        /apmg/cx_apm_error.

    METHODS get_connection_longtext
      IMPORTING
        !host         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS attach_payload
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

    DATA(http_client) = get_http_client( url ).

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

    " If "Authorization" header is set, we could disable login popup
    " but it's better to show the popup so we can adjust the login flow
    " i.e. provide
    " http_client->propertytype_logon_popup = http_client->co_disabled

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
          code    = DATA(status_code)
          message = DATA(message) ).

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

    me->rfc_destination = rfc_destination.
    me->ssl_id          = ssl_id.
    me->proxy_host      = proxy_host.
    me->proxy_service   = proxy_service.
    me->proxy_user      = proxy_user.
    me->proxy_password    = proxy_password.

    global_headers = NEW #( ).

  ENDMETHOD.


  METHOD create.

    result = NEW /apmg/cl_apm_http_agent(
      rfc_destination = rfc_destination
      ssl_id          = ssl_id
      proxy_host      = proxy_host
      proxy_service   = proxy_service
      proxy_user      = proxy_user
      proxy_password  = proxy_password ).

  ENDMETHOD.


  METHOD get_connection_longtext.

    " TODO: Replace with full link once docs are available
    CONSTANTS c_docs TYPE string VALUE 'https://docs.abappm.com/'.

    IF rfc_destination IS INITIAL.
      IF proxy_host IS NOT INITIAL.
        DATA(proxy) = | via proxy <b>{ proxy_host }:{ proxy_service }</b>|.
      ENDIF.

      result = |The system is trying to connect to <b>{ host }</b> |
        && |using SSL certificates under <b>{ ssl_id }</b>{ proxy }. |.
    ELSE.
      result = |The system is trying to connect using RFC destination <b>{ rfc_destination }</b>. |.
    ENDIF.

    result = result
      && |Check system parameters (transaction |
      && |<a href="sapevent:jump_transaction?transaction=RZ10" class="no-pad">RZ10</a>|
      && |), SSL setup (transaction |
      && |<a href="sapevent:jump_transaction?transaction=STRUST" class="no-pad">STRUST</a>|
      && |), Internet connection monitor (transaction |
      && |<a href="sapevent:jump_transaction?transaction=SMICM" class="no-pad">SMICM</a>|
      && |)|.

    IF rfc_destination IS NOT INITIAL.
      result = result
        && |, and RFC configuration (|
        && |<a href="sapevent:jump_transaction??transaction=SM59" class="no-pad">SM59</a>|
        && |)|.
    ENDIF.

    IF proxy IS NOT INITIAL.
      result = result
        && |, and proxy configuration (|
        && |<a href="sapevent:go_settings" class="no-pad">personal settings</a>|
        && |)|.
    ENDIF.

    result = result
      && |. It's recommended to get your SAP Basis and network teams involved. |
      && |For more information and troubleshooting, see the |
      && |<a href="sapevent:url?url={ c_docs }" class="no-pad">documentation</a>|
      && |.|.

  ENDMETHOD.


  METHOD get_http_client.

    IF rfc_destination IS INITIAL.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = url
          ssl_id             = ssl_id
          proxy_host         = proxy_host
          proxy_service      = proxy_service
          proxy_user         = proxy_user
          proxy_passwd       = proxy_password
        IMPORTING
          client             = result
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          pse_not_found      = 4
          pse_not_distrib    = 5
          pse_errors         = 6
          OTHERS             = 7 ).

    ELSE.

      cl_http_client=>create_by_destination(
        EXPORTING
          destination              = rfc_destination
        IMPORTING
          client                   = result
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6 ).

    ENDIF.

    IF sy-subrc <> 0.
      DATA(longtext) = get_connection_longtext( /apmg/cl_apm_http_login_manage=>get_host( url ) ).

      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100 EXPORTING longtext = longtext.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
