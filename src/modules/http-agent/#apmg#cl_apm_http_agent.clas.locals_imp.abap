CLASS lcl_http_response DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_http_response.

    CLASS-METHODS create
      IMPORTING
        http_client   TYPE REF TO if_http_client
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_http_response.

  PRIVATE SECTION.

    DATA http_client TYPE REF TO if_http_client.
    DATA http_response TYPE REF TO if_http_response.

ENDCLASS.

CLASS lcl_http_response IMPLEMENTATION.

  METHOD create.

    DATA(response) = NEW lcl_http_response( ).

    response->http_client   = http_client.
    response->http_response = http_client->response.
    result ?= response.

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~close.

    http_client->close( ).

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~is_ok.

    DATA(status_code) = /apmg/if_apm_http_response~code( ).
    result = xsdbool( status_code >= 200 AND status_code < 300 ).

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~data.

    result = http_response->get_data( ).

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~cdata.

    result = http_response->get_cdata( ).

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~code.

    DATA msg TYPE string ##NEEDED.

    http_response->get_status(
      IMPORTING
        reason = msg " for debug
        code   = result ).

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~json.

    TRY.
        result = /apmg/cl_apm_ajson=>parse( /apmg/if_apm_http_response~cdata( ) ).
      CATCH /apmg/cx_apm_ajson_error INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~error.

    result = http_response->get_cdata( ).

  ENDMETHOD.

  METHOD /apmg/if_apm_http_response~headers.

    DATA headers TYPE tihttpnvp.

    " HTTP headers are not case sensitive and can have multiple entries (i.e. for cookies)
    result = NEW #( iv_case_insensitive = abap_true iv_list_mode = abap_true ).

    http_response->get_header_fields( CHANGING fields = headers ).

    LOOP AT headers ASSIGNING FIELD-SYMBOL(<header>).
      result->set(
        iv_key = <header>-name
        iv_val = <header>-value ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
