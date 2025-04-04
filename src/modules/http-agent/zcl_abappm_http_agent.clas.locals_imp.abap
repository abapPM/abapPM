CLASS lcl_http_response DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_abappm_http_response.

    CLASS-METHODS create
      IMPORTING
        http_client   TYPE REF TO if_http_client
      RETURNING
        VALUE(result) TYPE REF TO zif_abappm_http_response.

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

  METHOD zif_abappm_http_response~close.

    http_client->close( ).

  ENDMETHOD.

  METHOD zif_abappm_http_response~is_ok.

    DATA(status_code) = zif_abappm_http_response~code( ).
    result = xsdbool( status_code >= 200 AND status_code < 300 ).

  ENDMETHOD.

  METHOD zif_abappm_http_response~data.

    result = http_response->get_data( ).

  ENDMETHOD.

  METHOD zif_abappm_http_response~cdata.

    result = http_response->get_cdata( ).

  ENDMETHOD.

  METHOD zif_abappm_http_response~code.

    DATA msg TYPE string ##NEEDED.

    http_response->get_status(
      IMPORTING
        reason = msg " for debug
        code   = result ).

  ENDMETHOD.

  METHOD zif_abappm_http_response~json.

    TRY.
        result = zcl_abappm_ajson=>parse( zif_abappm_http_response~cdata( ) ).
      CATCH zcx_abappm_ajson_error INTO DATA(error).
        zcx_abappm_error=>raise_with_text( error ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abappm_http_response~error.

    result = http_response->get_cdata( ).

  ENDMETHOD.

  METHOD zif_abappm_http_response~headers.

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
