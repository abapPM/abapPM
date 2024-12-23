CLASS lcl_http_response DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES ZIF_ABAPPM_HTTP_RESPONSE.

    CLASS-METHODS create
      IMPORTING
        http_client   TYPE REF TO if_http_client
      RETURNING
        VALUE(result) TYPE REF TO ZIF_ABAPPM_HTTP_RESPONSE.

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

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~CLOSE.

    http_client->close( ).

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~IS_OK.

    DATA(status_code) = ZIF_ABAPPM_HTTP_RESPONSE~CODE( ).
    result = boolc( status_code >= 200 AND status_code < 300 ).

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~DATA.

    result = http_response->get_data( ).

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~CDATA.

    result = http_response->get_cdata( ).

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~CODE.

    DATA msg TYPE string ##NEEDED.

    http_response->get_status(
      IMPORTING
        reason = msg " for debug
        code   = result ).

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~JSON.

    TRY.
        result = ZCL_ABAPPM_AJSON=>PARSE( ZIF_ABAPPM_HTTP_RESPONSE~CDATA( ) ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO DATA(error).
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( error ).
    ENDTRY.

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~ERROR.

    result = http_response->get_cdata( ).

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~HEADERS.

    DATA headers TYPE tihttpnvp.

    result = NEW #( ).

    http_response->get_header_fields( CHANGING fields = headers ).

    LOOP AT headers ASSIGNING FIELD-SYMBOL(<header>).
      result->set(
        iv_key = <header>-name
        iv_val = <header>-value ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
