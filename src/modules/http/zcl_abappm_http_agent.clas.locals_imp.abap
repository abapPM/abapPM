CLASS lcl_http_response DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES ZIF_ABAPPM_HTTP_RESPONSE.

    CLASS-METHODS create
      IMPORTING
        ii_client          TYPE REF TO if_http_client
      RETURNING
        VALUE(ri_response) TYPE REF TO ZIF_ABAPPM_HTTP_RESPONSE.

  PRIVATE SECTION.

    DATA mi_client TYPE REF TO if_http_client.
    DATA mi_response TYPE REF TO if_http_response.

ENDCLASS.

CLASS lcl_http_response IMPLEMENTATION.

  METHOD create.
    DATA lo_response TYPE REF TO lcl_http_response.
    CREATE OBJECT lo_response.
    lo_response->mi_client   = ii_client.
    lo_response->mi_response = ii_client->response.
    ri_response ?= lo_response.
  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~CLOSE.
    mi_client->close( ).
  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~IS_OK.
    DATA lv_code TYPE i.
    lv_code = ZIF_ABAPPM_HTTP_RESPONSE~CODE( ).
    rv_yes = boolc( lv_code >= 200 AND lv_code < 300 ).
  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~DATA.
    rv_data = mi_response->get_data( ).
  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~CDATA.
    rv_data = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~CODE.
    DATA lv_msg TYPE string ##NEEDED.
    mi_response->get_status(
      IMPORTING
        reason = lv_msg " for debug
        code   = rv_code ).
  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~JSON.

    DATA lx_error TYPE REF TO ZCX_ABAPPM_AJSON_ERROR.

    TRY.
        ri_json = ZCL_ABAPPM_AJSON=>PARSE( ZIF_ABAPPM_HTTP_RESPONSE~CDATA( ) ).
      CATCH ZCX_ABAPPM_AJSON_ERROR INTO lx_error.
        ZCX_ABAPPM_ERROR=>RAISE_WITH_TEXT( lx_error ).
    ENDTRY.

  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~ERROR.
    rv_message = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD ZIF_ABAPPM_HTTP_RESPONSE~HEADERS.

    DATA lt_headers TYPE tihttpnvp.

    FIELD-SYMBOLS <ls_header> LIKE LINE OF lt_headers.

    CREATE OBJECT ro_headers.

    mi_response->get_header_fields( CHANGING fields = lt_headers ).

    LOOP AT lt_headers ASSIGNING <ls_header>.
      ro_headers->set(
        iv_key = <ls_header>-name
        iv_val = <ls_header>-value ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
