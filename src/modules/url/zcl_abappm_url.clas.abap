CLASS zcl_abappm_url DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    TYPES:
      BEGIN OF ty_url_components,
        scheme     TYPE string,
        username   TYPE string,
        password   TYPE string,
        host       TYPE string,
        port       TYPE string,
        path       TYPE string,
        query      TYPE string,
        fragment   TYPE string,
        is_special TYPE abap_bool,
      END OF ty_url_components.

    DATA components TYPE ty_url_components READ-ONLY.

    CLASS-METHODS parse
      IMPORTING
        url           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_url
      RAISING
        zcx_abappm_error.

    CLASS-METHODS default_port
      IMPORTING
        scheme        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS serialize
      IMPORTING
        components TYPE ty_url_components
      RETURNING
        VALUE(url) TYPE string
      RAISING
        zcx_abappm_error.

    METHODS constructor
      IMPORTING
        components TYPE ty_url_components.

  PRIVATE SECTION.

    CLASS-METHODS is_special_scheme
      IMPORTING
        scheme        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS validate_scheme
      IMPORTING
        scheme TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS parse_authority
      IMPORTING
        authority TYPE string
      EXPORTING
        username  TYPE string
        password  TYPE string
        host      TYPE string
        port      TYPE string
      RAISING
        zcx_abappm_error.

    CLASS-METHODS normalize_path
      IMPORTING
        path          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS percent_encode
      IMPORTING
        raw           TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS percent_decode
      IMPORTING
        raw           TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS validate_ipv6_address
      IMPORTING
        address TYPE string
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_url IMPLEMENTATION.


  METHOD constructor.
    me->components = components.
  ENDMETHOD.


  METHOD default_port.

    CASE to_lower( scheme ).
      WHEN 'file'.
        result = ''.
      WHEN 'ftp'.
        result = '21'.
      WHEN 'http'.
        result = '80'.
      WHEN 'https'.
        result = '443'.
      WHEN 'ws'.
        result = '80'.
      WHEN 'wss'.
        result = '443'.
    ENDCASE.

  ENDMETHOD.


  METHOD is_special_scheme.

    CASE to_lower( scheme ).
      WHEN 'file' OR 'ftp' OR 'http' OR 'https' OR 'ws' OR 'wss'.
        result = abap_true.
      WHEN OTHERS.
        result = abap_false.
    ENDCASE.

  ENDMETHOD.


  METHOD normalize_path.

    DATA normalized_path TYPE string_table.

    CHECK path IS NOT INITIAL.

    DATA(len) = strlen( path ) - 1.
    SPLIT path AT '/' INTO TABLE DATA(path_segments).

    LOOP AT path_segments INTO DATA(segment).
      IF segment = '.' OR segment IS INITIAL.
        " Ignore '.' and empty segments
        CONTINUE.
      ELSEIF segment = '..' AND lines( normalized_path ) > 0.
        " Remove previous segment for '..'
        DELETE normalized_path INDEX lines( normalized_path ).
      ELSE.
        APPEND segment TO normalized_path.
      ENDIF.
    ENDLOOP.

    IF path+len(1) = '/'.
      APPEND '' TO normalized_path.
    ENDIF.

    " Reconstruct the normalized path
    LOOP AT normalized_path INTO segment.
      result = |{ result }/{ segment }|.
    ENDLOOP.

  ENDMETHOD.


  METHOD parse.

    DATA: components TYPE ty_url_components,
          remaining  TYPE string,
          authority  TYPE string,
          delimiter  TYPE i.

    IF url IS INITIAL.
      zcx_abappm_error=>raise( 'No URL' ).
    ENDIF.

    " Remove leading/trailing spaces
    remaining = condense( url ).

    " Parse scheme
    delimiter = find( val = remaining sub = ':' ).
    IF delimiter < 0.
      zcx_abappm_error=>raise( 'Invalid URL: no scheme found' ).
    ENDIF.

    components-scheme = to_lower( remaining(delimiter) ).

    validate_scheme( components-scheme ).
    components-is_special = is_special_scheme( components-scheme ).

    " Remove scheme and ':' from remaining string
    delimiter += 1.
    remaining = remaining+delimiter.

    " Check if URL has authority (starts with '//')
    IF strlen( remaining ) >= 2 AND remaining(2) = '//'.
      remaining = remaining+2.

      " Find end of authority
      delimiter = find( val = remaining sub = '/' ).
      IF delimiter < 0.
        authority = remaining.
        CLEAR remaining.
      ELSE.
        authority = remaining(delimiter).
        remaining = remaining+delimiter.
      ENDIF.

      " Parse authority section
      parse_authority(
        EXPORTING
          authority = authority
        IMPORTING
          username  = components-username
          password  = components-password
          host      = components-host
          port      = components-port ).
    ENDIF.

    " Find query and fragment positions
    DATA(query_pos) = find( val = remaining sub = '?' ).
    DATA(fragment_pos) = find( val = remaining sub = '#' ).

    " Set path first
    IF query_pos = 0.
      " URL starts with ?
      components-path = ''.
      remaining = remaining+1.

      " Find fragment after query
      fragment_pos = find( val = remaining sub = '#' ).
      IF fragment_pos >= 0.
        components-query = remaining(fragment_pos).
        fragment_pos += 1.
        components-fragment = remaining+fragment_pos.
      ELSE.
        components-query = remaining.
      ENDIF.
    ELSEIF fragment_pos = 0.
      " URL starts with #
      components-path = ''.
      fragment_pos += 1.
      components-fragment = remaining+1.
    ELSE.
      " Normal case - extract path
      IF query_pos > 0 AND ( fragment_pos < 0 OR query_pos < fragment_pos ).
        " Path ends with ?
        components-path = remaining(query_pos).
        query_pos += 1.
        IF fragment_pos > query_pos.
          DATA(query_len) = fragment_pos - query_pos.
          components-query = remaining+query_pos(query_len).
          fragment_pos += 1.
          components-fragment = remaining+fragment_pos.
        ELSE.
          components-query = remaining+query_pos.
        ENDIF.
      ELSEIF fragment_pos > 0.
        " Path ends with #
        components-path = remaining(fragment_pos).
        fragment_pos += 1.
        components-fragment = remaining+fragment_pos.
      ELSE.
        " Only path
        components-path = remaining.
      ENDIF.
    ENDIF.

    components-path     = percent_decode( normalize_path( components-path ) ).
    components-query    = percent_decode( components-query ).
    components-fragment = percent_decode( components-fragment ).

    result = NEW zcl_abappm_url( components ).

  ENDMETHOD.


  METHOD parse_authority.

    DATA:
      temp        TYPE string,
      delimiter   TYPE i,
      credentials TYPE string.

    temp = authority.

    " Parse username and password
    delimiter = find( val = temp sub = '@' ).
    IF delimiter >= 0.
      credentials = temp(delimiter).
      delimiter += 1.
      temp = temp+delimiter.

      delimiter = find( val = credentials sub = ':' ).
      IF delimiter >= 0.
        username = percent_decode( |{ credentials(delimiter) }| ).
        delimiter += 1.
        password = percent_decode( |{ credentials+delimiter }| ).
      ELSE.
        username = percent_decode( credentials ).
      ENDIF.
    ENDIF.


    " Parse host and port
    " First check if we have an IPv6 address
    IF temp IS NOT INITIAL AND temp(1) = '['.
      " Find the closing bracket
      delimiter = find( val = temp sub = ']' ).
      IF delimiter < 0.
        zcx_abappm_error=>raise( 'Invalid IPv6 address: missing closing bracket' ).
      ENDIF.

      " Extract IPv6 address without brackets
      DATA(host_len) = delimiter - 1.
      host = temp+1(host_len).

      " Check if there's a port after the IPv6 address
      delimiter += 1.
      IF delimiter < strlen( temp ) AND temp+delimiter(1) = ':'.
        delimiter += 1.
        port = temp+delimiter.
      ENDIF.
    ELSE.
      " Regular hostname or IPv4
      delimiter = find( val = temp sub = ':' ).
      IF delimiter >= 0.
        host = temp(delimiter).
        delimiter += 1.
        port = temp+delimiter.
      ELSE.
        host = temp.
      ENDIF.

      " TODO: punycode
    ENDIF.

    " Validate port if present
    IF port IS NOT INITIAL.
      IF NOT matches( val = port regex = '^\d+$' ).
        zcx_abappm_error=>raise( 'Invalid port number' ).
      ENDIF.
    ENDIF.

    " Validate IPv6 address if present
    IF host IS NOT INITIAL AND temp(1) = '['.
      validate_ipv6_address( host ).
    ENDIF.

  ENDMETHOD.


  METHOD percent_decode.

    result = cl_http_utility=>unescape_url( |{ raw }| ).

    IF raw IS NOT INITIAL AND result IS INITIAL.
      result = raw.
      RETURN.
    ENDIF.

    " Replace "hash"
    result = replace(
      val  = result
      sub  = '%23'
      with = '#'
      occ  = 0 ).

    " Escape "tick"
    result = replace(
      val  = result
      sub  = |'|
      with = '%27'
      occ  = 0 ).

    " Preserve "plus"
    DATA(idx) = 0.
    DO strlen( raw ) TIMES.
      IF raw+idx(1) = '+'.
        DATA(idx2) = idx + 1.
        result = |{ result(idx) }+{ result+idx2(*) }|.
      ENDIF.
      idx += 1.
    ENDDO.

  ENDMETHOD.


  METHOD percent_encode.

    result = escape( val = |{ raw }| format = cl_abap_format=>e_url ).

    " Unescape "tick"
    result = replace(
      val  = result
      sub  = '%2527'
      with = '%27'
      occ  = 0 ).

  ENDMETHOD.


  METHOD serialize.

    url = |{ components-scheme }:|.

    " Add authority if host is present
    IF components-host IS NOT INITIAL OR components-scheme = 'file'.
      url = |{ url }//|.

      " Add credentials if present
      IF components-username IS NOT INITIAL.
        url = |{ url }{ percent_encode( components-username ) }|.
      ENDIF.
      IF components-password IS NOT INITIAL.
        url = |{ url }:{ percent_encode( components-password ) }|.
      ENDIF.
      IF components-username IS NOT INITIAL OR components-password IS NOT INITIAL.
        url = |{ url }@|.
      ENDIF.

      " Add host and port
      url = |{ url }{ components-host }|.
      IF components-port IS NOT INITIAL.
        url = |{ url }:{ components-port }|.
      ENDIF.
    ENDIF.

    " Add path
    IF components-path IS NOT INITIAL.
      IF components-path(1) <> '/'.
        url = |{ url }/|.
      ENDIF.
      url = |{ url }{ percent_encode( components-path ) }|.
    ENDIF.

    " Add query
    IF components-query IS NOT INITIAL.
      url = |{ url }?{ percent_encode( components-query ) }|.
    ENDIF.

    " Add fragment
    IF components-fragment IS NOT INITIAL.
      url = |{ url }#{ percent_encode( components-fragment ) }|.
    ENDIF.

  ENDMETHOD.


  METHOD validate_ipv6_address.

    DATA:
      parts TYPE TABLE OF string,
      part  TYPE string,
      count TYPE i.

    " Split by colons
    SPLIT address AT ':' INTO TABLE parts.

    " Basic validation of IPv6 format
    IF lines( parts ) > 8.
      zcx_abappm_error=>raise( 'Invalid IPv6 address: too many segments' ).
    ENDIF.

    " Check each part
    LOOP AT parts INTO part.
      " Empty part is allowed for :: notation, but only once
      IF part IS INITIAL.
        ADD 1 TO count.
        IF count > 1.
          zcx_abappm_error=>raise( 'Invalid IPv6 address: multiple empty segments' ).
        ENDIF.
        CONTINUE.
      ENDIF.

      " Validate hexadecimal format and length
      IF NOT matches( val = part regex = '^[0-9A-Fa-f]{1,4}$' ).
        zcx_abappm_error=>raise( 'Invalid IPv6 address: invalid hexadecimal segment' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_scheme.

    IF NOT matches( val = scheme regex = '^[A-Za-z][-A-Za-z0-9+.]*' ).
      zcx_abappm_error=>raise( 'Invalid scheme' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
