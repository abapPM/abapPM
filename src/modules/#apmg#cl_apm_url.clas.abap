CLASS /apmg/cl_apm_url DEFINITION PUBLIC FINAL CREATE PUBLIC.

************************************************************************
* URL Object
*
* Implementation of WHATWG-URL standard
* https://url.spec.whatwg.org/
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* TODO: Add support for International Domain Names for Application
* (punycode)
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    TYPES:
      "! scheme://username:password@host:port/path?query#fragment
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
        VALUE(result) TYPE REF TO /apmg/cl_apm_url
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS default_port
      IMPORTING
        scheme        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS serialize
      IMPORTING
        components    TYPE ty_url_components
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

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
        /apmg/cx_apm_error.

    CLASS-METHODS parse_authority
      IMPORTING
        authority TYPE string
        scheme    TYPE string
      EXPORTING
        username  TYPE string
        password  TYPE string
        host      TYPE string
        port      TYPE string
      RAISING
        /apmg/cx_apm_error.

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
        /apmg/cx_apm_error.

    CLASS-METHODS validate_ipv4_address
      IMPORTING
        address TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_url IMPLEMENTATION.


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

    DATA components TYPE ty_url_components.

    IF url IS INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'No URL'.
    ENDIF.

    " Remove leading/trailing spaces
    DATA(remaining) = condense( url ).
    DATA(authority) = ``.

    " Parse scheme
    DATA(delimiter) = find( val = remaining sub = ':' ).
    IF delimiter < 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid URL: no scheme found'.
    ENDIF.

    components-scheme = to_lower( remaining(delimiter) ).

    validate_scheme( components-scheme ).
    components-is_special = is_special_scheme( components-scheme ).

    " Remove scheme and ':' from remaining string
    delimiter = delimiter + 1.
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

      " Split off fragment
      delimiter = find( val = authority sub = '#' ).
      IF delimiter >= 0.
        authority = authority(delimiter).
        remaining = authority+delimiter.
      ENDIF.

      " Parse authority section
      parse_authority(
        EXPORTING
          authority = authority
          scheme    = components-scheme
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
    CASE 0.
      WHEN query_pos.
        " URL starts with ?
        components-path = ''.
        remaining = remaining+1.

        " Find fragment after query
        fragment_pos = find( val = remaining sub = '#' ).
        IF fragment_pos >= 0.
          components-query = remaining(fragment_pos).
          fragment_pos = fragment_pos + 1.
          components-fragment = remaining+fragment_pos.
        ELSE.
          components-query = remaining.
        ENDIF.
      WHEN fragment_pos.
        " URL starts with #
        components-path = ''.
        fragment_pos = fragment_pos + 1.
        components-fragment = remaining+1.
      WHEN OTHERS.
        " Normal case - extract path
        IF query_pos > 0 AND ( fragment_pos < 0 OR query_pos < fragment_pos ).
          " Path ends with ?
          components-path = remaining(query_pos).
          query_pos = query_pos + 1.
          IF fragment_pos > query_pos.
            DATA(query_len) = fragment_pos - query_pos.
            components-query = remaining+query_pos(query_len).
            fragment_pos = fragment_pos + 1.
            components-fragment = remaining+fragment_pos.
          ELSE.
            components-query = remaining+query_pos.
          ENDIF.
        ELSEIF fragment_pos > 0.
          " Path ends with #
          components-path = remaining(fragment_pos).
          fragment_pos = fragment_pos + 1.
          components-fragment = remaining+fragment_pos.
        ELSE.
          " Only path
          components-path = remaining.
        ENDIF.
    ENDCASE.

    components-path     = percent_decode( normalize_path( components-path ) ).
    components-query    = percent_decode( components-query ).
    components-fragment = percent_decode( components-fragment ).

    result = NEW /apmg/cl_apm_url( components ).

  ENDMETHOD.


  METHOD parse_authority.

    DATA(temp) = authority.

    " Parse username and password
    DATA(delimiter) = find( val = temp sub = '@' ).
    IF delimiter >= 0.
      DATA(credentials) = temp(delimiter).
      delimiter = delimiter + 1.
      temp = temp+delimiter.

      delimiter = find( val = credentials sub = ':' ).
      IF delimiter >= 0.
        username = percent_decode( |{ credentials(delimiter) }| ).
        delimiter = delimiter + 1.
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
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: missing closing bracket'.
      ENDIF.

      " Extract IPv6 address without brackets
      DATA(host_len) = delimiter - 1.
      host = temp+1(host_len).

      " Check if there's a port after the IPv6 address
      delimiter = delimiter + 1.
      IF strlen( temp ) > delimiter AND temp+delimiter(1) = ':'.
        delimiter = delimiter + 1.
        port = temp+delimiter.
      ENDIF.
    ELSE.
      " Regular hostname or IPv4
      delimiter = find( val = temp sub = ':' ).
      IF delimiter >= 0.
        host = temp(delimiter).
        delimiter = delimiter + 1.
        port = temp+delimiter.
      ELSE.
        host = temp.
      ENDIF.

      " TODO: punycode
    ENDIF.

    " Validate port if present
    IF port IS NOT INITIAL.
      IF NOT matches( val = port regex = '^\d+$' ).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid port number'.
      ENDIF.
      IF port NOT BETWEEN 0 AND 65535.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Port number out of range'.
      ENDIF.
    ENDIF.

    " Validate host
    IF is_special_scheme( scheme ).
      IF host IS INITIAL.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Missing host'.
      ENDIF.
    ELSE.
      IF host CA | \n\t\r#/:<>?@[\\]^\||.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Host contain invalid code point'.
      ENDIF.
    ENDIF.

    " Validate IPv4 or IPv6 address if present
    IF temp IS NOT INITIAL AND temp(1) = '['.
      validate_ipv6_address( host ).
    ELSEIF host CO '0123456789. '.
      validate_ipv4_address( host ).
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
      idx = idx + 1.
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

    DATA(url) = |{ components-scheme }:|.

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

    result = url.

  ENDMETHOD.


  METHOD validate_ipv4_address.

    IF address IS NOT INITIAL AND address(1) = '.'.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv4 address: initial segment is empty'.
    ENDIF.

    DATA(len) = strlen( address ) - 1.
    IF len >= 0 AND address+len(1) = '.'.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv4 address: last segment is empty'.
    ENDIF.

    " Split by period
    SPLIT address AT '.' INTO TABLE DATA(parts).

    " Basic validation of IPv4 format
    IF lines( parts ) <> 4.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv4 address: not four segments'.
    ENDIF.

    " Check each part
    LOOP AT parts INTO DATA(part).
      IF NOT matches( val = part regex = '^\d+$' ).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv4 address: non-numeric segment'.
      ENDIF.
      IF part NOT BETWEEN 0 AND 255.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv4 address: segment exceeds 255'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_ipv6_address.

    IF address(1) = ':'.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: initial piece is empty'.
    ENDIF.

    DATA(len) = strlen( address ) - 1.
    IF len >= 0 AND address+len(1) = ':'.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: last piece is empty'.
    ENDIF.

    " Split by colons
    SPLIT address AT ':' INTO TABLE DATA(parts).

    " Basic validation of IPv6 format
    IF lines( parts ) > 8.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: too many pieces'.
    ENDIF.

    " Uncompressed addresses must have 8 parts
    IF address NS '::' AND lines( parts ) <> 8.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: too few pieces'.
    ENDIF.

    " Check each part
    DATA(count) = 0.
    LOOP AT parts INTO DATA(part).
      " Empty part is allowed for :: notation, but only once
      IF part IS INITIAL.
        count = count + 1.
        IF count > 1.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: multiple empty pieces'.
        ENDIF.
        CONTINUE.
      ENDIF.

      " Validate hexadecimal format and length
      IF NOT matches( val = part regex = '^[0-9A-Fa-f]{1,4}$' ).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: invalid hexadecimal piece'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_scheme.

    IF NOT matches( val = scheme regex = '^[A-Za-z][-A-Za-z0-9+.]*' ).
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid scheme'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
