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
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.1.0' ##NEEDED.

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

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_codepoints TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    CLASS-METHODS unescape_host
      IMPORTING
        raw           TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS domain_to_ascii
      IMPORTING
        domain        TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS unicode_codepoints
      IMPORTING
        input         TYPE string
      RETURNING
        VALUE(result) TYPE ty_codepoints
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS punycode_encode
      IMPORTING
        label         TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS punycode_delta
      IMPORTING
        delta         TYPE i
        bias          TYPE i
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS punycode_adapt
      IMPORTING
        delta         TYPE i
        count         TYPE i
        first         TYPE abap_bool
      RETURNING
        VALUE(result) TYPE i.

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


  METHOD domain_to_ascii.

    CHECK domain IS NOT INITIAL.

    " Punycode encoding, not full UTS #46 normalization or IDNA validation.
    DATA(domain_name) = to_lower( domain ).

    " Replace unicode dots
    DATA(dot1) = cl_abap_conv_in_ce=>uccpi( 12290 ).
    DATA(dot2) = cl_abap_conv_in_ce=>uccpi( 65294 ).
    DATA(dot3) = cl_abap_conv_in_ce=>uccpi( 65377 ).
    REPLACE ALL OCCURRENCES OF dot1 IN domain_name WITH '.'.
    REPLACE ALL OCCURRENCES OF dot2 IN domain_name WITH '.'.
    REPLACE ALL OCCURRENCES OF dot3 IN domain_name WITH '.'.

    IF domain_name CA | #%/:<>?@[\\]^\||.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Host contains invalid code point'.
    ENDIF.

    " Assemble by index so empty labels and a trailing root dot are preserved.
    SPLIT domain_name AT '.' INTO TABLE DATA(labels).
    DATA(last) = strlen( domain_name ) - 1.
    IF domain_name+last(1) = '.'.
      APPEND `` TO labels.
    ENDIF.
    LOOP AT labels INTO DATA(label).
      IF sy-tabix > 1.
        result = |{ result }.|.
      ENDIF.
      result = |{ result }{ punycode_encode( label ) }|.
    ENDLOOP.

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
      IF components-is_special = abap_true.
        delimiter = find( val = remaining regex = '[/?#\\]' ) ##REGEX_POSIX.
      ELSE.
        delimiter = find( val = remaining regex = '[/?#]' ) ##REGEX_POSIX.
      ENDIF.
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

    IF components-is_special = abap_true.
      REPLACE ALL OCCURRENCES OF '\' IN components-path WITH '/'.
    ENDIF.
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

      IF is_special_scheme( scheme ).
        " Decode UTF-8 escapes before converting domain labels. Keep literal '+'.
        host = unescape_host( host ).
        host = domain_to_ascii( host ).
      ENDIF.
    ENDIF.

    " Validate port if present
    IF port IS NOT INITIAL.
      IF NOT matches( val = port regex = '^\d+$' ) ##REGEX_POSIX.
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


  METHOD punycode_adapt.

    " RFC 3492 section 6.1: base=36, tmin=1, tmax=26, skew=38, damp=700.
    DATA(adjusted) = delta.

    IF first = abap_true.
      adjusted = adjusted DIV 700.
    ELSE.
      adjusted = adjusted DIV 2.
    ENDIF.

    adjusted = adjusted + adjusted DIV count.

    WHILE adjusted > 455.
      adjusted = adjusted DIV 35.
      result = result + 36.
    ENDWHILE.

    result = result + ( 36 * adjusted ) DIV ( adjusted + 38 ).

  ENDMETHOD.


  METHOD punycode_delta.

    CONSTANTS digits TYPE string VALUE 'abcdefghijklmnopqrstuvwxyz0123456789'.

    DATA(remainder) = delta.
    DATA(weight) = 36.
    DATA(threshold) = nmin( val1 = 26 val2 = nmax( val1 = 1 val2 = weight - bias ) ).

    WHILE remainder >= threshold.
      DATA(digit) = threshold + ( remainder - threshold ) MOD ( 36 - threshold ).
      result = |{ result }{ digits+digit(1) }|.
      remainder = ( remainder - threshold ) DIV ( 36 - threshold ).
      weight = weight + 36.
      threshold = nmin( val1 = 26 val2 = nmax( val1 = 1 val2 = weight - bias ) ).
    ENDWHILE.

    result = |{ result }{ digits+remainder(1) }|.

  ENDMETHOD.


  METHOD punycode_encode.

    " RFC 3492 section 6.3, with explicit signed 32-bit overflow checks.
    CONSTANTS max_integer TYPE i VALUE 2147483647.

    DATA(points) = unicode_codepoints( label ).
    DATA(count) = lines( points ).

    LOOP AT points INTO DATA(point) WHERE table_line < 128.
      result = |{ result }{ cl_abap_conv_in_ce=>uccpi( point ) }|.
    ENDLOOP.

    DATA(basic) = strlen( result ).
    DATA(handled) = basic.

    IF handled = count.
      RETURN.
    ENDIF.

    IF basic > 0.
      result = |{ result }-|.
    ENDIF.

    DATA(next_point) = 128.
    DATA(delta) = 0.
    DATA(bias) = 72.

    WHILE handled < count.
      DATA(minimum) = 1114112.

      LOOP AT points INTO point WHERE table_line >= next_point.
        minimum = nmin( val1 = minimum val2 = point ).
      ENDLOOP.

      IF minimum - next_point > ( max_integer - delta ) DIV ( handled + 1 ).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Punycode overflow'.
      ENDIF.

      delta = delta + ( minimum - next_point ) * ( handled + 1 ).
      next_point = minimum.

      LOOP AT points INTO point.
        IF point < next_point.
          IF delta = max_integer.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Punycode overflow'.
          ENDIF.
          delta = delta + 1.
        ELSEIF point = next_point.
          result = |{ result }{ punycode_delta( delta = delta bias = bias ) }|.
          bias = punycode_adapt( delta = delta count = handled + 1 first = xsdbool( handled = basic ) ).
          delta = 0.
          handled = handled + 1.
        ENDIF.
      ENDLOOP.

      delta = delta + 1.
      next_point = next_point + 1.
    ENDWHILE.

    result = |xn--{ result }|.

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
      DATA(host) = components-host.
      IF is_special_scheme( components-scheme ) AND host NS ':'.
        host = domain_to_ascii( host ).
      ENDIF.
      url = |{ url }{ host }|.
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


  METHOD unescape_host.

    CONSTANTS hex_digits TYPE string VALUE '0123456789ABCDEF'.

    TYPES ty_x TYPE x LENGTH 1.
    DATA bytes TYPE xstring.

    IF raw NS '%'.
      result = raw.
      RETURN.
    ENDIF.

    DATA(offset) = 0.
    DATA(length) = strlen( raw ).

    WHILE offset < length.
      IF raw+offset(1) <> '%'.
        result = result && raw+offset(1).
        offset = offset + 1.
        CONTINUE.
      ENDIF.

      CLEAR bytes.
      WHILE offset < length AND raw+offset(1) = '%'.
        IF offset + 2 >= length.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid percent escape in host'.
        ENDIF.
        DATA(start) = offset + 1.
        DATA(pair) = to_upper( raw+start(2) ).
        IF pair CN hex_digits.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid percent escape in host'.
        ENDIF.
        DATA(octet) = CONV ty_x( pair(2) ).
        CONCATENATE bytes octet INTO bytes IN BYTE MODE.
        offset = offset + 3.
      ENDWHILE.

      TRY.
          DATA(converter) = cl_abap_conv_in_ce=>create(
            encoding    = 'UTF-8'
            input       = bytes
            ignore_cerr = abap_false ).
          DATA(decoded) = ``.
          converter->read( IMPORTING data = decoded ).
          result = result && decoded.
        CATCH cx_root.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid UTF-8 escape in host'.
      ENDTRY.
    ENDWHILE.

  ENDMETHOD.


  METHOD unicode_codepoints.

    DATA(character) = space.
    DATA(length) = strlen( input ).
    DATA(offset) = 0.

    WHILE offset < length.
      character = input+offset(1).
      DATA(point) = cl_abap_conv_out_ce=>uccpi( character ).
      offset = offset + 1.

      " ABAP strings use UTF-16; combine surrogate pairs before Bootstring.
      IF point BETWEEN 55296 AND 56319.
        IF offset >= length.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid Unicode in host'.
        ENDIF.
        character = input+offset(1).
        DATA(low) = cl_abap_conv_out_ce=>uccpi( character ).
        IF low NOT BETWEEN 56320 AND 57343.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid Unicode in host'.
        ENDIF.
        point = 65536 + ( point - 55296 ) * 1024 + low - 56320.
        offset = offset + 1.
      ELSEIF point BETWEEN 56320 AND 57343.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid Unicode in host'.
      ENDIF.

      IF point <= 32 OR point = 127.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Host contains invalid code point'.
      ENDIF.

      APPEND point TO result.
    ENDWHILE.

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
      IF NOT matches( val = part regex = '^\d+$' ) ##REGEX_POSIX.
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
      IF NOT matches( val = part regex = '^[0-9A-Fa-f]{1,4}$' ) ##REGEX_POSIX.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid IPv6 address: invalid hexadecimal piece'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_scheme.

    IF NOT matches( val = scheme regex = '^[A-Za-z][-A-Za-z0-9+.]*' ) ##REGEX_POSIX.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Invalid scheme'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
