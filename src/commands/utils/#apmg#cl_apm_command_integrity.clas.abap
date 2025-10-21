CLASS /apmg/cl_apm_command_integrity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Command Integrity Checks
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: This is a stateless class. Do not add any attributes!
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS check_integrity
      IMPORTING
        !tarball TYPE xstring
        !dist    TYPE /apmg/if_apm_types=>ty_dist
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS get_integrity
      IMPORTING
        !tarball      TYPE xstring
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_dist
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_initial_key TYPE xstring VALUE ''.

    CLASS-METHODS calc_sha1
      IMPORTING
        !data         TYPE xstring
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS calc_sha512
      IMPORTING
        !data         TYPE xstring
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_integrity IMPLEMENTATION.


  METHOD calc_sha1.

    TRY.
        " Simple Shasum
        cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
            if_algorithm  = 'SHA1'
            if_key        = c_initial_key
            if_data       = data
          IMPORTING
            ef_hmacstring = DATA(sha1) ).

        result = to_lower( sha1 ).

      CATCH cx_abap_message_digest INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD calc_sha512.

    " Integrity Checksum (sha512)
    " https://www.npmjs.com/package/ssri
    " Note: It's not clear which ABAP kernel version is required for this

    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
            if_algorithm     = 'SHA512'
            if_key           = c_initial_key
            if_data          = data
          IMPORTING
            ef_hmacb64string = DATA(sha512) ).

        result = |sha512-{ sha512 }|.

      CATCH cx_abap_message_digest INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD check_integrity.

    DATA(shasum) = calc_sha1( tarball ).

    IF shasum <> dist-shasum.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Checksum error for tarball (sha1)'.
    ENDIF.

    DATA(integrity) = calc_sha512( tarball ).

    IF integrity <> dist-integrity.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Checksum error for tarball (sha512)'.
    ENDIF.

  ENDMETHOD.


  METHOD get_integrity.

    result = VALUE #(
      shasum    = calc_sha1( tarball )
      integrity = calc_sha512( tarball ) ).

  ENDMETHOD.
ENDCLASS.
