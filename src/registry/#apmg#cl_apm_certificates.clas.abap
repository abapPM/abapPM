CLASS /apmg/cl_apm_certificates DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Certificates
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS setup.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_sslc   TYPE psecontext VALUE 'SSLC',
      c_anonym TYPE ssfappl VALUE 'ANONYM'.

    CLASS-METHODS get_certificate_ca
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_strust=>ty_certificate.

    CLASS-METHODS get_certificate_registry
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_strust=>ty_certificate.

    CLASS-METHODS get_certificate_playground
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_strust=>ty_certificate.

ENDCLASS.



CLASS /apmg/cl_apm_certificates IMPLEMENTATION.


  METHOD get_certificate_ca.

    " Subject       CN=WE1, O=Google Trust Services, C=US
    " Issuer        CN=GTS Root R4, O=Google Trust Services LLC, C=US
    " Serial Number 7F:F3:19:77:97:2C:22:4A:76:15:5D:13:B6:D6:85:E3
    " Valid From    2023-12-13 09:00:00 to     2029-02-20 14:00:00

    result = VALUE #(
      ( '-----BEGIN CERTIFICATE-----' )
      ( 'MIICnzCCAiWgAwIBAgIQf/MZd5csIkp2FV0TttaF4zAKBggqhkjOPQQDAzBHMQsw' )
      ( 'CQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZpY2VzIExMQzEU' )
      ( 'MBIGA1UEAxMLR1RTIFJvb3QgUjQwHhcNMjMxMjEzMDkwMDAwWhcNMjkwMjIwMTQw' )
      ( 'MDAwWjA7MQswCQYDVQQGEwJVUzEeMBwGA1UEChMVR29vZ2xlIFRydXN0IFNlcnZp' )
      ( 'Y2VzMQwwCgYDVQQDEwNXRTEwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAARvzTr+' )
      ( 'Z1dHTCEDhUDCR127WEcPQMFcF4XGGTfn1XzthkubgdnXGhOlCgP4mMTG6J7/EFmP' )
      ( 'LCaY9eYmJbsPAvpWo4H+MIH7MA4GA1UdDwEB/wQEAwIBhjAdBgNVHSUEFjAUBggr' )
      ( 'BgEFBQcDAQYIKwYBBQUHAwIwEgYDVR0TAQH/BAgwBgEB/wIBADAdBgNVHQ4EFgQU' )
      ( 'kHeSNWfE/6jMqeZ72YB5e8yT+TgwHwYDVR0jBBgwFoAUgEzW63T/STaj1dj8tT7F' )
      ( 'avCUHYwwNAYIKwYBBQUHAQEEKDAmMCQGCCsGAQUFBzAChhhodHRwOi8vaS5wa2ku' )
      ( 'Z29vZy9yNC5jcnQwKwYDVR0fBCQwIjAgoB6gHIYaaHR0cDovL2MucGtpLmdvb2cv' )
      ( 'ci9yNC5jcmwwEwYDVR0gBAwwCjAIBgZngQwBAgEwCgYIKoZIzj0EAwMDaAAwZQIx' )
      ( 'AOcCq1HW90OVznX+0RGU1cxAQXomvtgM8zItPZCuFQ8jSBJSjz5keROv9aYsAm5V' )
      ( 'sQIwJonMaAFi54mrfhfoFNZEfuNMSQ6/bIBiNLiyoX46FohQvKeIoJ99cx7sUkFN' )
      ( '7uJW' )
      ( '-----END CERTIFICATE-----' ) ).

  ENDMETHOD.


  METHOD get_certificate_playground.

    " Subject       CN=playground.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number 92:80:B6:16:2C:31:32:ED:0D:C1:A7:0B:54:08:F0:A4
    " Valid From    2026-05-04 00:59:08 to     2026-08-02 01:58:57

    result = VALUE #(
      ( '-----BEGIN CERTIFICATE-----' )
      ( 'MIIDrDCCA1GgAwIBAgIRAJKAthYsMTLtDcGnC1QI8KQwCgYIKoZIzj0EAwIwOzEL' )
      ( 'MAkGA1UEBhMCVVMxHjAcBgNVBAoTFUdvb2dsZSBUcnVzdCBTZXJ2aWNlczEMMAoG' )
      ( 'A1UEAxMDV0UxMB4XDTI2MDUwNDAwNTkwOFoXDTI2MDgwMjAxNTg1N1owIDEeMBwG' )
      ( 'A1UEAxMVcGxheWdyb3VuZC5hYmFwcG0uY29tMFkwEwYHKoZIzj0CAQYIKoZIzj0D' )
      ( 'AQcDQgAE2/CJf8KhhFRGqqWErZS5PeT863zl/LLtQfgxSGdQeIZwnMLFXhKTgv7x' )
      ( 'evfxPH2/p7DBgYShZlBomaXYhbV96aOCAk8wggJLMA4GA1UdDwEB/wQEAwIHgDAT' )
      ( 'BgNVHSUEDDAKBggrBgEFBQcDATAMBgNVHRMBAf8EAjAAMB0GA1UdDgQWBBS9X5k+' )
      ( 'jeHMJsnziTMRlaLs9IpvQTAfBgNVHSMEGDAWgBSQd5I1Z8T/qMyp5nvZgHl7zJP5' )
      ( 'ODBeBggrBgEFBQcBAQRSMFAwJwYIKwYBBQUHMAGGG2h0dHA6Ly9vLnBraS5nb29n' )
      ( 'L3Mvd2UxL2tvQTAlBggrBgEFBQcwAoYZaHR0cDovL2kucGtpLmdvb2cvd2UxLmNy' )
      ( 'dDAgBgNVHREEGTAXghVwbGF5Z3JvdW5kLmFiYXBwbS5jb20wEwYDVR0gBAwwCjAI' )
      ( 'BgZngQwBAgEwNgYDVR0fBC8wLTAroCmgJ4YlaHR0cDovL2MucGtpLmdvb2cvd2Ux' )
      ( 'Ly1BNFFJeGVCdEhJLmNybDCCAQUGCisGAQQB1nkCBAIEgfYEgfMA8QB2ANdtfRDR' )
      ( 'p/V3wsfpX9cAv/mCyTNaZeHQswFzF8DIxWl3AAABnfC181oAAAQDAEcwRQIhAIno' )
      ( 'pfd/AgeVocBoGoIRBr7BGgxnZ4+VtnqghojknjO+AiADXUQcFs3tNWs0Yd8y9PFo' )
      ( 'fByhEmy+jBIjNLFUOutZtwB3AMijxH/Hs625NWsBP2p6Em3jOk5DpcZG+ZetOXWZ' )
      ( 'Hc+aAAABnfC1828AAAQDAEgwRgIhAJRId1bGjSMCVIU3hyoIox78cD7b+F53eUIN' )
      ( 'rDSz5IhzAiEA/z0gaNuUBENMzC7jWx8ki2l3xkamW2Q15fcYDGfVsdQwCgYIKoZI' )
      ( 'zj0EAwIDSQAwRgIhALORzJ6uiz+VINsbnCJoUzpZTwNT78rIl0Fri0LuWEFPAiEA' )
      ( 'mxJAdT4oRl1HNB2VUrZiXD/sIhoUwg+vQO7Uuz3I1Og=' )
      ( '-----END CERTIFICATE-----' ) ).

  ENDMETHOD.


  METHOD get_certificate_registry.

    " Subject       CN=registry.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number 5B:50:77:60:12:7A:4B:DE:13:E7:33:FB:6B:3C:CA:04
    " Valid From    2026-06-18 14:52:15 to     2026-09-16 15:52:14

    result = VALUE #(
      ( '-----BEGIN CERTIFICATE-----' )
      ( 'MIIDezCCAyKgAwIBAgIQW1B3YBJ6S94T5zP7azzKBDAKBggqhkjOPQQDAjA7MQsw' )
      ( 'CQYDVQQGEwJVUzEeMBwGA1UEChMVR29vZ2xlIFRydXN0IFNlcnZpY2VzMQwwCgYD' )
      ( 'VQQDEwNXRTEwHhcNMjYwNjE4MTQ1MjE1WhcNMjYwOTE2MTU1MjE0WjAeMRwwGgYD' )
      ( 'VQQDExNyZWdpc3RyeS5hYmFwcG0uY29tMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcD' )
      ( 'QgAE2FqPiAsCCrxQvD8Bmj52VLXrTs42wI8oL+GkWV3qE8aAYuhKf+XlQp5dqNVu' )
      ( 'TcojvT1bUbhr2qm09ENhUW45gaOCAiMwggIfMA4GA1UdDwEB/wQEAwIHgDATBgNV' )
      ( 'HSUEDDAKBggrBgEFBQcDATAMBgNVHRMBAf8EAjAAMB0GA1UdDgQWBBQxO6s4vsXK' )
      ( '1KLx4/BWlDtBA0DhbDAfBgNVHSMEGDAWgBSQd5I1Z8T/qMyp5nvZgHl7zJP5ODA1' )
      ( 'BggrBgEFBQcBAQQpMCcwJQYIKwYBBQUHMAKGGWh0dHA6Ly9pLnBraS5nb29nL3dl' )
      ( 'MS5jcnQwHgYDVR0RBBcwFYITcmVnaXN0cnkuYWJhcHBtLmNvbTATBgNVHSAEDDAK' )
      ( 'MAgGBmeBDAECATA2BgNVHR8ELzAtMCugKaAnhiVodHRwOi8vYy5wa2kuZ29vZy93' )
      ( 'ZTEvWmpjSHY4WmkyR0EuY3JsMIIBBAYKKwYBBAHWeQIEAgSB9QSB8gDwAHYA2AlV' )
      ( 'O5RPev/IFhlvlE+Fq7D4/F6HVSYPFdEucrtFSxQAAAGe227b4wAABAMARzBFAiEA' )
      ( 'tWQagbn/kEhqK+VMIdQU6iLdUVh+tbbNSnxGXsih0tcCIDJPzxV6U2qIrZ5Bu7Bq' )
      ( 'ivQFdlK4T5IyCSPayWbT529AAHYAwjF+V0UZo0XufzjespBB68fCIVoiv3/Vta12' )
      ( 'mtkOUs0AAAGe227buwAABAMARzBFAiEAvV6RziUFpZSTw6FVaD0wedLH308SmlqT' )
      ( 'WuD/TMj54QYCIDdjuTfMsikbyGuN0yXlLORHDzmgoV1/PE1iPb9nGgKUMAoGCCqG' )
      ( 'SM49BAMCA0cAMEQCIHwEc9wzctQOuTlVIKUH0vcI9QgwmqdGlNvfowUZLuZVAiBr' )
      ( '+rWvF4Xfi9s5PbmFq5m1MhRnSRfQaLUmJsT1uwd/Gg==' )
      ( '-----END CERTIFICATE-----' ) ).

  ENDMETHOD.


  METHOD setup.

    TRY.
        DATA(strust) = NEW /apmg/cl_apm_strust(
          context     = c_sslc
          application = c_anonym ).

        strust->load( create = abap_true ).
        strust->get_own_certificate( ).
        strust->get_certificate_list( ).

        " Root and apm certificates
        strust->add( get_certificate_ca( ) ).
        strust->add( get_certificate_registry( ) ).
        strust->add( get_certificate_playground( ) ).

        strust->update( ).

      CATCH /apmg/cx_apm_error INTO DATA(error).
        MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
