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

    CLASS-METHODS get_certificate_tools
      RETURNING
        VALUE(result) TYPE /apmg/cl_apm_strust=>ty_certificate.

ENDCLASS.



CLASS /apmg/cl_apm_certificates IMPLEMENTATION.


  METHOD get_certificate_ca.

     " Subject       CN=WE1, O=Google Trust Services, C=US
    " Issuer        CN=GTS Root R4, O=Google Trust Services LLC, C=US
    " Serial Number 7FF31977972C224A76155D13B6D685E3
    " Validity      20231213090000Z - 20290220140000Z

    result = VALUE #(
      ( '	BEGIN CERTIFICATE	' )
      ( 'MIICnzCCAiWgAwIBAgIQf/MZd5csIkp2FV0TttaF4zAKBggqhkjOPQQDAzBHMQswCQYDVQQGEwJVUzEi' )
      ( 'MCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZpY2VzIExMQzEUMBIGA1UEAxMLR1RTIFJvb3QgUjQwHhcN' )
      ( 'MjMxMjEzMDkwMDAwWhcNMjkwMjIwMTQwMDAwWjA7MQswCQYDVQQGEwJVUzEeMBwGA1UEChMVR29vZ2xl' )
      ( 'IFRydXN0IFNlcnZpY2VzMQwwCgYDVQQDEwNXRTEwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAARvzTr+' )
      ( 'Z1dHTCEDhUDCR127WEcPQMFcF4XGGTfn1XzthkubgdnXGhOlCgP4mMTG6J7/EFmPLCaY9eYmJbsPAvpW' )
      ( 'o4H+MIH7MA4GA1UdDwEB/wQEAwIBhjAdBgNVHSUEFjAUBggrBgEFBQcDAQYIKwYBBQUHAwIwEgYDVR0T' )
      ( 'AQH/BAgwBgEB/wIBADAdBgNVHQ4EFgQUkHeSNWfE/6jMqeZ72YB5e8yT+TgwHwYDVR0jBBgwFoAUgEzW' )
      ( '63T/STaj1dj8tT7FavCUHYwwNAYIKwYBBQUHAQEEKDAmMCQGCCsGAQUFBzAChhhodHRwOi8vaS5wa2ku' )
      ( 'Z29vZy9yNC5jcnQwKwYDVR0fBCQwIjAgoB6gHIYaaHR0cDovL2MucGtpLmdvb2cvci9yNC5jcmwwEwYD' )
      ( 'VR0gBAwwCjAIBgZngQwBAgEwCgYIKoZIzj0EAwMDaAAwZQIxAOcCq1HW90OVznX+0RGU1cxAQXomvtgM' )
      ( '8zItPZCuFQ8jSBJSjz5keROv9aYsAm5VsQIwJonMaAFi54mrfhfoFNZEfuNMSQ6/bIBiNLiyoX46FohQ' )
      ( 'vKeIoJ99cx7sUkFN7uJW' )
      ( '	END CERTIFICATE	' ) ).

  ENDMETHOD.


  METHOD get_certificate_playground.

    " Subject       CN=playground.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number 53C46EE8ED5300D11382977A19A96C19
    " Validity      20260830030715Z - 20261128040712Z

    result = VALUE #(
      ( '	BEGIN CERTIFICATE	' )
      ( 'MIIDgTCCAyagAwIBAgIQU8Ru6O1TANETgpd6GalsGTAKBggqhkjOPQQDAjA7MQswCQYDVQQGEwJVUzEe' )
      ( 'MBwGA1UEChMVR29vZ2xlIFRydXN0IFNlcnZpY2VzMQwwCgYDVQQDEwNXRTEwHhcNMjYwODMwMDMwNzE1' )
      ( 'WhcNMjYxMTI4MDQwNzEyWjAgMR4wHAYDVQQDExVwbGF5Z3JvdW5kLmFiYXBwbS5jb20wWTATBgcqhkjO' )
      ( 'PQIBBggqhkjOPQMBBwNCAASW/SIEwA+GtHbVE5P4GWPv99mPtnfi9e3JMYfuXywIArCXgHE67Nd7crBe' )
      ( 'Bige99knDVGBLs6vQEdPd+wkVQ/Wo4ICJTCCAiEwDgYDVR0PAQH/BAQDAgeAMBMGA1UdJQQMMAoGCCsG' )
      ( 'AQUFBwMBMAwGA1UdEwEB/wQCMAAwHQYDVR0OBBYEFNgrsW2ojk5/OfS0gJeLNQfZkhPdMB8GA1UdIwQY' )
      ( 'MBaAFJB3kjVnxP+ozKnme9mAeXvMk/k4MDUGCCsGAQUFBwEBBCkwJzAlBggrBgEFBQcwAoYZaHR0cDov' )
      ( 'L2kucGtpLmdvb2cvd2UxLmNydDAgBgNVHREEGTAXghVwbGF5Z3JvdW5kLmFiYXBwbS5jb20wEwYDVR0g' )
      ( 'BAwwCjAIBgZngQwBAgEwNgYDVR0fBC8wLTAroCmgJ4YlaHR0cDovL2MucGtpLmdvb2cvd2UxL05FOXQt' )
      ( 'NVExbGhNLmNybDCCAQQGCisGAQQB1nkCBAIEgfUEgfIA8AB1ANdtfRDRp/V3wsfpX9cAv/mCyTNaZeHQ' )
      ( 'swFzF8DIxWl3AAABoFDZpn4AAAQDAEYwRAIgWLuphlLo+iTzF8V7mp732g7wOJCqE5CV9LsPrs5++84C' )
      ( 'IFRv67O08Ww8Z1bR9/mbbRLpdOG8TXFLvKqDQ0528zwIAHcAwjF+V0UZo0XufzjespBB68fCIVoiv3/V' )
      ( 'ta12mtkOUs0AAAGgUNmmmwAABAMASDBGAiEAkyfiBO6ASfUE+3kBDMsdFFcK6EpVddSgSB7PQMHQMsIC' )
      ( 'IQCrH7cBnDwt9sv3LpC0028zxhyklP0wn8toTGHOGDHCqDAKBggqhkjOPQQDAgNJADBGAiEA9NjUxcDH' )
      ( 'h1NOKDbmw+5FZV6WZ0SL2T9njE6CCv5XCE4CIQCRprLEioTS1HpWeih4gF2/F6d/MqCyJP645FZWSWUK' )
      ( 'Og==' )
      ( '	END CERTIFICATE	' ) ).

  ENDMETHOD.


  METHOD get_certificate_registry.

    " Subject       CN=registry.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number AB97F2B4E1055F88139024E6508C0849
    " Validity      20260816181140Z - 20261114191134Z

    result = VALUE #(
      ( '	BEGIN CERTIFICATE	' )
      ( 'MIIDfDCCAyOgAwIBAgIRAKuX8rThBV+IE5Ak5lCMCEkwCgYIKoZIzj0EAwIwOzELMAkGA1UEBhMCVVMx' )
      ( 'HjAcBgNVBAoTFUdvb2dsZSBUcnVzdCBTZXJ2aWNlczEMMAoGA1UEAxMDV0UxMB4XDTI2MDgxNjE4MTE0' )
      ( 'MFoXDTI2MTExNDE5MTEzNFowHjEcMBoGA1UEAxMTcmVnaXN0cnkuYWJhcHBtLmNvbTBZMBMGByqGSM49' )
      ( 'AgEGCCqGSM49AwEHA0IABEUEfdfgRppgvxel30+eV08TtRBp5F7VinqSIj/wjhFYwKT2rARbSBpr5kSx' )
      ( '28wZn6OTK+WMqVdEDVr4lgvnSBijggIjMIICHzAOBgNVHQ8BAf8EBAMCB4AwEwYDVR0lBAwwCgYIKwYB' )
      ( 'BQUHAwEwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQUfxjLWe2aGU6JV8EefIr4v3hM2wswHwYDVR0jBBgw' )
      ( 'FoAUkHeSNWfE/6jMqeZ72YB5e8yT+TgwNQYIKwYBBQUHAQEEKTAnMCUGCCsGAQUFBzAChhlodHRwOi8v' )
      ( 'aS5wa2kuZ29vZy93ZTEuY3J0MB4GA1UdEQQXMBWCE3JlZ2lzdHJ5LmFiYXBwbS5jb20wEwYDVR0gBAww' )
      ( 'CjAIBgZngQwBAgEwNgYDVR0fBC8wLTAroCmgJ4YlaHR0cDovL2MucGtpLmdvb2cvd2UxL1hXUGt5UVh2' )
      ( 'SG1RLmNybDCCAQQGCisGAQQB1nkCBAIEgfUEgfIA8AB2ANgJVTuUT3r/yBYZb5RPhauw+Pxeh1UmDxXR' )
      ( 'LnK7RUsUAAABoAv8oMkAAAQDAEcwRQIhAKCxt4r0HoZkKsCkgxAO5ZmQzyXz0C9FNEjL8FXptgb6AiB/' )
      ( '08mqY1fWmQWvDHdbNoYKZlthAYFauZ0Dk7PR/suNAgB2AJROQ4f67MHvgfMZJCaoGGUBx9NfOAIBP3Jn' )
      ( 'fVU3LhnYAAABoAv8oJYAAAQDAEcwRQIhAPwOekbwBcKuvmbs+CTA50ykgESlAk8u6ZpBK8bwDOZnAiAU' )
      ( 'vt1WSmJzr8/e6mVj2QzcO6U9rWYLOUX62LRVgNLx4zAKBggqhkjOPQQDAgNHADBEAiB3yhWA7dm8I2pk' )
      ( '/xsn5jAKe7Zwk2LXrblEI+1Rhp273gIgZs4tb+Dz3BfG29SWD6oXmHCgcHnLP+EmZPBx38BQuYo=' )
      ( '	END CERTIFICATE	' ) ).

  ENDMETHOD.


  METHOD get_certificate_tools.

    " Subject       CN=tools.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number 4A349FC9FB95B0E00E14D1D4928D3283
    " Validity      20260729120328Z - 20261027130323Z

    result = VALUE #(
      ( '	BEGIN CERTIFICATE	' )
      ( 'MIIDdzCCAxygAwIBAgIQSjSfyfuVsOAOFNHUko0ygzAKBggqhkjOPQQDAjA7MQswCQYDVQQGEwJVUzEe' )
      ( 'MBwGA1UEChMVR29vZ2xlIFRydXN0IFNlcnZpY2VzMQwwCgYDVQQDEwNXRTEwHhcNMjYwNzI5MTIwMzI4' )
      ( 'WhcNMjYxMDI3MTMwMzIzWjAbMRkwFwYDVQQDExB0b29scy5hYmFwcG0uY29tMFkwEwYHKoZIzj0CAQYI' )
      ( 'KoZIzj0DAQcDQgAEm/E7KXCHnYfEEr9ScngbTnsZPFXcW4Vpuh5JVCJSOqMMoILNFy8Xb6IS2cvoH/x6' )
      ( 'dA+JXV6Y6rrEJoTF/7ugVqOCAiAwggIcMA4GA1UdDwEB/wQEAwIHgDATBgNVHSUEDDAKBggrBgEFBQcD' )
      ( 'ATAMBgNVHRMBAf8EAjAAMB0GA1UdDgQWBBRmD5QV2jW1VgaTkD338oZuDknlrTAfBgNVHSMEGDAWgBSQ' )
      ( 'd5I1Z8T/qMyp5nvZgHl7zJP5ODA1BggrBgEFBQcBAQQpMCcwJQYIKwYBBQUHMAKGGWh0dHA6Ly9pLnBr' )
      ( 'aS5nb29nL3dlMS5jcnQwGwYDVR0RBBQwEoIQdG9vbHMuYWJhcHBtLmNvbTATBgNVHSAEDDAKMAgGBmeB' )
      ( 'DAECATA2BgNVHR8ELzAtMCugKaAnhiVodHRwOi8vYy5wa2kuZ29vZy93ZTEvTlNBRHRUdXJUN1kuY3Js' )
      ( 'MIIBBAYKKwYBBAHWeQIEAgSB9QSB8gDwAHYA1219ENGn9XfCx+lf1wC/+YLJM1pl4dCzAXMXwMjFaXcA' )
      ( 'AAGfrfkPUwAABAMARzBFAiB6fTOxTHKYTb4Xn7ebHsWM7zPKeW9lj0xqUABFV3BEhgIhAIYbawyewDyy' )
      ( 'V4sDK2JILkB6uaz2UNvRTXBn8YSMS203AHYAyKPEf8ezrbk1awE/anoSbeM6TkOlxkb5l605dZkdz5oA' )
      ( 'AAGfrfkQbwAABAMARzBFAiA0G82kwNkxkjd9IQDAKgDmcTWVG1ZoDtxyqFOBsWh6hAIhAOFJNPf1BgnG' )
      ( 'eQ2nt6EvzlA0gK+yts5FXChufzxQHfMpMAoGCCqGSM49BAMCA0kAMEYCIQD3ZsuAslg90RJCAt/Bozq8' )
      ( 'p4LtVH9R17kuMTtSs+WwyAIhAKca+rFr8M/KWxfvbhgIo8nRg6qJ3LrfeO7pMfk8HU01' )
      ( '	END CERTIFICATE	' ) ).

  ENDMETHOD.


  METHOD setup.

    " Note: Authorization for object S_PSE_ADM is required
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
        strust->add( get_certificate_tools( ) ).

        strust->update( ).

      CATCH /apmg/cx_apm_error INTO DATA(error).
        MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
