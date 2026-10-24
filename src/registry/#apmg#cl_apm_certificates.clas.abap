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
      ( '-----BEGIN CERTIFICATE-----' )
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
      ( '-----END CERTIFICATE-----' ) ).

  ENDMETHOD.


  METHOD get_certificate_playground.

    " Subject       CN=playground.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number 53C46EE8ED5300D11382977A19A96C19
    " Validity      20260830030715Z - 20261128040712Z

    result = VALUE #(
      ( '-----BEGIN CERTIFICATE-----' )
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
      ( '-----END CERTIFICATE-----' ) ).

  ENDMETHOD.


  METHOD get_certificate_registry.

    " Subject       CN=registry.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number B0779B3DF1ED37E40ED4E90AFB51FE55
    " Validity      20261014205952Z - 20270112215950Z

    result = VALUE #(
      ( '-----BEGIN CERTIFICATE-----' )
      ( 'MIIDhTCCAyugAwIBAgIRALB3mz3x7TfkDtTpCvtR/lUwCgYIKoZIzj0EAwIwOzELMAkGA1UEBhMCVVMx' )
      ( 'HjAcBgNVBAoTFUdvb2dsZSBUcnVzdCBTZXJ2aWNlczEMMAoGA1UEAxMDV0UxMB4XDTI2MTAxNDIwNTk1' )
      ( 'MloXDTI3MDExMjIxNTk1MFowHjEcMBoGA1UEAxMTcmVnaXN0cnkuYWJhcHBtLmNvbTBZMBMGByqGSM49' )
      ( 'AgEGCCqGSM49AwEHA0IABEyONa3jX58L8vQLeG48GtpTQyBK/VUdtsLbGnLhNNfOat/03DCljUpf+pEc' )
      ( 'JyhO/v9Dr/SAvAN8AvXC0PTueY+jggIrMIICJzAOBgNVHQ8BAf8EBAMCB4AwEwYDVR0lBAwwCgYIKwYB' )
      ( 'BQUHAwEwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQUn7aEP1ujoI/Qcd0fQYrSmbtdSp8wHwYDVR0jBBgw' )
      ( 'FoAUkHeSNWfE/6jMqeZ72YB5e8yT+TgwNQYIKwYBBQUHAQEEKTAnMCUGCCsGAQUFBzAChhlodHRwOi8v' )
      ( 'aS5wa2kuZ29vZy93ZTEuY3J0MB4GA1UdEQQXMBWCE3JlZ2lzdHJ5LmFiYXBwbS5jb20wEwYDVR0gBAww' )
      ( 'CjAIBgZngQwBAgEwNgYDVR0fBC8wLTAroCmgJ4YlaHR0cDovL2MucGtpLmdvb2cvd2UxL3Y0YXNwTTRz' )
      ( 'UHhvLmNybDCCAQwGCisGAQQB1nkCBAIEgf0EgfoA+AB2AETCvQzpFA5kpclKAZMKWqG7NZcOAO4RFolo' )
      ( 'KhxE17VmAAABoTxt1OUAAAQDAEcwRQIgUEEhGwv41VyHfdimPS0rUAws3TzoVcRRUog0EFr18d4CIQDF' )
      ( 'jUz+6WieGKoTYHTpFFYfGlVx644/I6JY0O0Nf8ZuOQB+AI7KRwus3mrzogawpHqEt0b+H8a/lT4l5ptO' )
      ( '5AJI88boAAABoTxt1nwACAAABQAj//cIBAMARzBFAiEA5SQx/imRRl2BU2WTPxu8cJcwmTEXBFqEk8Gi' )
      ( 'EduKKyoCICMWxJ4qeYlVjWFHofkfCnAk/sXFHZ5d6SMiRBJdlCPhMAoGCCqGSM49BAMCA0gAMEUCICC9' )
      ( 'HEbO64Lso7z+TkIkuaUCi5HNloyKJgvhBBPS1028AiEA16ov2EB0eUenKM4cqxzUrGVnDl5x7/Lk+ltp' )
      ( '7aN3YHI=' )
      ( '-----END CERTIFICATE-----' ) ).

  ENDMETHOD.


  METHOD get_certificate_tools.

    " Subject       CN=tools.abappm.com
    " Issuer        CN=WE1, O=Google Trust Services, C=US
    " Serial Number DD43D2AFE41AD8B00ED9750D51FB7390
    " Validity      20260926133556Z - 20261225143550Z

    result = VALUE #(
      ( '-----BEGIN CERTIFICATE-----' )
      ( 'MIIDdjCCAxygAwIBAgIRAN1D0q/kGtiwDtl1DVH7c5AwCgYIKoZIzj0EAwIwOzELMAkGA1UEBhMCVVMx' )
      ( 'HjAcBgNVBAoTFUdvb2dsZSBUcnVzdCBTZXJ2aWNlczEMMAoGA1UEAxMDV0UxMB4XDTI2MDkyNjEzMzU1' )
      ( 'NloXDTI2MTIyNTE0MzU1MFowGzEZMBcGA1UEAxMQdG9vbHMuYWJhcHBtLmNvbTBZMBMGByqGSM49AgEG' )
      ( 'CCqGSM49AwEHA0IABPXSxtrDXTqM3tCqC6ASDUygapvMYpWfAVwJ5Tsa9GBNZJqMJdvMrvOiSsybbuqJ' )
      ( 'kaCIJgJfHRxGcek14OosOvijggIfMIICGzAOBgNVHQ8BAf8EBAMCB4AwEwYDVR0lBAwwCgYIKwYBBQUH' )
      ( 'AwEwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQUznymSR+iWs4+57AbqZAZI/ij92EwHwYDVR0jBBgwFoAU' )
      ( 'kHeSNWfE/6jMqeZ72YB5e8yT+TgwNQYIKwYBBQUHAQEEKTAnMCUGCCsGAQUFBzAChhlodHRwOi8vaS5w' )
      ( 'a2kuZ29vZy93ZTEuY3J0MBsGA1UdEQQUMBKCEHRvb2xzLmFiYXBwbS5jb20wEwYDVR0gBAwwCjAIBgZn' )
      ( 'gQwBAgEwNgYDVR0fBC8wLTAroCmgJ4YlaHR0cDovL2MucGtpLmdvb2cvd2UxL3M5S2RCMUVHOHUwLmNy' )
      ( 'bDCCAQMGCisGAQQB1nkCBAIEgfQEgfEA7wB1AK9niDtXsE7dj6bZfvYuqOuBCsdxYPAkXlXWDC/nhYc6' )
      ( 'AAABoN4k71cAAAQDAEYwRAIgJD3msoWkc+p0KqTJ7zOZ+Qm5zBfCPVKNDYZYi7nom/cCIDv0K/Dw8Lvi' )
      ( 'pzsZjpeo7tCSBovCcdNEVMG/ImCTkFWvAHYA1219ENGn9XfCx+lf1wC/+YLJM1pl4dCzAXMXwMjFaXcA' )
      ( 'AAGg3iTucwAABAMARzBFAiBueUxyJXuz6AIILAoOQj61fXq211SfLsoXN4TV92AAngIhAOWWxggxzn4B' )
      ( '+jjukGlO38fOy53NTHyyYm3irqlYKJsWMAoGCCqGSM49BAMCA0gAMEUCIHO6jBuwkKr4RwgIOzKrhkX2' )
      ( 'xrXj2rQIcb6BJ8Ei2WEOAiEA4xmDWNM805iDYiXsCZBi0vO0VvPXjMgJ7aG3yD7Jlhc=' )
      ( '-----END CERTIFICATE-----' ) ).

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
