CLASS zcl_abappm_semver_sap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Semantic Version for SAP Release Mapping
*
* Copyright (c) apm.to <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    METHODS sap_release_to_semver
      IMPORTING
        release       TYPE cvers-release
        support_pack  TYPE cvers-extrelease OPTIONAL
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_abap_invalid_value.

    METHODS sap_component_to_semver
      IMPORTING
        component     TYPE cvers-component
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_abap_invalid_value.

    METHODS semver_to_sap_release
      IMPORTING
        version       TYPE string
      RETURNING
        VALUE(result) TYPE cvers-release
      RAISING
        cx_abap_invalid_value.

    METHODS semver_to_sap_release_sp
      IMPORTING
        version      TYPE string
      EXPORTING
        release      TYPE cvers-release
        support_pack TYPE cvers-extrelease
      RAISING
        cx_abap_invalid_value.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abappm_semver_sap IMPLEMENTATION.


  METHOD sap_component_to_semver.

    DATA cvers TYPE cvers.

    SELECT SINGLE * FROM cvers INTO cvers WHERE component = component.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_abap_invalid_value
        EXPORTING
          value = |{ component }|.
    ENDIF.

    result = sap_release_to_semver( release = cvers-release support_pack = cvers-extrelease ).

  ENDMETHOD.


  METHOD sap_release_to_semver.

    " 750 > 7.50.x
    FIND REGEX '^(\d)(\d\d)\s*$' IN release SUBMATCHES DATA(ma) DATA(mi).
    IF sy-subrc <> 0.
      " 75E > 7.50.50x
      FIND REGEX '^(\d)(\d)([A-Z])\s*$' IN release SUBMATCHES ma mi DATA(pa).
      IF sy-subrc = 0.
        mi = mi * 10.
        pa = ( find( val = sy-abcde sub = pa ) + 1 ) * 100. " a=100, b=200, ...
      ELSE.
        " 1809 > 18.9.x
        FIND REGEX '^([1-9]\d)(\d\d)\s*$' IN release SUBMATCHES ma mi.
        IF sy-subrc = 0.
          pa = 0.
        ELSE.
          " 2011_1_731 > 2011.1.731
          FIND REGEX '^20\d+_\d+_(\d)(\d\d)\s*$' IN release SUBMATCHES ma mi.
          IF sy-subrc <> 0.
            " ST-A/PI: 01V_731 > 7.3.1
            FIND REGEX '^01._(\d)(\d\d)\s*$' IN release SUBMATCHES ma mi.
            IF sy-subrc <> 0.
              " unknown pattern... open GitHub issue
              RAISE EXCEPTION TYPE cx_abap_invalid_value
                EXPORTING
                  value = release && '/' && support_pack.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(int_ma) = CONV i( condense( ma ) ).
    DATA(int_mi) = CONV i( condense( mi ) ).
    DATA(int_pa) = CONV i( condense( pa ) ).

    IF support_pack IS NOT INITIAL AND support_pack CO ' 0123456789'.
      int_pa = int_pa + CONV i( condense( support_pack ) ).
    ENDIF.

    result = condense( |{ int_ma }.{ int_mi }.{ int_pa }| ).

  ENDMETHOD.


  METHOD semver_to_sap_release.

    FIND REGEX '^(\d)\.(\d{1,2})\.(\d+)' IN version SUBMATCHES DATA(ma) DATA(mi) DATA(pa).
    IF sy-subrc = 0.
      IF mi < 10.
        mi = '0' && mi.
      ENDIF.
      IF pa < 100.
        pa = ''.
      ELSE.
        " 75E <- 7.50.500
        mi = mi(1).
        pa = substring( val = sy-abcde off = ( pa DIV 100 - 1 ) len = 1 ). " 1xx=a, 2xx=b, ...
      ENDIF.
    ELSE.
      " 1809 <- 18.9.0
      FIND REGEX '^([1-9]\d)\.(\d{1,2})\.(\d)' IN version SUBMATCHES ma mi pa.
      IF sy-subrc = 0.
        IF mi < 10.
          mi = '0' && mi.
        ENDIF.
        pa = ''.
      ELSE.
        " unknown pattern...
        RAISE EXCEPTION TYPE cx_abap_invalid_value
          EXPORTING
            value = version.
      ENDIF.
    ENDIF.

    result = |{ ma }{ mi }{ pa }|.

  ENDMETHOD.


  METHOD semver_to_sap_release_sp.

    DATA sp TYPE i.

    release = semver_to_sap_release( version ).

    FIND REGEX '^(\d+)\.(\d+)\.(\d+)' IN version SUBMATCHES DATA(ma) DATA(mi) DATA(pa).
    IF sy-subrc = 0 AND pa > 100.
      pa = substring( val = pa off = strlen( pa ) - 2 ).
    ENDIF.

    support_pack = |{ pa ALIGN = RIGHT WIDTH = 10 PAD = '0' }|.

  ENDMETHOD.
ENDCLASS.
