CLASS zcl_abappm_semver_sap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS sap_release_to_semver
      IMPORTING
        release       TYPE cvers-release
        support_pack  TYPE cvers-extrelease OPTIONAL
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


  METHOD sap_release_to_semver.

    " 750 > 7.5.0
    FIND REGEX '^(\d)(\d)(\d)\s*$' IN release SUBMATCHES DATA(ma) DATA(mi) DATA(pa).
    IF sy-subrc <> 0.
      " 75E > 7.5.15
      FIND REGEX '^(\d)(\d)([A-Z])\s*$' IN release SUBMATCHES ma mi pa.
      IF sy-subrc = 0.
        pa = find( val = sy-abcde sub = pa ) + 10. " a=10, b=11, ...
      ELSE.
        " 1809 > 18.9.0
        FIND REGEX '^([1-9]\d)(\d\d)\s*$' IN release SUBMATCHES ma mi.
        IF sy-subrc = 0.
          pa = 0.
        ELSE.
          " 2011_1_731 > 2011.1.731
          FIND REGEX '^(\d+)_(\d+)_(\d+)\s*$' IN release SUBMATCHES ma mi pa.
          IF sy-subrc <> 0.
            " ST-A/PI: 01V_731 > 7.3.1
            FIND REGEX '^01._(\d)(\d)(\d)\s*$' IN release SUBMATCHES ma mi pa.
            IF sy-subrc <> 0.
              " unknown pattern... open GitHub issue
              RAISE EXCEPTION TYPE cx_abap_invalid_value.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(int_ma) = CONV i( condense( ma ) ).
    DATA(int_mi) = CONV i( condense( mi ) ).
    DATA(int_pa) = CONV i( condense( pa ) ).
    result = |{ int_ma }.{ int_mi }.{ int_pa }|.

    IF support_pack IS NOT INITIAL.
      " 750 SP 2 > 7.5.0-sp.2
      IF support_pack CO ' 0123456789'.
        result &&= |-sp.{ support_pack ALPHA = OUT }|.
      ELSE.
        result &&= |-sp.{ support_pack }|.
      ENDIF.
    ENDIF.

    result = condense( result ).

  ENDMETHOD.


  METHOD semver_to_sap_release.

    FIND REGEX '^(\d)\.(\d)\.(\d+)' IN version SUBMATCHES DATA(ma) DATA(mi) DATA(pa).
    IF sy-subrc = 0.
      IF pa BETWEEN 0 AND 9.
        " 750 <- 7.5.0
      ELSEIF pa BETWEEN 10 AND 36.
        " 75E <- 7.5.13
        pa = substring( val = sy-abcde off = pa - 10 len = 1 ). " 10=a, 11=b, ...
      ELSE.
        " unknown pattern...
        RAISE EXCEPTION TYPE cx_abap_invalid_value.
      ENDIF.
    ELSE.
      " 1809 <- 18.9.0
      FIND REGEX '^([1-9]\d)\.(\d{1,2})\.(0)' IN version SUBMATCHES ma mi pa.
      IF sy-subrc = 0.
        IF mi < 10.
          mi = '0' && mi.
        ENDIF.
        pa = ''.
      ELSE.
        " 2011_1_731 <- 2011.1.731
        FIND REGEX '^(\d+)\.(\d+)\.(\d+)' IN version SUBMATCHES ma mi pa.
        IF sy-subrc = 0.
          ma &&= '_'.
          mi &&= '_'.
        ELSE.
          " unknown pattern...
          RAISE EXCEPTION TYPE cx_abap_invalid_value.
        ENDIF.
      ENDIF.
    ENDIF.

    result = |{ ma }{ mi }{ pa }|.

  ENDMETHOD.


  METHOD semver_to_sap_release_sp.

    DATA sp TYPE i.

    release = semver_to_sap_release( version ).

    IF version CS '-'.
      SPLIT version AT '-' INTO DATA(rest) DATA(pre).

      IF pre CP 'sp.*'.
        SPLIT pre AT '.' INTO rest pre.
      ENDIF.

      IF pre CO ' 0123456789'.
        sp = pre.
      ENDIF.
    ENDIF.

    support_pack = |{ sp ALIGN = RIGHT WIDTH = 10 PAD = '0' }|.

  ENDMETHOD.
ENDCLASS.
