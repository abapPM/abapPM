CLASS ltcl_semver_re DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO ZCL_ABAPPM_SEMVER_RE.

    METHODS:
      test_src FOR TESTING,
      test_regex FOR TESTING,
      test_occ FOR TESTING.

ENDCLASS.

CLASS ZCL_ABAPPM_SEMVER_RE DEFINITION LOCAL FRIENDS ltcl_semver_re.

CLASS ltcl_semver_re IMPLEMENTATION.

  METHOD test_src.
    " has a list of src

    DATA i TYPE i.

    FIELD-SYMBOLS <token> TYPE ZCL_ABAPPM_SEMVER_RE=>TY_TOKEN.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE ZCL_ABAPPM_SEMVER_RE=>TOKEN TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      cl_abap_unit_assert=>assert_not_initial(
        act = <token>-src
        msg = |Regex component #{ i } must not be initial| ).

      cl_abap_unit_assert=>assert_equals(
        act = <token>-src
        exp = <token>-regex->pattern
        msg = |Regex component #{ i } does not match source| ).

      cl_abap_unit_assert=>assert_equals(
        act = <token>-safe_src
        exp = <token>-safe_regex->pattern
        msg = |Regex component #{ i } does not match source| ).

      IF <token>-safe_src CS '\s+'.
        cl_abap_unit_assert=>fail( msg = |Regex component #{ i } must not contain greedy whitespace| ).
      ENDIF.

      IF <token>-safe_src CS '\s*'.
        cl_abap_unit_assert=>fail( msg = |Regex component #{ i } must not contain greedy whitespace| ).
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD test_regex.
    " has a list of valid regex

    DATA i TYPE i.
    DATA regex TYPE REF TO cl_abap_regex.

    FIELD-SYMBOLS <token> TYPE ZCL_ABAPPM_SEMVER_RE=>TY_TOKEN.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE ZCL_ABAPPM_SEMVER_RE=>TOKEN TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      TRY.
          regex = <token>-regex.
          DATA(matcher) = regex->create_matcher( text = '1.2.3' ).
        CATCH cx_root.
          cl_abap_unit_assert=>fail(
            msg = |Error processing regex component #{ i }| ).
      ENDTRY.
    ENDDO.

  ENDMETHOD.

  METHOD test_occ.
    " either 0 or 1

    DATA i TYPE i.

    FIELD-SYMBOLS <token> TYPE ZCL_ABAPPM_SEMVER_RE=>TY_TOKEN.

    DO.
      i += 1.
      ASSIGN COMPONENT i OF STRUCTURE ZCL_ABAPPM_SEMVER_RE=>TOKEN TO <token>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      cl_abap_unit_assert=>assert_number_between(
        number = <token>-occ
        lower  = 0
        upper  = 1
        msg    = |Occurence of component #{ i } must be either 0 or 1| ).
    ENDDO.

  ENDMETHOD.

ENDCLASS.
