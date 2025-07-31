CLASS /apmg/cl_apm_semver DEFINITION
  PUBLIC
  CREATE PRIVATE.

************************************************************************
* SemVer
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    DATA:
      version    TYPE string READ-ONLY,
      major      TYPE i READ-ONLY,
      minor      TYPE i READ-ONLY,
      patch      TYPE i READ-ONLY,
      prerelease TYPE string_table READ-ONLY,
      build      TYPE string_table READ-ONLY.

    METHODS constructor
      IMPORTING
        version TYPE string
        loose   TYPE abap_bool DEFAULT abap_false
        incpre  TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS create
      IMPORTING
        version       TYPE any
        loose         TYPE abap_bool DEFAULT abap_false
        incpre        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_semver
      RAISING
        /apmg/cx_apm_error.

    METHODS format
      RETURNING
        VALUE(result) TYPE string.

    METHODS to_string
      RETURNING
        VALUE(result) TYPE string.

    METHODS compare
      IMPORTING
        other         TYPE any
      RETURNING
        VALUE(result) TYPE i
      RAISING
        /apmg/cx_apm_error.

    METHODS compare_main
      IMPORTING
        other         TYPE any
      RETURNING
        VALUE(result) TYPE i
      RAISING
        /apmg/cx_apm_error.

    METHODS compare_pre
      IMPORTING
        other         TYPE any
      RETURNING
        VALUE(result) TYPE i
      RAISING
        /apmg/cx_apm_error.

    METHODS compare_build
      IMPORTING
        other         TYPE any
      RETURNING
        VALUE(result) TYPE i
      RAISING
        /apmg/cx_apm_error.

    METHODS inc
      IMPORTING
        release_type    TYPE string
        identifier      TYPE string OPTIONAL
        identifier_base TYPE string OPTIONAL
      RETURNING
        VALUE(result)   TYPE REF TO /apmg/cl_apm_semver
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS false TYPE string VALUE 'false'.

    DATA:
      raw     TYPE string,
      options TYPE /apmg/if_apm_semver_options=>ty_options.

    METHODS _inc_check
      IMPORTING
        release_type    TYPE string
        identifier      TYPE string OPTIONAL
        identifier_base TYPE string OPTIONAL
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_semver IMPLEMENTATION.


  METHOD compare.

    DATA(semver) = create( version = other loose = options-loose incpre = options-incpre ).

    CHECK semver IS BOUND.

    IF semver->version = version.
      result = 0.
    ELSE.
      result = compare_main( semver ).
      IF result = 0.
        result = compare_pre( semver ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD compare_build.

    DATA(semver) = create( version = other loose = options-loose incpre = options-incpre ).

    DATA(i) = 1.
    DO.
      DATA(a) = VALUE #( build[ i ] DEFAULT `` ).
      DATA(b) = VALUE #( semver->build[ i ] DEFAULT `` ).
      IF a IS INITIAL AND b IS INITIAL.
        result = 0.
        RETURN.
      ELSEIF b IS INITIAL.
        result = +1.
        RETURN.
      ELSEIF a IS INITIAL.
        result = -1.
        RETURN.
      ELSEIF a <> b.
        result = /apmg/cl_apm_semver_identifier=>compare_identifiers( a = a b = b ).
        RETURN.
      ENDIF.
      i = i + 1.
    ENDDO.

  ENDMETHOD.


  METHOD compare_main.

    DATA(semver) = create( version = other loose = options-loose incpre = options-incpre ).

    CHECK semver IS BOUND.

    result = /apmg/cl_apm_semver_identifier=>compare_identifiers( a = major b = semver->major ).
    IF result = 0.
      result = /apmg/cl_apm_semver_identifier=>compare_identifiers( a = minor b = semver->minor ).
      IF result = 0.
        result = /apmg/cl_apm_semver_identifier=>compare_identifiers( a = patch b = semver->patch ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD compare_pre.

    DATA(semver) = create( version = other loose = options-loose incpre = options-incpre ).

    CHECK semver IS BOUND.

    " NOT having a prerelease is > having one
    IF prerelease IS NOT INITIAL AND semver->prerelease IS INITIAL.
      result = -1.
    ELSEIF prerelease IS INITIAL AND semver->prerelease IS NOT INITIAL.
      result = +1.
    ELSEIF prerelease IS INITIAL AND semver->prerelease IS INITIAL.
      result = 0.
    ELSE.
      DATA(i) = 1.
      DO.
        DATA(a) = VALUE #( prerelease[ i ] DEFAULT `` ).
        DATA(b) = VALUE #( semver->prerelease[ i ] DEFAULT `` ).
        IF a IS INITIAL AND b IS INITIAL.
          result = 0.
          RETURN.
        ELSEIF b IS INITIAL.
          result = +1.
          RETURN.
        ELSEIF a IS INITIAL.
          result = -1.
          RETURN.
        ELSEIF a <> b.
          result = /apmg/cl_apm_semver_identifier=>compare_identifiers( a = a b = b ).
          RETURN.
        ENDIF.
        i = i + 1.
      ENDDO.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    IF strlen( version ) > /apmg/if_apm_semver_constants=>max_length.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Version is longer than { /apmg/if_apm_semver_constants=>max_length } characters|.
    ENDIF.

    options-loose  = loose.
    options-incpre = incpre.

    DATA(r) = COND #(
      WHEN loose = abap_true
      THEN /apmg/cl_apm_semver_re=>token-loose-safe_regex
      ELSE /apmg/cl_apm_semver_re=>token-full-safe_regex ).

    TRY.
        DATA(m) = r->create_matcher( text = /apmg/cl_apm_semver_utils=>version_trim( version ) ).

        IF NOT m->match( ).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
            EXPORTING
              text = |Invalid version: { version }|.
        ENDIF.

        raw = version.

        " these are actually numbers
        DATA(major_num) = CONV decfloat34( m->get_submatch( 1 ) ).
        DATA(minor_num) = CONV decfloat34( m->get_submatch( 2 ) ).
        DATA(patch_num) = CONV decfloat34( m->get_submatch( 3 ) ).

        IF major_num BETWEEN 0 AND /apmg/if_apm_semver_constants=>max_safe_integer.
          major = major_num.
        ELSE.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
            EXPORTING
              text = |Invalid major version: { major_num }|.
        ENDIF.

        IF minor_num BETWEEN 0 AND /apmg/if_apm_semver_constants=>max_safe_integer.
          minor = minor_num.
        ELSE.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
            EXPORTING
              text = |Invalid minor version: { minor_num }|.
        ENDIF.

        IF patch_num BETWEEN 0 AND /apmg/if_apm_semver_constants=>max_safe_integer.
          patch = patch_num.
        ELSE.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
            EXPORTING
              text = |Invalid patch version: { patch_num }|.
        ENDIF.

        DATA(m4) = m->get_submatch( 4 ).
        IF m4 IS NOT INITIAL.
          SPLIT m4 AT '.' INTO TABLE prerelease.

          LOOP AT prerelease ASSIGNING FIELD-SYMBOL(<pre>).
            IF /apmg/cl_apm_semver_utils=>is_numeric( <pre> ).
              DATA(pre_num) = CONV decfloat34( <pre> ).
              <pre> = pre_num.
            ENDIF.
          ENDLOOP.
        ENDIF.

        DATA(m5) = m->get_submatch( 5 ).
        IF m5 IS NOT INITIAL.
          SPLIT m5 AT '.' INTO TABLE build.
        ENDIF.

      CATCH cx_sy_matcher.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Error evaluating regex for { version }|.
    ENDTRY.

    format( ).

  ENDMETHOD.


  METHOD create.

    DATA(descr) = cl_abap_typedescr=>describe_by_data( version ).
    DATA(kind) = descr->type_kind.

    IF kind = cl_abap_typedescr=>typekind_oref AND version IS INSTANCE OF /apmg/cl_apm_semver.

      result ?= version.

      IF result->options-loose = loose AND result->options-incpre = incpre.
        RETURN.
      ENDIF.

      result = NEW /apmg/cl_apm_semver( version = |{ result->version }| loose = loose incpre = incpre ).

    ELSEIF kind = cl_abap_typedescr=>typekind_char OR kind = cl_abap_typedescr=>typekind_string.

      result = NEW /apmg/cl_apm_semver( version = |{ version }| loose = loose incpre = incpre ).

    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Invalid version. Must be a string or a semver. Got { descr->absolute_name }|.
    ENDIF.

  ENDMETHOD.


  METHOD format.

    version = |{ major }.{ minor }.{ patch }|.

    IF prerelease IS NOT INITIAL.
      version = version && |-{ concat_lines_of( table = prerelease sep = '.' ) }|.
    ENDIF.

    version = condense( version ).

    result = version.

  ENDMETHOD.


  METHOD inc.

    DATA prerelease_tab LIKE prerelease.

    _inc_check(
      release_type    = release_type
      identifier      = identifier
      identifier_base = identifier_base ).

    CASE release_type.
      WHEN 'premajor'.
        CLEAR prerelease.
        patch = 0.
        minor = 0.
        major = major + 1.
        inc( release_type = 'pre' identifier = identifier identifier_base = identifier_base ).
      WHEN 'preminor'.
        CLEAR prerelease.
        patch = 0.
        minor = minor + 1.
        inc( release_type = 'pre' identifier = identifier identifier_base = identifier_base ).
      WHEN 'prepatch'.
        " If this is already a prerelease, it will bump to the next version
        " drop any prereleases that might already exist, since they are not
        " relevant at this point.
        CLEAR prerelease.
        inc( release_type = 'patch' identifier = identifier identifier_base = identifier_base ).
        inc( release_type = 'pre' identifier = identifier identifier_base = identifier_base ).
      WHEN 'prerelease'.
        " If the input is a non-prerelease version, this acts the same as
        " prepatch.
        IF prerelease IS INITIAL.
          inc( release_type = 'patch' identifier = identifier identifier_base = identifier_base ).
        ENDIF.
        inc( release_type = 'pre' identifier = identifier identifier_base = identifier_base ).
      WHEN 'release'.
        IF prerelease IS INITIAL.
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
            EXPORTING
              text = |Version { raw } is not a prerelease|.
        ENDIF.
        CLEAR prerelease.
      WHEN 'major'.
        " If this is a pre-major version, bump up to the same major version.
        " Otherwise increment major.
        " 1.0.0-5 bumps to 1.0.0
        " 1.1.0 bumps to 2.0.0
        IF minor <> 0 OR patch <> 0 OR prerelease IS INITIAL.
          major = major + 1.
        ENDIF.
        minor = 0.
        patch = 0.
        CLEAR prerelease.
      WHEN 'minor'.
        " If this is a pre-minor version, bump up to the same minor version.
        " Otherwise increment minor.
        " 1.2.0-5 bumps to 1.2.0
        " 1.2.1 bumps to 1.3.0
        IF patch <> 0 OR prerelease IS INITIAL.
          minor = minor + 1.
        ENDIF.
        patch = 0.
        CLEAR prerelease.
      WHEN 'patch'.
        " If this is not a pre-release version, it will increment the patch.
        " If it is a pre-release it will bump up to the same patch version.
        " 1.2.0-5 patches to 1.2.0
        " 1.2.0 patches to 1.2.1
        IF prerelease IS INITIAL.
          patch = patch + 1.
        ENDIF.
        CLEAR prerelease.
      WHEN 'pre'.
        " This probably shouldn't be used publicly.
        " 1.0.0 'pre' would become 1.0.0-0 which is the wrong direction.
        IF identifier_base IS INITIAL OR identifier_base = `0`.
          DATA(base) = `0`.
        ELSE.
          base = COND #( WHEN /apmg/cl_apm_semver_utils=>is_numeric( identifier_base ) THEN `1` ELSE `0` ).
        ENDIF.

        IF prerelease IS INITIAL.
          prerelease = VALUE #( ( base ) ).
        ELSE.
          DATA(i) = lines( prerelease ).
          WHILE i > 0.
            IF /apmg/cl_apm_semver_utils=>is_numeric( prerelease[ i ] ).
              prerelease[ i ] = prerelease[ i ] + 1.
              prerelease[ i ] = condense( prerelease[ i ] ).
              i = -2.
            ENDIF.
            i = i - 1.
          ENDWHILE.
          IF i = 0.
            " didn't increment anything
            DATA(prerelease_string) = concat_lines_of( table = prerelease sep = '.' ).
            IF identifier = prerelease_string AND identifier_base = false.
              RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
                EXPORTING
                  text = 'Invalid increment argument: identifier already exists'.
            ENDIF.

            INSERT base INTO TABLE prerelease.
          ENDIF.
        ENDIF.
        IF identifier IS NOT INITIAL.
          " 1.2.0-beta.1 bumps to 1.2.0-beta.2,
          " 1.2.0-beta.fooblz or 1.2.0-beta bumps to 1.2.0-beta.0
          prerelease_tab = VALUE #( ( identifier ) ( base ) ).
          IF identifier_base = false.
            prerelease_tab = VALUE #( ( identifier ) ).
          ENDIF.

          IF /apmg/cl_apm_semver_identifier=>compare_identifiers( a = prerelease[ 1 ] b = identifier ) = 0.
            IF NOT /apmg/cl_apm_semver_utils=>is_numeric( VALUE #( prerelease[ 2 ] DEFAULT `-` ) ).
              prerelease = prerelease_tab.
            ENDIF.
          ELSE.
            prerelease = prerelease_tab.
          ENDIF.
        ENDIF.
      WHEN 'prepush'.
        " Used by zcl_semver_ranges->min_version
        INSERT identifier_base INTO TABLE prerelease.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Invalid release type argument { release_type }|.
    ENDCASE.

    format( ).

    raw = version.

    IF build IS NOT INITIAL.
      raw = raw && |+{ concat_lines_of( table = build sep = '.' ) }|.
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD to_string.
    result = version.
  ENDMETHOD.


  METHOD _inc_check.

    IF release_type CP 'pre*'.
      IF identifier IS INITIAL AND identifier_base = false.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = 'Invalid increment argument: identifier is empty'.
      ENDIF.

      " Avoid an invalid semver results
      IF identifier IS NOT INITIAL.
        DATA(regex) = COND #(
          WHEN options-loose = abap_true
          THEN |^{ /apmg/cl_apm_semver_re=>token-prereleaseloose-safe_src }$|
          ELSE |^{ /apmg/cl_apm_semver_re=>token-prerelease-safe_src }$| ).

        TRY.
            DATA(r) = NEW cl_abap_regex( pattern = regex ).
            DATA(m) = r->create_matcher( text = |-{ identifier }| ).

            IF NOT m->match( ) OR m->get_submatch( 1 ) <> identifier.
              RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
                EXPORTING
                  text = |Invalid identifier: { identifier }|.
            ENDIF.
          CATCH cx_sy_matcher.
            RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
              EXPORTING
                text = |Error evaluating regex for { identifier }|.
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
