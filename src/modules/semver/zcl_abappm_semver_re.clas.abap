CLASS zcl_abappm_semver_re DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer Regex
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.
* TODO: Migrate POSIX to PCRE
* https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenregex_posix_pcre_incompat.htm

    TYPES:
      BEGIN OF ty_token,
        src        TYPE string,
        regex      TYPE REF TO cl_abap_regex,
        safe_src   TYPE string,
        safe_regex TYPE REF TO cl_abap_regex,
        occ        TYPE i, " 0 = global, 1 = once
      END OF ty_token.

    CONSTANTS:
      letter_dash_number      TYPE string VALUE '[a-zA-Z0-9-]',
      caret_trim_replace      TYPE string VALUE '$1^',
      tilde_trim_replace      TYPE string VALUE '$1~',
      comparator_trim_replace TYPE string VALUE '$1$2$3',
      version_trim_replace    TYPE string VALUE '$1$2'. " added for POSIX

    CLASS-METHODS class_constructor.

    CLASS-DATA:
      BEGIN OF token,
        v                         TYPE ty_token, " added for POSIX
        vtrim                     TYPE ty_token, " added for POSIX
        build                     TYPE ty_token,
        buildidentifier           TYPE ty_token,
        caret                     TYPE ty_token,
        caretloose                TYPE ty_token,
        carettrim                 TYPE ty_token,
        coerce                    TYPE ty_token,
        coercefull                TYPE ty_token,
        coerceplain               TYPE ty_token,
        coercertl                 TYPE ty_token,
        coercertlfull             TYPE ty_token,
        comparator                TYPE ty_token,
        comparatorloose           TYPE ty_token,
        comparatortrim            TYPE ty_token,
        full                      TYPE ty_token,
        fullplain                 TYPE ty_token,
        gte0                      TYPE ty_token,
        gte0pre                   TYPE ty_token,
        gtlt                      TYPE ty_token,
        hyphenrange               TYPE ty_token,
        hyphenrangeloose          TYPE ty_token,
        lonecaret                 TYPE ty_token,
        lonetilde                 TYPE ty_token,
        loose                     TYPE ty_token,
        looseplain                TYPE ty_token,
        mainversion               TYPE ty_token,
        mainversionloose          TYPE ty_token,
        nonnumericidentifier      TYPE ty_token,
        numericidentifier         TYPE ty_token,
        numericidentifierloose    TYPE ty_token,
        prerelease                TYPE ty_token,
        prereleaseidentifier      TYPE ty_token,
        prereleaseidentifierloose TYPE ty_token,
        prereleaseloose           TYPE ty_token,
        star                      TYPE ty_token,
        tilde                     TYPE ty_token,
        tildeloose                TYPE ty_token,
        tildetrim                 TYPE ty_token,
        xrange                    TYPE ty_token,
        xrangeidentifier          TYPE ty_token,
        xrangeidentifierloose     TYPE ty_token,
        xrangeloose               TYPE ty_token,
        xrangeplain               TYPE ty_token,
        xrangeplainloose          TYPE ty_token,
      END OF token.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS create_token
      IMPORTING
        name   TYPE string
        value  TYPE string
        global TYPE abap_bool DEFAULT abap_false.

    CLASS-METHODS make_safe_regex
      IMPORTING
        value         TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_abappm_semver_re IMPLEMENTATION.


  METHOD class_constructor.

    " The following Regular Expressions can be used for tokenizing,
    " validating, and parsing SemVer version strings.

    " ## POSIX: Additional logic required since POSIX regex is greedy
    " The whitespace after "v" or "=" will be trimmed separately
    create_token(
      name  = 'V'
      value = |[v=]*| ).
    " value = |[v=\\s]*| ) " PCRE

    create_token(
      name  = 'VTRIM'
      value = |([v=]+)\\s+(\\d)|
      global = abap_true ).

    " ## Numeric Identifier
    " A single `0`, or a non-zero digit followed by zero or more digits.

    create_token(
      name  = 'NUMERICIDENTIFIER'
      value = |0\|[1-9]\\d*| ).
    create_token(
      name  = 'NUMERICIDENTIFIERLOOSE'
      value = |\\d+| ).

    " ## Non-numeric Identifier
    " Zero or more digits, followed by a letter or hyphen, and then zero or
    " more letters, digits, or hyphens.

    create_token(
      name  = 'NONNUMERICIDENTIFIER'
      value = |\\d*[a-zA-Z-]{ letter_dash_number }*| ).

    " ## Main Version
    " Three dot-separated numeric identifiers.

    create_token(
      name  = 'MAINVERSION'
      value = |({ token-numericidentifier-src })\\.| &&
              |({ token-numericidentifier-src })\\.| &&
              |({ token-numericidentifier-src })| ).

    create_token(
      name  = 'MAINVERSIONLOOSE'
      value = |({ token-numericidentifierloose-src })\\.| &&
              |({ token-numericidentifierloose-src })\\.| &&
              |({ token-numericidentifierloose-src })| ).

    " ## Pre-release Version Identifier
    " A numeric identifier, or a non-numeric identifier.

    create_token(
      name  = 'PRERELEASEIDENTIFIER'
      value = |(?:{ token-numericidentifier-src }\|| &&
              |{ token-nonnumericidentifier-src })| ).

    create_token(
      name  = 'PRERELEASEIDENTIFIERLOOSE'
      value = |(?:{ token-numericidentifierloose-src }\|| &&
              |{ token-nonnumericidentifier-src })| ).

    " ## Pre-release Version
    " Hyphen, followed by one or more dot-separated pre-release version
    " identifiers.

    create_token(
      name  = 'PRERELEASE'
      value = |(?:-({ token-prereleaseidentifier-src }| &&
              |(?:\\.{ token-prereleaseidentifier-src })*))| ).

    create_token(
      name  = 'PRERELEASELOOSE'
      value = |(?:-?({ token-prereleaseidentifierloose-src }| &&
              |(?:\\.{ token-prereleaseidentifierloose-src })*))| ).

    " ## Build Metadata Identifier
    " Any combination of digits, letters, or hyphens.

    create_token(
      name  = 'BUILDIDENTIFIER'
      value = |{ letter_dash_number }+| ).

    " ## Build Metadata
    " Plus sign, followed by one or more period-separated build metadata
    " identifiers.

    create_token(
      name  = 'BUILD'
      value = |(?:\\+({ token-buildidentifier-src }| &&
              |(?:\\.{ token-buildidentifier-src })*))| ).

    " ## Full Version String
    " A main version, followed optionally by a pre-release version and
    " build metadata.

    " Note that the only major, minor, patch, and pre-release sections of
    " the version string are capturing groups.  The build metadata is not a
    " capturing group, because it should not ever be used in version
    " comparison.

    create_token(
      name  = 'FULLPLAIN'
      value = |v?{ token-mainversion-src }{ token-prerelease-src }?{ token-build-src }?| ).

    create_token(
      name  = 'FULL'
      value = |^{ token-fullplain-src }$| ).

    " like full, but allows v1.2.3 and =1.2.3, which people do sometimes.
    " also, 1.0.0alpha1 (prerelease without the hyphen) which is pretty
    " common in the npm registry.

    create_token(
      name  = 'LOOSEPLAIN'
      value = |{ token-v-src }{ token-mainversionloose-src }{ token-prereleaseloose-src }?{ token-build-src }?| ).

    create_token(
      name  = 'LOOSE'
      value = |^{ token-looseplain-src }$| ).

    create_token(
      name  = 'GTLT'
      value = |((?:<\|>)?=?)| ).

    " Something like "2.*" or "1.2.x".
    " Note that "x.x" is a valid xRange identifer, meaning "any version"
    " Only the first item is strictly required.

    create_token(
      name  = 'XRANGEIDENTIFIERLOOSE'
      value = |{ token-numericidentifierloose-src }\|x\|X\|\\*| ).
    create_token(
      name  = 'XRANGEIDENTIFIER'
      value = |{ token-numericidentifier-src }\|x\|X\|\\*| ).

    create_token(
      name  = 'XRANGEPLAIN'
      value = |{ token-v-src }({ token-xrangeidentifier-src })| &&
              |(?:\\.({ token-xrangeidentifier-src })| &&
              |(?:\\.({ token-xrangeidentifier-src })| &&
              |(?:{ token-prerelease-src })?| &&
              |{ token-build-src }?| &&
              |)?)?| ).

    create_token(
      name  = 'XRANGEPLAINLOOSE'
      value = |{ token-v-src }({ token-xrangeidentifierloose-src })| &&
              |(?:\\.({ token-xrangeidentifierloose-src })| &&
              |(?:\\.({ token-xrangeidentifierloose-src })| &&
              |(?:{ token-prereleaseloose-src })?| &&
              |{ token-build-src }?| &&
              |)?)?| ).

    create_token(
      name  = 'XRANGE'
      value = |^{ token-gtlt-src }\\s*{ token-xrangeplain-src }$| ).
    create_token(
      name  = 'XRANGELOOSE'
      value = |^{ token-gtlt-src }\\s*{ token-xrangeplainloose-src }$| ).

    " Coercion.
    " Extract anything that could conceivably be a part of a valid semver

    create_token(
      name  = 'COERCEPLAIN'
      value = |(^\|[^\\d])(\\d\{1,{ zif_abappm_semver_constants=>max_safe_component_length }\})| &&
              |(?:\\.(\\d\{1,{ zif_abappm_semver_constants=>max_safe_component_length }\}))?| &&
              |(?:\\.(\\d\{1,{ zif_abappm_semver_constants=>max_safe_component_length }\}))?| ).
    create_token(
      name  = 'COERCE'
      value = |{ token-coerceplain-src }(?:$\|[^\\d])| ).
    create_token(
      name  = 'COERCEFULL'
      value = |{ token-coerceplain-src }| &&
              |(?:{ token-prerelease-src })?| &&
              |(?:{ token-build-src })?| &&
              |(?:$\|[^\\d])| ).
    create_token(
      name  = 'COERCERTL'
      value = token-coerce-src
      global = abap_true ).
    create_token(
      name  = 'COERCERTLFULL'
      value = token-coercefull-src
      global = abap_true ).

    " Tilde ranges.
    " Meaning is "reasonably at or greater than"

    create_token(
      name  = 'LONETILDE'
      value = |(?:~>?)| ).

    create_token(
      name  = 'TILDETRIM'
      value = |(\\s*){ token-lonetilde-src }\\s+|
      global = abap_true ).

    create_token(
      name  = 'TILDE'
      value = |^{ token-lonetilde-src }{ token-xrangeplain-src }$| ).
    create_token(
      name  = 'TILDELOOSE'
      value = |^{ token-lonetilde-src }{ token-xrangeplainloose-src }$| ).

    " Caret ranges.
    " Meaning is "at least and backwards compatible with"

    create_token(
      name  = 'LONECARET'
      value = |(?:\\^)| ).

    create_token(
      name  = 'CARETTRIM'
      value = |(\\s*){ token-lonecaret-src }\\s+|
      global = abap_true ).

    create_token(
      name  = 'CARET'
      value = |^{ token-lonecaret-src }{ token-xrangeplain-src }$| ).
    create_token(
      name  = 'CARETLOOSE'
      value = |^{ token-lonecaret-src }{ token-xrangeplainloose-src }$| ).

    " A simple gt/lt/eq thing, or just "" to indicate "any version"

    create_token(
      name  = 'COMPARATORLOOSE'
      value = |^{ token-gtlt-src }\\s*({ token-looseplain-src })$\|^$| ).
    create_token(
      name  = 'COMPARATOR'
      value = |^{ token-gtlt-src }\\s*({ token-fullplain-src })$\|^$| ).

    " An expression to strip any whitespace between the gtlt and the thing
    " it modifies, so that `> 1.2.3` ==> `>1.2.3`

    create_token(
      name  = 'COMPARATORTRIM'
      value = |(\\s*){ token-gtlt-src }\\s*({ token-looseplain-src }\|| &&
              |{ token-xrangeplain-src })|
      global = abap_true ).

    " Something like `1.2.3 - 1.2.4`
    " Note that these all use the loose form, because they'll be
    " checked against either the strict or loose comparator form
    " later.

    create_token(
      name  = 'HYPHENRANGE'
      value = |^\\s*({ token-xrangeplain-src })| &&
              |\\s+-\\s+| &&
              |({ token-xrangeplain-src })| &&
              |\\s*$| ).

    create_token(
      name  = 'HYPHENRANGELOOSE'
      value = |^\\s*({ token-xrangeplainloose-src })| &&
              |\\s+-\\s+| &&
              |({ token-xrangeplainloose-src })| &&
              |\\s*$| ).

    " Star ranges basically just allow anything at all.

    create_token(
      name  = 'STAR'
      value = |(<\|>)?=?\\s*\\*| ).

    " >=0.0.0 is like a star

    create_token(
      name  = 'GTE0'
      value = |^\\s*>=\\s*0\\.0\\.0\\s*$| ).
    create_token(
      name  = 'GTE0PRE'
      value = |^\\s*>=\\s*0\\.0\\.0-0\\s*$| ).

  ENDMETHOD.


  METHOD create_token.

    FIELD-SYMBOLS <token> TYPE ty_token.

    ASSIGN COMPONENT name OF STRUCTURE token TO <token>.
    ASSERT sy-subrc = 0.

    <token>-src        = value.
    <token>-regex      = NEW cl_abap_regex( pattern = value ).
    <token>-safe_src   = make_safe_regex( value ).
    <token>-safe_regex = NEW cl_abap_regex( pattern = make_safe_regex( value ) ).
    <token>-occ        = COND #( WHEN global = abap_true THEN 0 ELSE 1 ).

  ENDMETHOD.


  METHOD make_safe_regex.

    TYPES:
      BEGIN OF ty_regex_replacement,
        token TYPE string,
        max   TYPE i,
      END OF ty_regex_replacement,
      ty_regex_replacements TYPE STANDARD TABLE OF ty_regex_replacement WITH KEY token max.

    " Replace some greedy regex tokens to prevent regex dos issues. These regex are
    " used internally via the safeRe object since all inputs in this library get
    " normalized first to trim and collapse all extra whitespace. The original
    " regexes are exported for userland consumption and lower level usage. A
    " future breaking change could export the safer regex only with a note that
    " all input should have extra whitespace removed.

    DATA(safe_regex_replacements) = VALUE ty_regex_replacements(
      ( token = `\s`               max = 1 )
      ( token = `\d`               max = zif_abappm_semver_constants=>max_length )
      ( token = letter_dash_number max = zif_abappm_semver_constants=>max_safe_build_length ) ).

    result = value.

    LOOP AT safe_regex_replacements INTO DATA(safe_regex_replacement).
      result = replace(
        val  = result
        sub  = |{ safe_regex_replacement-token }*|
        with = |{ safe_regex_replacement-token }\{0,{ safe_regex_replacement-max }\}|
        occ  = 0 ).
      result = replace(
        val  = result
        sub  = |{ safe_regex_replacement-token }+|
        with = |{ safe_regex_replacement-token }\{1,{ safe_regex_replacement-max }\}|
        occ  = 0 ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
