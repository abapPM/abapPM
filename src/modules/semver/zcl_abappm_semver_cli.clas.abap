CLASS zcl_abappm_semver_cli DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* SemVer CLI
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS main
      IMPORTING
        args          TYPE string
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      argv            TYPE string_table,
      versions        TYPE string_table,
      ranges          TYPE string_table,
      inc             TYPE string,
      identifier      TYPE string,
      identifier_base TYPE string,
      help            TYPE abap_bool,
      loose           TYPE abap_bool,
      incpre          TYPE abap_bool,
      coerce          TYPE abap_bool,
      rtl             TYPE abap_bool,
      reverse         TYPE abap_bool.

    CLASS-METHODS _argv
      IMPORTING
        args TYPE string.

    CLASS-METHODS _versions
      RAISING
        zcx_abappm_error.

    CLASS-METHODS _help
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS _success
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_semver_cli IMPLEMENTATION.


  METHOD main.

    _argv( args ).

    IF help = abap_true.
      result = _help( ).
    ELSE.
      _versions( ).

      result = _success( ).
    ENDIF.

  ENDMETHOD.


  METHOD _argv.

    CLEAR:
      argv, versions, ranges, inc, identifier, identifier_base,
      help, loose, incpre, coerce, rtl, reverse.

    IF args IS INITIAL.
      help = abap_true.
      RETURN.
    ENDIF.

    DATA(arg) = replace(
      val  = args
      sub  = '='
      with = ` `
      occ  = 0 ).

    SPLIT arg AT ` ` INTO TABLE argv.

    DATA(idx) = 1.
    DATA(val) = ``.
    DO.
      DATA(a) = VALUE #( argv[ idx ] OPTIONAL ).
      IF a IS INITIAL.
        EXIT.
      ENDIF.

      CASE a.
        WHEN '-rv' OR '-rev' OR '--rev' OR '--reverse'.
          reverse = abap_true.
        WHEN '-l' OR '--loose'.
          loose = abap_true.
        WHEN '-p' OR '--include-prerelease'.
          incpre = abap_true.
        WHEN '-v' OR '--version'.
          idx = idx + 1.
          val = VALUE #( argv[ idx ] OPTIONAL ).
          INSERT val INTO TABLE versions.
        WHEN '-i' OR '--inc' OR '--increment'.
          val = VALUE #( argv[ idx + 1 ] OPTIONAL ).
          CASE val.
            WHEN 'major' OR 'minor' OR 'patch' OR 'prerelease' OR 'premajor' OR 'preminor' OR 'prepatch'.
              idx = idx + 1.
              inc = VALUE #( argv[ idx ] OPTIONAL ).
            WHEN OTHERS.
              inc = 'patch'.
          ENDCASE.
        WHEN '--preid'.
          idx = idx + 1.
          identifier = VALUE #( argv[ idx ] OPTIONAL ).
        WHEN '-r' OR '--range'.
          idx = idx + 1.
          val = VALUE #( argv[ idx ] OPTIONAL ).
          INSERT val INTO TABLE ranges.
        WHEN '-n'.
          idx = idx + 1.
          TRY.
              identifier_base = VALUE #( argv[ idx ] OPTIONAL ).
            CATCH cx_root.
          ENDTRY.
        WHEN '-c' OR '--coerce'.
          coerce = abap_true.
        WHEN '--rtl'.
          rtl = abap_true.
        WHEN '--ltr'.
          rtl = abap_false.
        WHEN '-h' OR '--help' OR '-?'.
          help = abap_true.
        WHEN OTHERS.
          INSERT a INTO TABLE versions.
      ENDCASE.

      idx = idx + 1.
    ENDDO.

  ENDMETHOD.


  METHOD _help.

    result = VALUE #(
      ( |SemVer { zif_abappm_semver_constants=>version }| )
      ( `` )
      ( `ABAP implementation of the https://semver.org/ specification` )
      ( `Original JavaScript Copyright Isaac Z. Schlueter` )
      ( `ABAP port by Marc F. Bernard` )
      ( `` )
      ( `Usage: semver [options] <version> [<version> [...]]` )
      ( `` )
      ( `Prints valid versions sorted by SemVer precedence` )
      ( `` )
      ( `Options:` )
      ( `` )
      ( `-r --range <range>` )
      ( `        Print versions that match the specified range.` )
      ( `` )
      ( `-i --increment [<level>]` )
      ( `        Increment a version by the specified level.  Level can` )
      ( `        be one of: major, minor, patch, premajor, preminor,` )
      ( `        prepatch, or prerelease.  Default level is 'patch'.` )
      ( `        Only one version may be specified.` )
      ( `` )
      ( `--preid <identifier>` )
      ( `        Identifier to be used to prefix premajor, preminor,` )
      ( `        prepatch or prerelease version increments.` )
      ( `` )
      ( `-l --loose` )
      ( `        Interpret versions and ranges loosely` )
      ( `` )
      ( `-n <base>` )
      ( `        Base number to be used for the prerelease identifier.` )
      ( `        Can be either 0 or 1, or false to omit the number altogether.` )
      ( `        Defaults to 0.` )
      ( `` )
      ( `-p --include-prerelease` )
      ( `        Always include prerelease versions in range matching` )
      ( `` )
      ( `-c --coerce` )
      ( `        Coerce a string into SemVer if possible` )
      ( `        (does not imply --loose)` )
      ( `` )
      ( `--rtl` )
      ( `        Coerce version strings right to left` )
      ( `` )
      ( `--ltr` )
      ( `        Coerce version strings left to right (default)` )
      ( `` )
      ( `Program exits successfully if any valid version satisfies` )
      ( `all supplied ranges, and prints all satisfying versions.` )
      ( `If no satisfying versions are found, then exits failure.` )
      ( `Versions are printed in ascending order, so supplying` )
      ( `multiple versions to the utility will just sort them.` ) ).

  ENDMETHOD.


  METHOD _success.

    IF reverse = abap_true.
      versions = zcl_abappm_semver_functions=>rsort( versions ).
    ELSE.
      versions = zcl_abappm_semver_functions=>sort( versions ).
    ENDIF.

    LOOP AT versions ASSIGNING FIELD-SYMBOL(<version>).
      <version> = zcl_abappm_semver_functions=>clean( version = <version> loose = loose incpre = incpre ).
    ENDLOOP.

    IF inc IS NOT INITIAL.
      LOOP AT versions ASSIGNING <version>.
        DATA(semver) = zcl_abappm_semver_functions=>inc(
          version         = <version>
          release         = inc
          identifier      = identifier
          identifier_base = identifier_base
          loose           = loose
          incpre          = incpre ).

        IF semver IS BOUND.
          <version> = semver->version.
        ENDIF.
      ENDLOOP.
    ENDIF.

    INSERT LINES OF versions INTO TABLE result.

  ENDMETHOD.


  METHOD _versions.

    DELETE versions WHERE table_line IS INITIAL.

    LOOP AT versions ASSIGNING FIELD-SYMBOL(<version>).
      DATA(tabix) = sy-tabix.

      IF coerce = abap_true.
        DATA(semver) = zcl_abappm_semver_functions=>coerce( version = <version> rtl = rtl ).
        IF semver IS BOUND.
          <version> = semver->version.
        ELSE.
          DELETE versions INDEX tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF NOT zcl_abappm_semver_functions=>valid( <version> ).
        DELETE versions INDEX tabix.
      ENDIF.
    ENDLOOP.

    IF versions IS INITIAL.
      zcx_abappm_error=>raise( 'No valid versions found' ).
    ENDIF.

    IF inc IS NOT INITIAL AND ( lines( versions ) > 1 OR lines( ranges ) > 0 ).
      zcx_abappm_error=>raise( '--inc can only be used on a single version with no range' ).
    ENDIF.

    LOOP AT ranges ASSIGNING FIELD-SYMBOL(<range>).
      LOOP AT versions ASSIGNING <version>.
        tabix = sy-tabix.

        IF NOT zcl_abappm_semver_functions=>satisfies(
          version = <version>
          range   = <range>
          loose   = loose
          incpre  = incpre ).

          DELETE versions INDEX tabix.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    IF versions IS INITIAL.
      zcx_abappm_error=>raise( 'No valid versions found' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
