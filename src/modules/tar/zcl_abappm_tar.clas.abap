CLASS zcl_abappm_tar DEFINITION
  PUBLIC
  CREATE PRIVATE.

************************************************************************
* Tar
*
* Tar UStar Format
* Based on https://en.wikipedia.org/wiki/Tar_(computing)
* https://en.wikipedia.org/wiki/Gzip
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    TYPES:
      ty_typeflag TYPE c LENGTH 1,
      BEGIN OF ty_file,
        name     TYPE string,
        date     TYPE d,
        time     TYPE t,
        mode     TYPE i,
        unixtime TYPE i,
        size     TYPE i,
        typeflag TYPE ty_typeflag,
        content  TYPE xstring,
      END OF ty_file,
      ty_files TYPE STANDARD TABLE OF ty_file WITH KEY name.

    CONSTANTS:
      BEGIN OF c_typeflag,
        file              TYPE ty_typeflag VALUE '0',
        hard_link         TYPE ty_typeflag VALUE '1',
        symbolic_link     TYPE ty_typeflag VALUE '2',
        character_special TYPE ty_typeflag VALUE '3',
        block_special     TYPE ty_typeflag VALUE '4',
        directory         TYPE ty_typeflag VALUE '5',
        fifo              TYPE ty_typeflag VALUE '6',
        contiguous_file   TYPE ty_typeflag VALUE '7',
      END OF c_typeflag.

    CLASS-METHODS class_constructor.

    "! Create archive
    CLASS-METHODS new
      IMPORTING
        !iv_force_ustar TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE REF TO zcl_abappm_tar.

    METHODS constructor
      IMPORTING
        !iv_force_ustar TYPE abap_bool.

    "! Load archive
    METHODS load
      IMPORTING
        !iv_tar       TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tar
      RAISING
        zcx_abappm_error.

    "! Create archive
    METHODS save
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    "! Read file from archive
    METHODS get
      IMPORTING
        !iv_name      TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    "! List the table of contents of an archive (no data)
    METHODS list
      RETURNING
        VALUE(result) TYPE ty_files
      RAISING
        zcx_abappm_error.

    "! Append file to archive
    METHODS append
      IMPORTING
        !iv_name      TYPE string
        !iv_content   TYPE xsequence
        !iv_date      TYPE d OPTIONAL
        !iv_time      TYPE t OPTIONAL
        !iv_mode      TYPE i OPTIONAL
        !iv_typeflag  TYPE c OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tar
      RAISING
        zcx_abappm_error.

    "! Delete file from archive
    METHODS delete
      IMPORTING
        !iv_name      TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tar
      RAISING
        zcx_abappm_error.

    "! Gzip archive
    METHODS gzip
      IMPORTING
        !iv_tar       TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    "! Gunzip archive
    METHODS gunzip
      IMPORTING
        !iv_gzip      TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_header,
        name     TYPE c LENGTH 100,
        mode     TYPE c LENGTH 8,
        uid      TYPE c LENGTH 8,
        gid      TYPE c LENGTH 8,
        size     TYPE c LENGTH 12,
        mtime    TYPE c LENGTH 12,
        chksum   TYPE c LENGTH 8,
        typeflag TYPE c LENGTH 1,
        linkname TYPE c LENGTH 100,
        magic    TYPE c LENGTH 6,
        version  TYPE c LENGTH 2,
        uname    TYPE c LENGTH 32,
        gname    TYPE c LENGTH 32,
        devmajor TYPE c LENGTH 8,
        devminor TYPE c LENGTH 8,
        prefix   TYPE c LENGTH 155,
        padding  TYPE c LENGTH 12,
      END OF ty_header.

    TYPES:
      BEGIN OF ty_data,
        name    TYPE string,
        content TYPE xstring,
      END OF ty_data,
      ty_tar_data TYPE HASHED TABLE OF ty_data WITH UNIQUE KEY name.

    CONSTANTS:
      c_blocksize     TYPE i VALUE 512,
      c_ustar_magic   TYPE c LENGTH 5 VALUE 'ustar',
      c_ustar_version TYPE c LENGTH 2 VALUE '00',
      c_mode_default  TYPE i VALUE 436, " octal 664 rw-rw-r--
      c_path_sep      TYPE c VALUE '/', " unix
      c_epoch         TYPE timestamp VALUE '19700101000000'.

    CLASS-DATA:
      gv_null        TYPE c LENGTH 256,
      go_convert_in  TYPE REF TO cl_abap_conv_in_ce,
      go_convert_out TYPE REF TO cl_abap_conv_out_ce.

    DATA:
      mv_force_ustar TYPE abap_bool,
      mt_files       TYPE ty_files,
      mt_data        TYPE ty_tar_data.

    METHODS _append_nulls
      CHANGING
        !cg_data TYPE simple.

    METHODS _remove_nulls
      CHANGING
        !cg_data TYPE simple.

    METHODS _pad
      IMPORTING
        !iv_number       TYPE numeric
        !iv_length       TYPE i
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _unpad
      IMPORTING
        !iv_data         TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE i.

    METHODS _from_octal
      IMPORTING
        !iv_octal        TYPE string
      RETURNING
        VALUE(rv_result) TYPE i.

    METHODS _to_octal
      IMPORTING
        !iv_number       TYPE numeric
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _from_xstring
      IMPORTING
        !iv_data         TYPE xstring
      RETURNING
        VALUE(rv_result) TYPE string
      RAISING
        zcx_abappm_error.

    METHODS _to_xstring
      IMPORTING
        !ig_data         TYPE simple
      RETURNING
        VALUE(rv_result) TYPE xstring
      RAISING
        zcx_abappm_error.

    METHODS _from_filename
      IMPORTING
        !iv_filename TYPE string
      EXPORTING
        !ev_prefix   TYPE ty_header-prefix
        !ev_name     TYPE ty_header-name
      RAISING
        zcx_abappm_error.

    METHODS _to_filename
      IMPORTING
        !iv_prefix       TYPE ty_header-prefix
        !iv_name         TYPE ty_header-name
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _from_unixtime
      IMPORTING
        !iv_unixtime TYPE i
      EXPORTING
        !ev_date     TYPE d
        !ev_time     TYPE t
      RAISING
        zcx_abappm_error.

    METHODS _to_unixtime
      IMPORTING
        !iv_date         TYPE d
        !iv_time         TYPE t
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        zcx_abappm_error.

    METHODS _checksum
      IMPORTING
        VALUE(ig_data)   TYPE any
      RETURNING
        VALUE(rv_result) TYPE i
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_tar IMPLEMENTATION.


  METHOD append.

    DATA:
      ls_file TYPE ty_file,
      ls_data TYPE ty_data.

    " List
    ls_file-name    = iv_name.
    ls_file-size    = xstrlen( iv_content ).
    IF iv_date IS INITIAL.
      ls_file-date = sy-datum.
    ELSE.
      ls_file-date = iv_date.
    ENDIF.
    IF iv_time IS INITIAL.
      ls_file-time = sy-uzeit.
    ELSE.
      ls_file-time = iv_time.
    ENDIF.
    IF iv_mode IS INITIAL.
      ls_file-mode = c_mode_default.
    ELSE.
      ls_file-mode = iv_mode.
    ENDIF.
    IF iv_typeflag IS INITIAL.
      ls_file-typeflag = c_typeflag-file.
    ELSE.
      ls_file-typeflag = iv_typeflag.
    ENDIF.
    ls_file-unixtime = _to_unixtime( iv_date = ls_file-date iv_time = ls_file-time ).

    INSERT ls_file INTO TABLE mt_files.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error adding file (list)' ).
    ENDIF.

    " Data
    ls_data-name    = iv_name.
    ls_data-content = iv_content.
    INSERT ls_data INTO TABLE mt_data.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error adding file (data)' ).
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD class_constructor.

    " Generate a char 256 null
    DATA lv_x TYPE x LENGTH 4 VALUE '00000000'.

    FIELD-SYMBOLS <lv_y> TYPE c.

    ASSIGN lv_x TO <lv_y> CASTING ##SUBRC_OK.

    gv_null = <lv_y>.

    DO 8 TIMES.
      gv_null = gv_null && gv_null.
    ENDDO.

    " Initialize converters
    go_convert_in  = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    go_convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

  ENDMETHOD.


  METHOD constructor.
    mv_force_ustar = iv_force_ustar.
  ENDMETHOD.


  METHOD delete.

    DELETE mt_files WHERE name = iv_name.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error deleting file (list)' ).
    ENDIF.

    DELETE mt_data WHERE name = iv_name.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error deleting file (data)' ).
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD get.

    FIELD-SYMBOLS <ls_data> TYPE ty_data.

    READ TABLE mt_data ASSIGNING <ls_data> WITH TABLE KEY name = iv_name.
    IF sy-subrc = 0.
      result = <ls_data>-content.
    ELSE.
      zcx_abappm_error=>raise( 'Error getting file' ).
    ENDIF.

  ENDMETHOD.


  METHOD gunzip.
    cl_abap_gzip=>decompress_binary_with_header(
      EXPORTING
        gzip_in = iv_gzip
      IMPORTING
        raw_out = result ).
  ENDMETHOD.


  METHOD gzip.
    cl_abap_gzip=>compress_binary_with_header(
      EXPORTING
        raw_in   = iv_tar
      IMPORTING
        gzip_out = result ).
  ENDMETHOD.


  METHOD list.
    result = mt_files.
  ENDMETHOD.


  METHOD load.

    DATA:
      lv_size   TYPE i,
      ls_header TYPE ty_header,
      ls_file   TYPE ty_file,
      ls_data   TYPE ty_data,
      lv_block  TYPE xstring,
      lv_count  TYPE i,
      lv_length TYPE i,
      lv_offset TYPE i.

    lv_size = xstrlen( iv_tar ).

    IF lv_size = 0 OR lv_size MOD c_blocksize <> 0.
      zcx_abappm_error=>raise( 'Error loading file (blocksize)' ).
    ENDIF.

    CLEAR mt_files.

    DO.
      IF lv_offset + c_blocksize > lv_size.
        EXIT.
      ENDIF.

      " Header block
      lv_block = iv_tar+lv_offset(c_blocksize).
      lv_offset = lv_offset + c_blocksize.

      ls_header = _from_xstring( lv_block ).

      _remove_nulls( CHANGING cg_data = ls_header ).

      IF ls_header IS INITIAL.
        CONTINUE.
      ENDIF.

      IF mv_force_ustar = abap_true.
        IF ls_header-magic <> c_ustar_magic.
          zcx_abappm_error=>raise( 'Error loading file (ustar)' ).
        ELSEIF ls_header-version <> c_ustar_version AND ls_header-version <> ` `.
          zcx_abappm_error=>raise( 'Error loading file (version)' ).
        ENDIF.
      ENDIF.

      CLEAR ls_file.
      ls_file-name     = _to_filename( iv_prefix = ls_header-prefix iv_name = ls_header-name ).
      ls_file-size     = _unpad( ls_header-size ).
      ls_file-mode     = _unpad( ls_header-mode ).
      ls_file-unixtime = _unpad( ls_header-mtime ).

      _from_unixtime(
        EXPORTING
          iv_unixtime = ls_file-unixtime
        IMPORTING
          ev_date     = ls_file-date
          ev_time     = ls_file-time ).

      IF ls_header-typeflag IS INITIAL.
        ls_file-typeflag = c_typeflag-file.
      ELSE.
        ls_file-typeflag = ls_header-typeflag.
      ENDIF.

      INSERT ls_file INTO TABLE mt_files.

      " Data blocks
      CLEAR ls_data.
      ls_data-name = ls_file-name.

      lv_length = ls_file-size.
      lv_count  = ( ls_file-size - 1 ) DIV c_blocksize + 1.

      DO lv_count TIMES.
        IF lv_length > c_blocksize.
          lv_block = iv_tar+lv_offset(c_blocksize).
        ELSE.
          lv_block = iv_tar+lv_offset(lv_length).
        ENDIF.
        ls_data-content = ls_data-content && lv_block.
        lv_offset = lv_offset + c_blocksize.
        lv_length = lv_length - c_blocksize.
      ENDDO.

      INSERT ls_data INTO TABLE mt_data.
    ENDDO.

    result = me.

  ENDMETHOD.


  METHOD new.

    CREATE OBJECT result
      EXPORTING
        iv_force_ustar = iv_force_ustar.

  ENDMETHOD.


  METHOD save.

    DATA:
      ls_header TYPE ty_header,
      lv_block  TYPE x LENGTH c_blocksize,
      lv_count  TYPE i,
      lv_length TYPE i,
      lv_offset TYPE i.

    FIELD-SYMBOLS:
      <ls_file> TYPE ty_file,
      <ls_data> TYPE ty_data.

    " TODO: Support other types
    LOOP AT mt_files ASSIGNING <ls_file>
      WHERE typeflag = c_typeflag-file OR typeflag = c_typeflag-directory.

      IF strlen( <ls_file>-name ) > 255.
        zcx_abappm_error=>raise( 'Error saving file (name)' ).
      ELSEIF <ls_file>-name CA '\'.
        zcx_abappm_error=>raise( 'Error saving file (path)' ).
      ENDIF.

      " Header block
      CLEAR ls_header.

      _from_filename(
        EXPORTING
          iv_filename = <ls_file>-name
        IMPORTING
          ev_prefix   = ls_header-prefix
          ev_name     = ls_header-name ).

      ls_header-mode     = _pad( iv_number = <ls_file>-mode iv_length = 7 ).
      ls_header-uid      = ''.
      ls_header-gid      = ''.
      ls_header-size     = _pad( iv_number = <ls_file>-size iv_length = 11 ).
      ls_header-mtime    = _pad( iv_number = <ls_file>-unixtime iv_length = 11 ).
      ls_header-typeflag = <ls_file>-typeflag.
      ls_header-magic    = c_ustar_magic.
      ls_header-version  = c_ustar_version.
      ls_header-uname    = to_lower( cl_abap_syst=>get_user_name( ) ).
      ls_header-gname    = ''.
      ls_header-linkname = ''.
      ls_header-devminor = ''.
      ls_header-devmajor = ''.
      ls_header-padding  = ''.

      _append_nulls( CHANGING cg_data = ls_header ).

      ls_header-chksum = `        `. " 8 spaces
      ls_header-chksum = _pad( iv_number = _checksum( ls_header ) iv_length = 7 ) && gv_null.

      lv_block = _to_xstring( ls_header ).
      result   = result && lv_block.

      " Data blocks
      READ TABLE mt_data ASSIGNING <ls_data> WITH TABLE KEY name = <ls_file>-name.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Error saving file (data)' ).
      ENDIF.

      lv_offset = 0.
      lv_length = <ls_file>-size.
      lv_count  = ( lv_length - 1 ) DIV c_blocksize + 1.

      DO lv_count TIMES.
        IF lv_length > c_blocksize.
          lv_block = <ls_data>-content+lv_offset(c_blocksize).
        ELSE.
          lv_block = <ls_data>-content+lv_offset(lv_length).
        ENDIF.
        result    = result && lv_block.
        lv_offset = lv_offset + c_blocksize.
        lv_length = lv_length - c_blocksize.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD _append_nulls.

    DATA lv_count TYPE i.

    FIELD-SYMBOLS <lv_field> TYPE any.

    DO.
      lv_count = lv_count + 1.
      ASSIGN COMPONENT lv_count OF STRUCTURE cg_data TO <lv_field>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      <lv_field> = <lv_field> && gv_null.
    ENDDO.

  ENDMETHOD.


  METHOD _checksum.

    DATA:
      lv_data TYPE xstring,
      lv_i    TYPE i,
      lv_x    TYPE x LENGTH 1.

    lv_data = _to_xstring( ig_data ).

    DO xstrlen( lv_data ) TIMES.
      lv_x = lv_data+lv_i(1).
      rv_result = rv_result + lv_x.
      lv_i = lv_i + 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_filename.

    DATA:
      lv_name   TYPE string,
      lv_prefix TYPE string.

    lv_name = iv_filename.
    DO.
      IF strlen( lv_name ) <= 100.
        ev_name = lv_name.
        EXIT.
      ENDIF.

      " Shorten name by moving part of path to prefix
      SPLIT lv_name AT c_path_sep INTO lv_prefix lv_name.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Error file name too long' ).
      ENDIF.

      IF ev_prefix IS INITIAL.
        ev_prefix = lv_prefix.
      ELSE.
        ev_prefix = ev_prefix && c_path_sep && lv_prefix.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD _from_octal.

    DATA lv_offset TYPE i.

    DO strlen( iv_octal ) TIMES.
      rv_result = rv_result * 8 + iv_octal+lv_offset(1).
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_unixtime.

    DATA lv_timestamp TYPE timestampl.

    TRY.
        lv_timestamp = cl_abap_tstmp=>add(
          tstmp = c_epoch
          secs  = iv_unixtime ).

      CATCH cx_parameter_invalid_range
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting from UNIX time' ).
    ENDTRY.

    CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC' INTO DATE ev_date TIME ev_time.

  ENDMETHOD.


  METHOD _from_xstring.

    TRY.
        go_convert_in->convert(
          EXPORTING
            input = iv_data
            n     = xstrlen( iv_data )
          IMPORTING
            data  = rv_result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting from xstring' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _pad.

    rv_result = |{ _to_octal( iv_number ) ALIGN = RIGHT PAD = '0' WIDTH = iv_length }|.

  ENDMETHOD.


  METHOD _remove_nulls.

    DATA lv_count TYPE i.

    FIELD-SYMBOLS <lv_field> TYPE any.

    DO.
      lv_count = lv_count + 1.
      ASSIGN COMPONENT lv_count OF STRUCTURE cg_data TO <lv_field>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      REPLACE ALL OCCURRENCES OF gv_null(1) IN <lv_field> WITH ''.
    ENDDO.

  ENDMETHOD.


  METHOD _to_filename.

    IF iv_prefix IS INITIAL.
      rv_result = iv_name.
    ELSE.
      rv_result = iv_prefix && c_path_sep && iv_name.
    ENDIF.

  ENDMETHOD.


  METHOD _to_octal.

    DATA lv_number TYPE i.

    lv_number = iv_number.

    WHILE lv_number > 0.
      rv_result = |{ lv_number MOD 8 }{ rv_result }|.
      lv_number = lv_number DIV 8.
    ENDWHILE.

    IF rv_result IS INITIAL.
      rv_result = '0'.
    ENDIF.

  ENDMETHOD.


  METHOD _to_unixtime.

    DATA lv_timestamp TYPE timestamp.

    CONVERT DATE iv_date TIME iv_time INTO TIME STAMP lv_timestamp TIME ZONE 'UTC'.

    TRY.
        rv_result = cl_abap_tstmp=>subtract(
          tstmp1 = lv_timestamp
          tstmp2 = c_epoch ).

      CATCH cx_parameter_invalid_range
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting to UNIX time' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _to_xstring.

    DATA lv_data TYPE string.

    lv_data = ig_data.

    TRY.
        go_convert_out->convert(
          EXPORTING
            data   = lv_data
          IMPORTING
            buffer = rv_result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting to xstring' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _unpad.

    DATA lv_data TYPE string.

    lv_data = iv_data.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_data WITH ''.

    rv_result = _from_octal( condense( lv_data ) ).

  ENDMETHOD.
ENDCLASS.
