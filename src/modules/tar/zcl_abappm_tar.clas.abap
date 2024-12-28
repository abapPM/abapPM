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
        !force_ustar  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tar.

    METHODS constructor
      IMPORTING
        !force_ustar TYPE abap_bool.

    "! Load archive
    METHODS load
      IMPORTING
        !tar          TYPE xstring
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
        !name         TYPE string
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
        !name         TYPE string
        !content      TYPE xsequence
        !date         TYPE d OPTIONAL
        !time         TYPE t OPTIONAL
        !mode         TYPE i OPTIONAL
        !typeflag     TYPE c OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tar
      RAISING
        zcx_abappm_error.

    "! Delete file from archive
    METHODS delete
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_abappm_tar
      RAISING
        zcx_abappm_error.

    "! Gzip archive
    METHODS gzip
      IMPORTING
        !tar          TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    "! Gunzip archive
    METHODS gunzip
      IMPORTING
        !gzip         TYPE xstring
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
      BEGIN OF ty_tar_item,
        name    TYPE string,
        content TYPE xstring,
      END OF ty_tar_item,
      ty_tar_list TYPE HASHED TABLE OF ty_tar_item WITH UNIQUE KEY name.

    CONSTANTS:
      c_blocksize     TYPE i VALUE 512,
      c_ustar_magic   TYPE c LENGTH 5 VALUE 'ustar',
      c_ustar_version TYPE c LENGTH 2 VALUE '00',
      c_mode_default  TYPE i VALUE 436, " octal 664 rw-rw-r--
      c_path_sep      TYPE c VALUE '/', " unix
      c_epoch         TYPE timestamp VALUE '19700101000000'.

    CLASS-DATA:
      null        TYPE c LENGTH 256,
      convert_in  TYPE REF TO cl_abap_conv_in_ce,
      convert_out TYPE REF TO cl_abap_conv_out_ce.

    DATA:
      force_ustar TYPE abap_bool,
      tar_files   TYPE ty_files,
      tar_list    TYPE ty_tar_list.

    METHODS _append_nulls
      CHANGING
        !data TYPE simple.

    METHODS _remove_nulls
      CHANGING
        !data TYPE simple.

    METHODS _pad
      IMPORTING
        !number       TYPE numeric
        !length       TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS _unpad
      IMPORTING
        !data         TYPE csequence
      RETURNING
        VALUE(result) TYPE i.

    METHODS _from_octal
      IMPORTING
        !octal        TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS _to_octal
      IMPORTING
        !number       TYPE numeric
      RETURNING
        VALUE(result) TYPE string.

    METHODS _from_xstring
      IMPORTING
        !data         TYPE xstring
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_abappm_error.

    METHODS _to_xstring
      IMPORTING
        !data         TYPE simple
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

    METHODS _from_filename
      IMPORTING
        !filename TYPE string
      EXPORTING
        !prefix   TYPE ty_header-prefix
        !name     TYPE ty_header-name
      RAISING
        zcx_abappm_error.

    METHODS _to_filename
      IMPORTING
        !prefix       TYPE ty_header-prefix
        !name         TYPE ty_header-name
      RETURNING
        VALUE(result) TYPE string.

    METHODS _from_unixtime
      IMPORTING
        !unixtime TYPE i
      EXPORTING
        !date     TYPE d
        !time     TYPE t
      RAISING
        zcx_abappm_error.

    METHODS _to_unixtime
      IMPORTING
        !date         TYPE d
        !time         TYPE t
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_abappm_error.

    METHODS _checksum
      IMPORTING
        VALUE(data)   TYPE any
      RETURNING
        VALUE(result) TYPE i
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_tar IMPLEMENTATION.


  METHOD append.

    DATA:
      file TYPE ty_file,
      item TYPE ty_tar_item.

    " List
    file-name    = name.
    file-size    = xstrlen( content ).
    IF date IS INITIAL.
      file-date = sy-datum.
    ELSE.
      file-date = date.
    ENDIF.
    IF time IS INITIAL.
      file-time = sy-uzeit.
    ELSE.
      file-time = time.
    ENDIF.
    IF mode IS INITIAL.
      file-mode = c_mode_default.
    ELSE.
      file-mode = mode.
    ENDIF.
    IF typeflag IS INITIAL.
      file-typeflag = c_typeflag-file.
    ELSE.
      file-typeflag = typeflag.
    ENDIF.
    file-unixtime = _to_unixtime( date = file-date time = file-time ).

    INSERT file INTO TABLE tar_files.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error adding file (list)' ).
    ENDIF.

    " Data
    item-name    = name.
    item-content = content.
    INSERT item INTO TABLE tar_list.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error adding file (data)' ).
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD class_constructor.

    " Generate a char 256 null
    DATA x TYPE x LENGTH 4 VALUE '00000000'.

    FIELD-SYMBOLS <c> TYPE c.

    ASSIGN x TO <c> CASTING ##SUBRC_OK.

    null = <c>.
    DO 8 TIMES.
      null = null && null.
    ENDDO.

    " Initialize converters
    convert_in  = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

  ENDMETHOD.


  METHOD constructor.

    me->force_ustar = force_ustar.

  ENDMETHOD.


  METHOD delete.

    DELETE tar_files WHERE name = name.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error deleting file (list)' ).
    ENDIF.

    DELETE tar_list WHERE name = name.
    IF sy-subrc <> 0.
      zcx_abappm_error=>raise( 'Error deleting file (data)' ).
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD get.

    READ TABLE tar_list ASSIGNING FIELD-SYMBOL(<item>) WITH TABLE KEY name = name.
    IF sy-subrc = 0.
      result = <item>-content.
    ELSE.
      zcx_abappm_error=>raise( 'Error getting file' ).
    ENDIF.

  ENDMETHOD.


  METHOD gunzip.

    cl_abap_gzip=>decompress_binary_with_header(
      EXPORTING
        gzip_in = gzip
      IMPORTING
        raw_out = result ).

  ENDMETHOD.


  METHOD gzip.

    cl_abap_gzip=>compress_binary_with_header(
      EXPORTING
        raw_in   = tar
      IMPORTING
        gzip_out = result ).

  ENDMETHOD.


  METHOD list.

    result = tar_files.

  ENDMETHOD.


  METHOD load.

    DATA:
      header TYPE ty_header,
      file   TYPE ty_file,
      item   TYPE ty_tar_item,
      block  TYPE xstring,
      count  TYPE i,
      length TYPE i,
      offset TYPE i.

    DATA(size) = xstrlen( tar ).

    IF size = 0 OR size MOD c_blocksize <> 0.
      zcx_abappm_error=>raise( 'Error loading file (blocksize)' ).
    ENDIF.

    CLEAR tar_files.

    DO.
      IF offset + c_blocksize > size.
        EXIT.
      ENDIF.

      " Header block
      block = tar+offset(c_blocksize).
      offset = offset + c_blocksize.

      header = _from_xstring( block ).

      _remove_nulls( CHANGING data = header ).

      IF header IS INITIAL.
        CONTINUE.
      ENDIF.

      IF force_ustar = abap_true.
        IF header-magic <> c_ustar_magic.
          zcx_abappm_error=>raise( 'Error loading file (ustar)' ).
        ELSEIF header-version <> c_ustar_version AND header-version <> ` `.
          zcx_abappm_error=>raise( 'Error loading file (version)' ).
        ENDIF.
      ENDIF.

      CLEAR file.
      file-name     = _to_filename( prefix = header-prefix name = header-name ).
      file-size     = _unpad( header-size ).
      file-mode     = _unpad( header-mode ).
      file-unixtime = _unpad( header-mtime ).

      _from_unixtime(
        EXPORTING
          unixtime = file-unixtime
        IMPORTING
          date     = file-date
          time     = file-time ).

      IF header-typeflag IS INITIAL.
        file-typeflag = c_typeflag-file.
      ELSE.
        file-typeflag = header-typeflag.
      ENDIF.

      INSERT file INTO TABLE tar_files.

      " Data blocks
      CLEAR item.
      item-name = file-name.

      length = file-size.
      count  = ( file-size - 1 ) DIV c_blocksize + 1.

      DO count TIMES.
        IF length > c_blocksize.
          block = tar+offset(c_blocksize).
        ELSE.
          block = tar+offset(length).
        ENDIF.
        item-content = item-content && block.
        offset = offset + c_blocksize.
        length = length - c_blocksize.
      ENDDO.

      INSERT item INTO TABLE tar_list.
    ENDDO.

    result = me.

  ENDMETHOD.


  METHOD new.

    CREATE OBJECT result
      EXPORTING
        force_ustar = force_ustar.

  ENDMETHOD.


  METHOD save.

    DATA:
      header TYPE ty_header,
      block  TYPE x LENGTH c_blocksize,
      count  TYPE i,
      length TYPE i,
      offset TYPE i.

    " TODO: Support other types
    LOOP AT tar_files ASSIGNING FIELD-SYMBOL(<file>)
      WHERE typeflag = c_typeflag-file OR typeflag = c_typeflag-directory.

      IF strlen( <file>-name ) > 255.
        zcx_abappm_error=>raise( 'Error saving file (name)' ).
      ELSEIF <file>-name CA '\'.
        zcx_abappm_error=>raise( 'Error saving file (path)' ).
      ENDIF.

      " Header block
      CLEAR header.

      _from_filename(
        EXPORTING
          filename = <file>-name
        IMPORTING
          prefix   = header-prefix
          name     = header-name ).

      header-mode     = _pad( number = <file>-mode length = 7 ).
      header-uid      = ''.
      header-gid      = ''.
      header-size     = _pad( number = <file>-size length = 11 ).
      header-mtime    = _pad( number = <file>-unixtime length = 11 ).
      header-typeflag = <file>-typeflag.
      header-magic    = c_ustar_magic.
      header-version  = c_ustar_version.
      header-uname    = to_lower( cl_abap_syst=>get_user_name( ) ).
      header-gname    = ''.
      header-linkname = ''.
      header-devminor = ''.
      header-devmajor = ''.
      header-padding  = ''.

      _append_nulls( CHANGING data = header ).

      header-chksum = `        `. " 8 spaces
      header-chksum = _pad( number = _checksum( header ) length = 7 ) && null.

      block = _to_xstring( header ).
      result   = result && block.

      " Data blocks
      READ TABLE tar_list ASSIGNING FIELD-SYMBOL(<item>) WITH TABLE KEY name = <file>-name.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Error saving file (data)' ).
      ENDIF.

      offset = 0.
      length = <file>-size.
      count  = ( length - 1 ) DIV c_blocksize + 1.

      DO count TIMES.
        IF length > c_blocksize.
          block = <item>-content+offset(c_blocksize).
        ELSE.
          block = <item>-content+offset(length).
        ENDIF.
        result    = result && block.
        offset = offset + c_blocksize.
        length = length - c_blocksize.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD _append_nulls.

    DATA(count) = 0.
    DO.
      count = count + 1.
      ASSIGN COMPONENT count OF STRUCTURE data TO FIELD-SYMBOL(<field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      <field> = <field> && null.
    ENDDO.

  ENDMETHOD.


  METHOD _checksum.

    DATA(xstring) = _to_xstring( data ).
    DATA(i) = 0.

    DO xstrlen( xstring ) TIMES.
      DATA(x) = xstring+i(1).
      result = result + x.
      i += 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_filename.

    DATA:
      temp_name   TYPE string,
      temp_prefix TYPE string.

    temp_name = filename.
    DO.
      IF strlen( temp_name ) <= 100.
        name = temp_name.
        EXIT.
      ENDIF.

      " Shorten name by moving part of path to prefix
      SPLIT temp_name AT c_path_sep INTO temp_prefix temp_name.
      IF sy-subrc <> 0.
        zcx_abappm_error=>raise( 'Error file name too long' ).
      ENDIF.

      IF prefix IS INITIAL.
        prefix = temp_prefix.
      ELSE.
        prefix = prefix && c_path_sep && temp_prefix.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD _from_octal.

    DATA(offset) = 0.

    DO strlen( octal ) TIMES.
      result = result * 8 + octal+offset(1).
      offset += 1.
    ENDDO.

  ENDMETHOD.


  METHOD _from_unixtime.

    TRY.
        DATA(timestamp) = cl_abap_tstmp=>add(
          tstmp = c_epoch
          secs  = unixtime ).

      CATCH cx_parameter_invalid_range
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting from UNIX time' ).
    ENDTRY.

    CONVERT TIME STAMP timestamp TIME ZONE 'UTC' INTO DATE date TIME time.

  ENDMETHOD.


  METHOD _from_xstring.

    TRY.
        convert_in->convert(
          EXPORTING
            input = data
            n     = xstrlen( data )
          IMPORTING
            data  = result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting from xstring' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _pad.

    result = |{ _to_octal( number ) ALIGN = RIGHT PAD = '0' WIDTH = length }|.

  ENDMETHOD.


  METHOD _remove_nulls.

    DATA(count) = 0.

    DO.
      count = count + 1.
      ASSIGN COMPONENT count OF STRUCTURE data TO FIELD-SYMBOL(<field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      REPLACE ALL OCCURRENCES OF null(1) IN <field> WITH ''.
    ENDDO.

  ENDMETHOD.


  METHOD _to_filename.

    IF prefix IS INITIAL.
      result = name.
    ELSE.
      result = prefix && c_path_sep && name.
    ENDIF.

  ENDMETHOD.


  METHOD _to_octal.

    DATA(temp_number) = CONV i( number ).

    WHILE temp_number > 0.
      result = |{ temp_number MOD 8 }{ result }|.
      temp_number = temp_number DIV 8.
    ENDWHILE.

    IF result IS INITIAL.
      result = '0'.
    ENDIF.

  ENDMETHOD.


  METHOD _to_unixtime.

    DATA timestamp TYPE timestamp.

    CONVERT DATE date TIME time INTO TIME STAMP timestamp TIME ZONE 'UTC'.

    TRY.
        result = cl_abap_tstmp=>subtract(
          tstmp1 = timestamp
          tstmp2 = c_epoch ).

      CATCH cx_parameter_invalid_range
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting to UNIX time' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _to_xstring.

    DATA(string_data) = CONV string( data ).

    TRY.
        convert_out->convert(
          EXPORTING
            data   = string_data
          IMPORTING
            buffer = result ).

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        zcx_abappm_error=>raise( 'Error converting to xstring' ).
    ENDTRY.

  ENDMETHOD.


  METHOD _unpad.

    DATA(temp_data) = CONV string( data ).

    temp_data = replace(
      val  = temp_data
      sub  = ` `
      with = ''
      occ  = 0 ).

    result = _from_octal( condense( data ) ).

  ENDMETHOD.
ENDCLASS.
