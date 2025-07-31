CLASS /apmg/cl_apm_object_prog DEFINITION PUBLIC FINAL CREATE PUBLIC.

************************************************************************
* apm PROG Importer (=WIP=)
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* TODO!: Activation of target program
* TODO: Add support for dynpros, cua, longtexts
************************************************************************
  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_object.

    METHODS constructor
      IMPORTING
        !item TYPE /apmg/if_apm_object=>ty_item.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_state,
        active   TYPE r3state VALUE 'A',
        inactive TYPE r3state VALUE 'I',
      END OF c_state.

    DATA program_name TYPE program.

    METHODS deserialize_program
      IMPORTING
        !progdir TYPE zif_abapgit_sap_report=>ty_progdir
        !source  TYPE /apmg/if_apm_importer=>ty_code
        !tpool   TYPE textpool_table
        !package TYPE devclass
      RAISING
        /apmg/cx_apm_error.

    METHODS deserialize_textpool
      IMPORTING
        !program  TYPE syrepid
        !tpool    TYPE textpool_table
        !language TYPE sy-langu OPTIONAL
        !include  TYPE abap_bool DEFAULT abap_false
      RAISING
        /apmg/cx_apm_error.

    METHODS get_program_title
      IMPORTING
        !tpool        TYPE textpool_table
      RETURNING
        VALUE(result) TYPE repti.

    METHODS insert_program
      IMPORTING
        !progdir TYPE zif_abapgit_sap_report=>ty_progdir
        !source  TYPE /apmg/if_apm_importer=>ty_code
        !title   TYPE repti
        !package TYPE devclass
        !state   TYPE progdir-state DEFAULT c_state-inactive
      RAISING
        /apmg/cx_apm_error.

    METHODS update_program
      IMPORTING
        !progdir TYPE zif_abapgit_sap_report=>ty_progdir
        !source  TYPE /apmg/if_apm_importer=>ty_code
        !title   TYPE repti
        !state   TYPE progdir-state DEFAULT c_state-inactive
      RAISING
        /apmg/cx_apm_error.
ENDCLASS.



CLASS /apmg/cl_apm_object_prog IMPLEMENTATION.


  METHOD /apmg/if_apm_object~import.

    DATA program_texts TYPE textpool_table.

    DATA(is_pretty) = xsdbool( is_dryrun = abap_false ).

    TRY.
        " Get old program
        DATA(program_dir) = zcl_abapgit_factory=>get_sap_report( )->read_progdir( program_name ).

        " TODO: Make files mandatory
        IF files IS INITIAL.
          DATA(orig_program_code) = /apmg/cl_apm_code_importer=>read( program_name ).
        ELSE.
          orig_program_code = files->get_abap( ).
        ENDIF.

        DATA(program_code) = /apmg/cl_apm_code_importer=>import(
          program_name   = program_name
          program_source = orig_program_code
          map            = map
          is_pretty      = is_pretty ).

        READ TEXTPOOL program_name INTO program_texts.

        IF is_dryrun IS INITIAL AND program_code <> orig_program_code.
          program_dir-name = new_object.

          deserialize_program(
            progdir = program_dir
            source  = program_code
            tpool   = program_texts
            package = new_package ).

          deserialize_textpool(
            program = new_object
            tpool   = program_texts ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    program_name = item-obj_name.

  ENDMETHOD.


  METHOD deserialize_program.

    DATA:
      progname TYPE reposrc-progname,
      title    TYPE rglif-title.

    TRY.
        zcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
          iv_object   = 'ABAP'
          iv_obj_name = progdir-name
          iv_package  = package
          iv_language = sy-langu ).

        title = get_program_title( tpool ).

        " Check if program already exists
        SELECT SINGLE progname FROM reposrc INTO @progname
          WHERE progname = @progdir-name
            AND r3state = @c_state-active.

        IF sy-subrc = 0.
          update_program(
            progdir = progdir
            source  = source
            title   = title ).
        ELSE.
          insert_program(
            progdir = progdir
            source  = source
            title   = title
            package = package ).
        ENDIF.

        zcl_abapgit_factory=>get_sap_report( )->update_progdir(
          is_progdir = progdir
          iv_package = package ).

        zcl_abapgit_objects_activation=>add(
          iv_type = 'REPS'
          iv_name = progdir-name ).

      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize_textpool.

    IF language IS INITIAL.
      DATA(_language) = sy-langu.
    ELSE.
      _language = language.
    ENDIF.

    IF _language = sy-langu.
      DATA(state) = c_state-inactive. "Textpool in main language needs to be activated
    ELSE.
      state = c_state-active. "Translations are always active
    ENDIF.

    IF tpool IS INITIAL.
      IF include = abap_false OR state = c_state-active.
        DELETE TEXTPOOL program "Remove initial description from textpool if
          LANGUAGE _language     "original program does not have a textpool
          STATE state.

        DATA(delete) = abap_true.
      ELSE.
        INSERT TEXTPOOL program "In case of includes: Deletion of textpool in
          FROM tpool            "main language cannot be activated because
          LANGUAGE _language     "this would activate the deletion of the textpool
          STATE state.          "of the mail program -> insert empty textpool
      ENDIF.
    ELSE.
      INSERT TEXTPOOL program
        FROM tpool
        LANGUAGE _language
        STATE state.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Error from INSERT TEXTPOOL'.
      ENDIF.
    ENDIF.

    " Textpool in main language needs to be activated
    IF state = c_state-inactive.
      TRY.
          zcl_abapgit_objects_activation=>add(
            iv_type   = 'REPT'
            iv_name   = program
            iv_delete = delete ).

        CATCH zcx_abapgit_exception INTO DATA(error).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD get_program_title.

    FIELD-SYMBOLS <any> TYPE any.

    READ TABLE tpool INTO DATA(tpool_line) WITH KEY id = 'R'.
    IF sy-subrc = 0.
      " there is a bug in RPY_PROGRAM_UPDATE, the header line of TTAB is not
      " cleared, so the title length might be inherited from a different program.
      ASSIGN ('(SAPLSIFP)TTAB') TO <any>.
      IF sy-subrc = 0.
        CLEAR <any>.
      ENDIF.

      result = tpool_line-entry.
    ENDIF.

  ENDMETHOD.


  METHOD insert_program.

    DATA _source TYPE abaptxt255_tab.

    _source = source.

    TRY.
        CALL FUNCTION 'RPY_PROGRAM_INSERT'
          EXPORTING
            development_class = package
            program_name      = progdir-name
            program_type      = progdir-subc
            title_string      = title
            save_inactive     = state
            suppress_dialog   = abap_true
            uccheck           = progdir-uccheck " does not exist on lower releases
          TABLES
            source_extended   = _source
          EXCEPTIONS
            already_exists    = 1
            cancelled         = 2
            name_not_allowed  = 3
            permission_error  = 4
            OTHERS            = 5 ##FM_SUBRC_OK.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'RPY_PROGRAM_INSERT'
          EXPORTING
            development_class = package
            program_name      = progdir-name
            program_type      = progdir-subc
            title_string      = title
            save_inactive     = state
            suppress_dialog   = abap_true
          TABLES
            source_extended   = _source
          EXCEPTIONS
            already_exists    = 1
            cancelled         = 2
            name_not_allowed  = 3
            permission_error  = 4
            OTHERS            = 5 ##FM_SUBRC_OK.
    ENDTRY.
    IF sy-subrc = 3.

      TRY.
          " For cases that standard function does not handle (like FUGR),
          " we save active and inactive version of source with the given PROGRAM TYPE.
          " Without the active version, the code will not be visible in case of activation errors.
          zcl_abapgit_factory=>get_sap_report( )->insert_report(
            iv_name         = progdir-name
            iv_package      = package
            it_source       = source
            iv_state        = c_state-active
            iv_version      = progdir-uccheck
            iv_program_type = progdir-subc ).

          zcl_abapgit_factory=>get_sap_report( )->insert_report(
            iv_name         = progdir-name
            iv_package      = package
            it_source       = source
            iv_state        = c_state-inactive
            iv_version      = progdir-uccheck
            iv_program_type = progdir-subc ).

        CATCH zcx_abapgit_exception INTO DATA(error).
          RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
      ENDTRY.
    ELSEIF sy-subrc > 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
    ENDIF.

  ENDMETHOD.


  METHOD update_program.

    DATA _source TYPE abaptxt255_tab.

    _source = source.

    zcl_abapgit_language=>set_current_language( sy-langu ).

    CALL FUNCTION 'RPY_PROGRAM_UPDATE'
      EXPORTING
        program_name     = progdir-name
        title_string     = title
        save_inactive    = state
      TABLES
        source_extended  = _source
      EXCEPTIONS
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcl_abapgit_language=>restore_login_language( ).

      IF sy-msgid = 'EU' AND sy-msgno = '510'.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'User is currently editing program'.
      ELSEIF sy-msgid = 'EU' AND sy-msgno = '522'.
        " for generated table maintenance function groups, the author is set to SAP* instead of the user which
        " generates the function group. This hits some standard checks, pulling new code again sets the author
        " to the current user which avoids the check
      ELSE.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_t100.
      ENDIF.
    ENDIF.

    zcl_abapgit_language=>restore_login_language( ).

  ENDMETHOD.
ENDCLASS.
