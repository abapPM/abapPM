CLASS zcx_abappm_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

************************************************************************
* Error
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    INTERFACES:
      if_t100_dyn_msg,
      if_t100_message.

    TYPES:
      BEGIN OF ty_scr_info,
        program TYPE progname,
        include TYPE progname,
        line    TYPE i,
      END OF ty_scr_info.

    "! Black Hole
    "! Can be used for MESSAGE ... INTO null
    CLASS-DATA null TYPE string ##NEEDED.

    DATA longtext TYPE string READ-ONLY.
    DATA callstack TYPE abap_callstack READ-ONLY.
    DATA src_info TYPE ty_scr_info READ-ONLY.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL
        !longtext TYPE csequence OPTIONAL.

    "! Raise exception with text
    "! @parameter text | Text
    "! @parameter previous | Previous exception
    "! @parameter longtext | Longtext
    "! @raising zcx_abappm_error | Exception
    CLASS-METHODS raise
      IMPORTING
        !text     TYPE clike
        !previous TYPE REF TO cx_root OPTIONAL
        !longtext TYPE csequence OPTIONAL
      RAISING
        zcx_abappm_error.

    "! Raise exception with T100 message
    "! <p>
    "! Will default to sy-msg* variables. These need to be set right before calling this method.
    "! </p>
    "! @parameter msgid | Message ID
    "! @parameter msgno | Message number
    "! @parameter msgv1 | Message variable 1
    "! @parameter msgv2 | Message variable 2
    "! @parameter msgv3 | Message variable 3
    "! @parameter msgv4 | Message variable 4
    "! @parameter previous | Previous exception
    "! @parameter longtext | Longtext
    "! @raising zcx_abappm_error | Exception
    CLASS-METHODS raise_t100
      IMPORTING
        msgid     TYPE symsgid DEFAULT sy-msgid
        msgno     TYPE symsgno DEFAULT sy-msgno
        msgv1     TYPE symsgv DEFAULT sy-msgv1
        msgv2     TYPE symsgv DEFAULT sy-msgv2
        msgv3     TYPE symsgv DEFAULT sy-msgv3
        msgv4     TYPE symsgv DEFAULT sy-msgv4
        !previous TYPE REF TO cx_root OPTIONAL
        !longtext TYPE csequence OPTIONAL
      RAISING
        zcx_abappm_error.

    "! Raise with text from previous exception
    "! @parameter previous | Previous exception
    "! @parameter longtext | Longtext
    "! @raising zcx_abappm_error | Exception
    CLASS-METHODS raise_with_text
      IMPORTING
        !previous TYPE REF TO cx_root
        !longtext TYPE csequence OPTIONAL
      RAISING
        zcx_abappm_error.

    METHODS get_source_position REDEFINITION.
    METHODS if_message~get_longtext REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_generic_error_msg TYPE string VALUE `An error occured`.

    METHODS save_callstack.

    METHODS get_t100_longtext
      RETURNING
        VALUE(result) TYPE tline_tab.

ENDCLASS.



CLASS zcx_abappm_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    if_t100_dyn_msg~msgv1 = msgv1.
    if_t100_dyn_msg~msgv2 = msgv2.
    if_t100_dyn_msg~msgv3 = msgv3.
    if_t100_dyn_msg~msgv4 = msgv4.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->longtext = longtext.

    save_callstack( ).

    " Save for debugger
    get_source_position(
      IMPORTING
        program_name = src_info-program
        include_name = src_info-include
        source_line  = src_info-line ).

  ENDMETHOD.


  METHOD get_source_position.

    READ TABLE callstack ASSIGNING FIELD-SYMBOL(<callstack>) INDEX 1.
    IF sy-subrc = 0.
      program_name = <callstack>-mainprogram.
      include_name = <callstack>-include.
      source_line  = <callstack>-line.
    ELSE.
      super->get_source_position(
        IMPORTING
          program_name = program_name
          include_name = include_name
          source_line  = source_line ).
    ENDIF.

  ENDMETHOD.


  METHOD get_t100_longtext.

    DATA(docu_key) = CONV doku_obj( if_t100_message~t100key-msgid && if_t100_message~t100key-msgno ).

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id     = 'NA'
        langu  = sy-langu
        object = docu_key
        typ    = 'E'
      TABLES
        line   = result
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc = 0.
      ASSIGN me->(if_t100_message~t100key-attr1) TO FIELD-SYMBOL(<msgv>).
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V1&' IN TABLE result WITH <msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr2) TO <msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V2&' IN TABLE result WITH <msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr3) TO <msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V3&' IN TABLE result WITH <msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr4) TO <msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V4&' IN TABLE result WITH <msgv>.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_message~get_longtext.

    IF longtext IS NOT INITIAL.

      result = longtext.

      IF preserve_newlines = abap_false.
        result = lcl_error_longtext=>remove_newlines( result ).
      ENDIF.

    ELSEIF if_t100_message~t100key IS NOT INITIAL.

      result = lcl_error_longtext=>to_string( get_t100_longtext( ) ).

      IF preserve_newlines = abap_false.
        result = lcl_error_longtext=>remove_newlines( result ).
      ENDIF.

    ELSE.
      result = super->get_longtext( preserve_newlines ).
    ENDIF.

  ENDMETHOD.


  METHOD raise.

    IF text IS INITIAL.
      cl_message_helper=>set_msg_vars_for_clike( c_generic_error_msg ).
    ELSE.
      cl_message_helper=>set_msg_vars_for_clike( text ).
    ENDIF.

    raise_t100(
      previous = previous
      longtext = longtext ).

  ENDMETHOD.


  METHOD raise_t100.

    IF msgid IS NOT INITIAL.
      DATA(t100_key) = VALUE scx_t100key(
        msgid = msgid
        msgno = msgno
        attr1 = 'IF_T100_DYN_MSG~MSGV1'
        attr2 = 'IF_T100_DYN_MSG~MSGV2'
        attr3 = 'IF_T100_DYN_MSG~MSGV3'
        attr4 = 'IF_T100_DYN_MSG~MSGV4' ).
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abappm_error
      EXPORTING
        textid   = t100_key
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4
        previous = previous
        longtext = longtext.

  ENDMETHOD.


  METHOD raise_with_text.

    raise(
      text     = previous->get_text( )
      previous = previous
      longtext = longtext ).

  ENDMETHOD.


  METHOD save_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.

    DATA(main_pattern) = cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) && '*'.

    " Remember that the first lines are from this exception class and are
    " removed so that highest level in the callstack is the position where
    " the exception is raised.
    "
    " For a merged report it's hard to do that, because the exception
    " isn't visible in the callstack. Therefore we have to check the events.
    LOOP AT callstack ASSIGNING FIELD-SYMBOL(<callstack>).

      IF <callstack>-mainprogram CP main_pattern " full
        OR <callstack>-blockname = `SAVE_CALLSTACK` " merged
        OR <callstack>-blockname = `CONSTRUCTOR` " merged
        OR <callstack>-blockname CP `RAISE*`. "merged

        DELETE TABLE callstack FROM <callstack>.

      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
