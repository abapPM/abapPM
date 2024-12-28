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

    CLASS-DATA null TYPE string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL.

    "! Raise exception with text
    "! @parameter text | Text
    "! @parameter previous | Previous exception
    "! @raising zcx_abappm_error | Exception
    CLASS-METHODS raise
      IMPORTING
        !text     TYPE clike
        !previous TYPE REF TO cx_root OPTIONAL
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
    "! @raising zcx_abappm_error | Exception
    CLASS-METHODS raise_t100
      IMPORTING
        VALUE(msgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(msgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(msgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(msgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(msgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(msgv4) TYPE symsgv DEFAULT sy-msgv4
        !previous    TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_abappm_error.

    "! Raise with text from previous exception
    "! @parameter previous | Previous exception
    "! @raising zcx_abappm_error | Exception
    CLASS-METHODS raise_with_text
      IMPORTING
        !previous TYPE REF TO cx_root
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_generic_error_msg TYPE string VALUE `An error occured`.

    CLASS-METHODS split_text_to_symsg
      IMPORTING
        !text         TYPE string
      RETURNING
        VALUE(result) TYPE symsg.

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

  ENDMETHOD.


  METHOD raise.

    IF text IS INITIAL.
      DATA(msg) = split_text_to_symsg( c_generic_error_msg ).
    ELSE.
      msg = split_text_to_symsg( text ).
    ENDIF.

    " Set syst variables using generic error message
    MESSAGE e001(00) WITH msg-msgv1 msg-msgv2 msg-msgv3 msg-msgv4 INTO null.

    raise_t100( previous = previous ).

  ENDMETHOD.


  METHOD raise_t100.

    DATA t100_key TYPE scx_t100key.

    IF msgid IS NOT INITIAL.
      t100_key-msgid = msgid.
      t100_key-msgno = msgno.
      t100_key-attr1 = 'IF_T100_DYN_MSG~MSGV1'.
      t100_key-attr2 = 'IF_T100_DYN_MSG~MSGV2'.
      t100_key-attr3 = 'IF_T100_DYN_MSG~MSGV3'.
      t100_key-attr4 = 'IF_T100_DYN_MSG~MSGV4'.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abappm_error
      EXPORTING
        textid   = t100_key
        msgv1    = msgv1
        msgv2    = msgv2
        msgv3    = msgv3
        msgv4    = msgv4
        previous = previous.

  ENDMETHOD.


  METHOD raise_with_text.

    raise(
      text     = previous->get_text( )
      previous = previous ).

  ENDMETHOD.


  METHOD split_text_to_symsg.

    CONSTANTS:
      c_length_of_msgv           TYPE i VALUE 50,
      c_offset_of_last_character TYPE i VALUE 49.

    DATA:
      msg_text TYPE c LENGTH 200,
      rest     TYPE c LENGTH 200,
      msg_var  TYPE c LENGTH c_length_of_msgv,
      index    TYPE sy-index.

    msg_text = text.

    DO 4 TIMES.
      index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = c_length_of_msgv
          text   = msg_text
        IMPORTING
          line   = msg_var
          rest   = rest.

      IF msg_var+c_offset_of_last_character(1) = space OR msg_text+c_length_of_msgv(1) = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        rest = | { rest }|.
      ENDIF.

      msg_text = rest.

      CASE index.
        WHEN 1.
          result-msgv1 = msg_var.
        WHEN 2.
          result-msgv2 = msg_var.
        WHEN 3.
          result-msgv3 = msg_var.
        WHEN 4.
          result-msgv4 = msg_var.
      ENDCASE.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
