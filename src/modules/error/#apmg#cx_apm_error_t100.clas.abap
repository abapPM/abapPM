CLASS /apmg/cx_apm_error_t100 DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cx_apm_error
  CREATE PUBLIC.

************************************************************************
* Message Error
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !msgid    TYPE symsgid DEFAULT sy-msgid
        !msgno    TYPE symsgno DEFAULT sy-msgno
        !msgv1    TYPE symsgv DEFAULT sy-msgv1
        !msgv2    TYPE symsgv DEFAULT sy-msgv2
        !msgv3    TYPE symsgv DEFAULT sy-msgv3
        !msgv4    TYPE symsgv DEFAULT sy-msgv4
        !previous TYPE REF TO cx_root OPTIONAL
        !longtext TYPE csequence OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cx_apm_error_t100 IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    DATA(textid) = VALUE scx_t100key(
      msgid = msgid
      msgno = msgno
      attr1 = 'IF_T100_DYN_MSG~MSGV1'
      attr2 = 'IF_T100_DYN_MSG~MSGV2'
      attr3 = 'IF_T100_DYN_MSG~MSGV3'
      attr4 = 'IF_T100_DYN_MSG~MSGV4' ).

    super->constructor(
      textid   = textid
      previous = previous
      msgv1    = msgv1
      msgv2    = msgv2
      msgv3    = msgv3
      msgv4    = msgv4
      longtext = longtext ).

  ENDMETHOD.
ENDCLASS.
