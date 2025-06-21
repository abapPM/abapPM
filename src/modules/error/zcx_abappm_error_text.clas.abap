CLASS zcx_abappm_error_text DEFINITION
  PUBLIC
  INHERITING FROM zcx_abappm_error_t100
  CREATE PUBLIC.

************************************************************************
* Text Error
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !text     TYPE clike
        !previous LIKE previous OPTIONAL
        !longtext TYPE csequence OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_abappm_error_text IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    cl_message_helper=>set_msg_vars_for_clike( text ).

    super->constructor(
      previous = previous
      longtext = longtext ).

  ENDMETHOD.
ENDCLASS.
