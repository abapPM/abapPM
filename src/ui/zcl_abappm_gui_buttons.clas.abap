CLASS zcl_abappm_gui_buttons DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Buttons
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS advanced
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS help
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS package_list
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS settings
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS experimental
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_gui_buttons IMPLEMENTATION.


  METHOD advanced.
    result = zcl_abapgit_html=>icon(
      iv_name = 'tools-solid'
      iv_hint = 'Utilities' ).
  ENDMETHOD.


  METHOD experimental.
    result = zcl_abapgit_html=>icon(
      iv_name = 'vial-solid/red'
      iv_hint = 'Experimental Features are Enabled' ).
  ENDMETHOD.


  METHOD help.
    result = zcl_abapgit_html=>icon(
      iv_name = 'question-circle-solid'
      iv_hint = 'Help' ).
  ENDMETHOD.


  METHOD package_list.
    result = zcl_abapgit_html=>icon( 'bars' ) && ' Package List'.
  ENDMETHOD.


  METHOD settings.
    result = zcl_abapgit_html=>icon( 'cog' ) && ' Settings'.
  ENDMETHOD.
ENDCLASS.
