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
        VALUE(rv_html_string) TYPE string.

    CLASS-METHODS help
      RETURNING
        VALUE(rv_html_string) TYPE string.

    CLASS-METHODS package_list
      RETURNING
        VALUE(rv_html_string) TYPE string.

    CLASS-METHODS settings
      RETURNING
        VALUE(rv_html_string) TYPE string.

    CLASS-METHODS experimental
      RETURNING
        VALUE(rv_html_string) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_gui_buttons IMPLEMENTATION.


  METHOD advanced.
    rv_html_string = zcl_abapgit_html=>icon(
      iv_name = 'tools-solid'
      iv_hint = 'Utilities' ).
  ENDMETHOD.


  METHOD experimental.
    rv_html_string = zcl_abapgit_html=>icon(
      iv_name = 'vial-solid/red'
      iv_hint = 'Experimental Features are Enabled' ).
  ENDMETHOD.


  METHOD help.
    rv_html_string = zcl_abapgit_html=>icon(
      iv_name = 'question-circle-solid'
      iv_hint = 'Help' ).
  ENDMETHOD.


  METHOD package_list.
    rv_html_string = zcl_abapgit_html=>icon( 'bars' ) && ' Package List'.
  ENDMETHOD.


  METHOD settings.
    rv_html_string = zcl_abapgit_html=>icon( 'cog' ) && ' Settings'.
  ENDMETHOD.
ENDCLASS.
