CLASS /apmg/cl_apm_gui_styles DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Styles
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS emoji
      IMPORTING
        styles        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_html.

    CLASS-METHODS markdown
      IMPORTING
        styles        TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_html.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_gui_styles IMPLEMENTATION.


  METHOD emoji.

    " Emoji styles
    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->add( '<style>' ).
    html->add( '@scope {' ).
    html->add( /apmg/cl_apm_emoji=>styles( ) ).

    IF styles IS NOT INITIAL.
      html->add( styles ).
    ENDIF.

    html->add( '}' ).
    html->add( '</style>' ).

    result = html.

  ENDMETHOD.


  METHOD markdown.

    " Markdown + Emoji styles
    DATA(html) = /apmg/cl_apm_html=>create( ).

    html->add( '<style>' ).
    html->add( '@scope {' ).
    html->add( /apmg/cl_apm_markdown=>styles( ) ).
    html->add( /apmg/cl_apm_emoji=>styles( ) ).

    IF styles IS NOT INITIAL.
      html->add( styles ).
    ENDIF.

    html->add( '}' ).
    html->add( '</style>' ).

    result = html.

  ENDMETHOD.
ENDCLASS.
