CLASS /apmg/cl_apm_gui_in_page_modal DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm GUI In-Page Modal
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !child         TYPE REF TO /apmg/if_apm_gui_renderable
        !width          TYPE i OPTIONAL
        !height         TYPE i OPTIONAL
      RETURNING
        VALUE(result)  TYPE REF TO /apmg/cl_apm_gui_in_page_modal
      RAISING
        /apmg/cx_apm_error.

    METHODS constructor
      IMPORTING
        !child  TYPE REF TO /apmg/if_apm_gui_renderable
        !width  TYPE i OPTIONAL
        !height TYPE i OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA child TYPE REF TO /apmg/if_apm_gui_renderable.

    DATA:
      BEGIN OF attributes,
        width  TYPE i,
        height TYPE i,
      END OF attributes.

ENDCLASS.



CLASS /apmg/cl_apm_gui_in_page_modal IMPLEMENTATION.


  METHOD constructor.

    attributes-width  = width.
    attributes-height = height.
    me->child         = child.

  ENDMETHOD.


  METHOD create.

    result = NEW /apmg/cl_apm_gui_in_page_modal(
      child  = child
      width  = width
      height = height ).

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_renderable~render.

    DATA(style) = NEW /apmg/cl_apm_string_buffer( ).

    ri_html = /apmg/cl_apm_html=>create( ).

    IF child IS NOT BOUND.
      RETURN.
    ENDIF.

    IF attributes-width IS NOT INITIAL.
      style->add( |width:{ attributes-width }px;| ).
    ENDIF.
    IF attributes-height IS NOT INITIAL.
      style->add( |height:{ attributes-height }px;| ).
    ENDIF.

    ri_html->add( |<div id="modal" class="modal" style="{ style->join_w_space_and_flush( ) }">| ).
    ri_html->add( '<div class="modal-guts">' ).
    ri_html->add( child->render( ) ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).
    ri_html->add( '<div class="modal-overlay"></div>' ).

  ENDMETHOD.
ENDCLASS.
