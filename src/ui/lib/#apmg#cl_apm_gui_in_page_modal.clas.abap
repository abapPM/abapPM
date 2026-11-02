CLASS /apmg/cl_apm_gui_in_page_modal DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !child        TYPE REF TO /apmg/if_apm_gui_renderable
        !width        TYPE i OPTIONAL
        !height       TYPE i OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_gui_in_page_modal
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
      BEGIN OF attrs,
        width  TYPE i,
        height TYPE i,
      END OF attrs.

ENDCLASS.



CLASS /apmg/cl_apm_gui_in_page_modal IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_renderable~render.

    ri_html = /apmg/cl_apm_html=>create( ).

    IF child IS INITIAL.
      RETURN.
    ENDIF.

    IF attrs-width IS NOT INITIAL.
      DATA(width) = |width:{ attrs-width }px;|.
    ENDIF.
    IF attrs-height IS NOT INITIAL.
      DATA(height) = |height:{ attrs-height }px;|.
    ENDIF.

    ri_html->add( |<div id="modal" class="modal" style="{ width }{ height }">| ).
    ri_html->add( |<div class="modal-guts">| ).
    ri_html->add( child->render( ) ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).
    ri_html->add( |<div class="modal-overlay"></div>| ).

    " Keep Tab inside the popup
    /apmg/cl_apm_gui_factory=>get_gui_services( )->get_html_parts( )->add_part(
      iv_collection = /apmg/cl_apm_gui_component=>c_html_parts-scripts
      ii_part       = /apmg/cl_apm_html=>create( )->set_title( 'in_page_modal' )->add( 'trapFocus();' ) ).

  ENDMETHOD.


  METHOD constructor.

    attrs-width  = width.
    attrs-height = height.
    me->child    = child.

  ENDMETHOD.


  METHOD create.

    CREATE OBJECT result
      EXPORTING
        child  = child
        width  = width
        height = height.

  ENDMETHOD.
ENDCLASS.
