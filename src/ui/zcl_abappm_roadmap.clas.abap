CLASS zcl_abappm_roadmap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS not_implemented
      RAISING
        zcx_abappm_error.

    CLASS-METHODS planned
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_roadmap IMPLEMENTATION.


  METHOD not_implemented.
    zcx_abappm_error=>raise( 'The feature has not been implemented yet' ).
  ENDMETHOD.


  METHOD planned.
    zcx_abappm_error=>raise( 'The feature has not been implemented but is on the roadmap' ).
  ENDMETHOD.
ENDCLASS.
