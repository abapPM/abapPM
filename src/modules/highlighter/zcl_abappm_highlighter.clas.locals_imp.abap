CLASS lcl_out DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS convert
      IMPORTING
        !string       TYPE string
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        zcx_abappm_error.

  PRIVATE SECTION.

    CLASS-DATA conv_new TYPE REF TO object.
    CLASS-DATA conv_old TYPE REF TO object.

ENDCLASS.

CLASS lcl_out IMPLEMENTATION.

  METHOD convert.

    IF conv_new IS INITIAL AND conv_old IS INITIAL.
      TRY.
          CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_out
            RECEIVING
              instance = conv_new.
        CATCH cx_sy_dyn_call_illegal_class.
          DATA(class) = 'CL_ABAP_CONV_OUT_CE'.
          CALL METHOD (class)=>create
            EXPORTING
              encoding = 'UTF-8'
            RECEIVING
              conv     = conv_old.
      ENDTRY.
    ENDIF.

    TRY.
        IF conv_new IS NOT INITIAL.
          CALL METHOD conv_new->('IF_ABAP_CONV_OUT~CONVERT')
            EXPORTING
              source = string
            RECEIVING
              result = result.
        ELSE.
          CALL METHOD conv_old->('CONVERT')
            EXPORTING
              data   = string
            IMPORTING
              buffer = result.
        ENDIF.
      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_abappm_error_prev EXPORTING previous = error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
