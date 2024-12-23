CLASS lcl_persist_utils DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA update_function TYPE funcname.

    CLASS-METHODS get_package_description
      IMPORTING
        package    TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_user_description
      IMPORTING
        username   TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_update_function
      RETURNING
        VALUE(result) TYPE funcname.

ENDCLASS.

CLASS lcl_persist_utils IMPLEMENTATION.

  METHOD get_package_description.
    SELECT SINGLE ctext FROM tdevct INTO result
      WHERE devclass = package AND spras = sy-langu ##SUBRC_OK.
  ENDMETHOD.

  METHOD get_user_description.

    DATA user_address TYPE addr3_val.

    DATA(user_name) = CONV xubname( username ).

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = user_name
      IMPORTING
        user_address           = user_address
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      result = user_address-name_text.
    ELSE.
      result = user_name.
    ENDIF.

  ENDMETHOD.

  METHOD get_update_function.

    IF update_function IS INITIAL.
      update_function = 'CALL_V1_PING'.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = update_function
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        " Fallback
        update_function = 'BANK_OBJ_WORKL_RELEASE_LOCKS'.
      ENDIF.
    ENDIF.

    result = update_function.

  ENDMETHOD.

ENDCLASS.
