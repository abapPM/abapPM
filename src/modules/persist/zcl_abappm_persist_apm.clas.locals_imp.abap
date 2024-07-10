CLASS lcl_persist_utils DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA gv_update_function TYPE funcname.

    CLASS-METHODS get_package_description
      IMPORTING
        iv_package    TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_user_description
      IMPORTING
        iv_username   TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_update_function
      RETURNING
        VALUE(result) TYPE funcname.

ENDCLASS.

CLASS lcl_persist_utils IMPLEMENTATION.

  METHOD get_package_description.
    SELECT SINGLE ctext FROM tdevct INTO result
      WHERE devclass = iv_package AND spras = sy-langu ##SUBRC_OK.
  ENDMETHOD.

  METHOD get_user_description.

    DATA:
      lv_username     TYPE xubname,
      ls_user_address TYPE addr3_val.

    lv_username = iv_username.

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = lv_username
      IMPORTING
        user_address           = ls_user_address
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      result = ls_user_address-name_text.
    ELSE.
      result = iv_username.
    ENDIF.

  ENDMETHOD.

  METHOD get_update_function.

    IF gv_update_function IS INITIAL.
      gv_update_function = 'CALL_V1_PING'.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = gv_update_function
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        " Fallback
        gv_update_function = 'BANK_OBJ_WORKL_RELEASE_LOCKS'.
      ENDIF.
    ENDIF.

    result = gv_update_function.

  ENDMETHOD.

ENDCLASS.
