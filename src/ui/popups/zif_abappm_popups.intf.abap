INTERFACE zif_abappm_popups PUBLIC.

  TYPES ty_char1 TYPE c LENGTH 1.
  TYPES ty_icon TYPE c LENGTH 30.

  TYPES:
    BEGIN OF ty_popup_position,
      start_column LIKE  sy-cucol,
      start_row    LIKE  sy-curow,
      end_column   LIKE  sy-cucol,
      end_row      LIKE  sy-curow,
    END OF ty_popup_position.

  METHODS popup_search_help
    IMPORTING
      !iv_tab_field   TYPE string
    RETURNING
      VALUE(rv_value) TYPE ddshretval-fieldval
    RAISING
      zcx_abappm_error.

  METHODS popup_folder_logic
    RETURNING
      VALUE(rv_folder_logic) TYPE string
    RAISING
      zcx_abappm_error.

  METHODS popup_to_confirm
    IMPORTING
      !iv_titlebar              TYPE clike
      !iv_text_question         TYPE clike
      !iv_text_button_1         TYPE clike DEFAULT 'Yes'
      !iv_icon_button_1         TYPE ty_icon DEFAULT space
      !iv_text_button_2         TYPE clike DEFAULT 'No'
      !iv_icon_button_2         TYPE ty_icon DEFAULT space
      !iv_default_button        TYPE ty_char1 DEFAULT '1'
      !iv_display_cancel_button TYPE ty_char1 DEFAULT abap_true
      !iv_popup_type            TYPE clike DEFAULT 'ICON_MESSAGE_QUESTION'
    RETURNING
      VALUE(rv_answer)          TYPE ty_char1
    RAISING
      zcx_abappm_error.

  METHODS popup_to_create_package
    IMPORTING
      is_package_data  TYPE zif_abapgit_sap_package=>ty_create OPTIONAL
    EXPORTING
      !es_package_data TYPE zif_abapgit_sap_package=>ty_create
      !ev_create       TYPE abap_bool
    RAISING
      zcx_abappm_error.

  METHODS popup_to_select_transport
    RETURNING
      VALUE(rv_trkorr) TYPE trkorr.

  METHODS popup_to_select_labels
    IMPORTING
      iv_labels        TYPE string OPTIONAL
    RETURNING
      VALUE(rv_labels) TYPE string
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
