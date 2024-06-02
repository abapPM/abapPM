INTERFACE zif_abappm_settings PUBLIC.


************************************************************************
* Settings
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  TYPES:
    BEGIN OF ty_package_settings,
      package         TYPE devclass,
      favorite        TYPE abap_bool,
      write_protected TYPE abap_bool,
      labels          TYPE string_table,
    END OF ty_package_settings.

  TYPES:
    BEGIN OF ty_settings,
      registry          TYPE string,
      last_package      TYPE devclass,
      show_last_package TYPE abap_bool,
      BEGIN OF gui_settings,
        adt_jump_enabled TYPE abap_bool,
        max_lines        TYPE i,
        icon_scaling     TYPE c LENGTH 1,
        ui_theme         TYPE string,
        label_colors     TYPE string,
      END OF gui_settings,
      BEGIN OF keyboard_settings,
        link_hints_enabled TYPE abap_bool,
        link_hint_key      TYPE c LENGTH 1,
      END OF keyboard_settings,
      BEGIN OF list_settings,
        filter           TYPE string,
        only_favorites   TYPE abap_bool,
        show_details     TYPE abap_bool,
        order_by         TYPE string,
        order_descending TYPE abap_bool,
      END OF list_settings,
      package_settings  TYPE SORTED TABLE OF ty_package_settings WITH UNIQUE KEY package,
    END OF ty_settings.

  METHODS get
    RETURNING
      VALUE(result) TYPE ty_settings.

  METHODS get_json
    IMPORTING
      !iv_complete  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !is_settings  TYPE ty_settings
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_settings
    RAISING
      zcx_abappm_error.

  METHODS set_json
    IMPORTING
      !iv_json      TYPE string
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_settings
    RAISING
      zcx_abappm_error.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO zif_abappm_settings
    RAISING
      zcx_abappm_error.

  METHODS save
    RAISING
      zcx_abappm_error.

  METHODS delete
    RAISING
      zcx_abappm_error.

  METHODS is_valid
    RETURNING
      VALUE(result) TYPE abap_bool.

ENDINTERFACE.
