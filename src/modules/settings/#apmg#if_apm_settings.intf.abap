INTERFACE /apmg/if_apm_settings PUBLIC.

************************************************************************
* apm Settings
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.

  TYPES:
    BEGIN OF ty_gui_settings,
      adt_jump_enabled  TYPE abap_bool,
      activate_wo_popup TYPE abap_bool,
      max_lines         TYPE i,
      icon_scaling      TYPE c LENGTH 1,
      ui_theme          TYPE string,
      label_colors      TYPE string,
    END OF ty_gui_settings,
    BEGIN OF ty_keyboard_settings,
      link_hints_enabled TYPE abap_bool,
      link_hint_key      TYPE c LENGTH 1,
    END OF ty_keyboard_settings,
    BEGIN OF ty_list_settings,
      filter           TYPE string,
      only_favorites   TYPE abap_bool,
      " show_details     TYPE abap_bool, " uses JS windows.localStorage
      order_by         TYPE string,
      order_descending TYPE abap_bool,
    END OF ty_list_settings,
    BEGIN OF ty_package_settings,
      package         TYPE devclass,
      favorite        TYPE abap_bool,
      write_protected TYPE abap_bool,
      labels          TYPE string_table,
    END OF ty_package_settings.

  TYPES:
    BEGIN OF ty_settings,
      registry              TYPE string,
      last_package          TYPE devclass,
      show_last_package     TYPE abap_bool,
      experimental_features TYPE string,
      gui_settings          TYPE ty_gui_settings,
      keyboard_settings     TYPE ty_keyboard_settings,
      list_settings         TYPE ty_list_settings,
      package_settings      TYPE SORTED TABLE OF ty_package_settings WITH UNIQUE KEY package,
    END OF ty_settings.

  TYPES ty_name TYPE syst_uname.

  CONSTANTS:
    c_registry   TYPE string VALUE 'https://registry.abappm.com',
    c_playground TYPE string VALUE 'https://playground.abappm.com',
    c_global     TYPE ty_name VALUE /apmg/if_apm_persist_apm=>c_key_name-global_settings,
    c_user       TYPE ty_name VALUE /apmg/if_apm_persist_apm=>c_key_name-user_settings.

  METHODS get
    RETURNING
      VALUE(result) TYPE ty_settings.

  METHODS get_json
    IMPORTING
      !is_complete  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string
    RAISING
      /apmg/cx_apm_error.

  METHODS set
    IMPORTING
      !settings     TYPE ty_settings
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_settings
    RAISING
      /apmg/cx_apm_error.

  METHODS set_json
    IMPORTING
      !json         TYPE string
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_settings
    RAISING
      /apmg/cx_apm_error.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_apm_settings
    RAISING
      /apmg/cx_apm_error.

  METHODS save
    RAISING
      /apmg/cx_apm_error.

  METHODS delete
    RAISING
      /apmg/cx_apm_error.

  METHODS is_valid
    RETURNING
      VALUE(result) TYPE abap_bool.

ENDINTERFACE.
