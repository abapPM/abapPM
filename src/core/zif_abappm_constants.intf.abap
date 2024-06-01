INTERFACE zif_abappm_constants PUBLIC.

************************************************************************
* apm Constants
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS:
    c_version TYPE string VALUE '1.0.0'.

  CONSTANTS:
    c_website       TYPE string VALUE 'https://abappm.com',
    c_documentation TYPE string VALUE 'https://docs.abappm.com',
    c_registry      TYPE string VALUE 'https://registry.abappm.com',
    c_repository    TYPE string VALUE 'https://github.com/abapPM/abapPM'.

  CONSTANTS:
    BEGIN OF c_action,
      apm_home             TYPE string VALUE 'apm_home',
      change_order_by      TYPE string VALUE 'change_order_by',
      changelog            TYPE string VALUE 'changelog',
      clipboard            TYPE string VALUE 'clipboard',
      db_display           TYPE string VALUE 'db_display',
      db_edit              TYPE string VALUE 'db_edit',
      direction            TYPE string VALUE 'direction',
      documentation        TYPE string VALUE 'documentation',
      go_back              TYPE string VALUE 'go_back',
      go_db                TYPE string VALUE 'go_db',
      go_debuginfo         TYPE string VALUE 'go_debuginfo',
      go_home              TYPE string VALUE 'go_home',
      go_settings_global   TYPE string VALUE 'go_settings_global',
      go_settings_personal TYPE string VALUE 'go_settings_personal',
      go_tutorial          TYPE string VALUE 'go_tutorial',
      goto_message         TYPE string VALUE 'goto_message',
      goto_source          TYPE string VALUE 'goto_source',
      homepage             TYPE string VALUE 'homepage',
      ie_devtools          TYPE string VALUE 'ie_devtools',
      jump                 TYPE string VALUE 'jump',
      jump_transaction     TYPE string VALUE 'jump_transaction',
      jump_transport       TYPE string VALUE 'jump_transport',
      jump_user            TYPE string VALUE 'jump_user',
      show_callstack       TYPE string VALUE 'show_callstack',
      show_hotkeys         TYPE string VALUE 'show_hotkeys',
      sponsor              TYPE string VALUE 'sponsor',
      toggle_favorites     TYPE string VALUE 'toggle_favorites',
      url                  TYPE string VALUE 'url',
      yank_to_clipboard    TYPE string VALUE 'yank_to_clipboard',
    END OF c_action.

ENDINTERFACE.
