INTERFACE /apmg/if_apm_gui_router PUBLIC.

************************************************************************
* apm GUI Router
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS:
    BEGIN OF c_action,
      apm_deprecate        TYPE string VALUE 'apm_deprecate',
      apm_home             TYPE string VALUE 'apm_home',
      apm_init             TYPE string VALUE 'apm_init',
      apm_install          TYPE string VALUE 'apm_install',
      apm_uninstall        TYPE string VALUE 'apm_uninstall',
      apm_publish          TYPE string VALUE 'apm_publish',
      apm_undeprecate      TYPE string VALUE 'apm_undeprecate',
      apm_unpublish        TYPE string VALUE 'apm_unpublish',
      apm_update           TYPE string VALUE 'apm_update',
      change_order_by      TYPE string VALUE 'change_order_by',
      changelog            TYPE string VALUE 'changelog',
      clipboard            TYPE string VALUE 'clipboard',
      direction            TYPE string VALUE 'direction',
      documentation        TYPE string VALUE 'documentation',
      favorite_package     TYPE string VALUE 'favorite_package',
      feedback             TYPE string VALUE 'feedback',
      go_back              TYPE string VALUE 'go_back',
      go_db                TYPE string VALUE 'go_db',
      go_debuginfo         TYPE string VALUE 'go_debuginfo',
      go_home              TYPE string VALUE 'go_home',
      go_package           TYPE string VALUE 'go_package',
      go_settings          TYPE string VALUE 'go_settings',
      go_settings_personal TYPE string VALUE 'go_settings_personal',
      go_tree              TYPE string VALUE 'go_tree',
      go_tutorial          TYPE string VALUE 'go_tutorial',
      goto_message         TYPE string VALUE 'goto_message',
      goto_source          TYPE string VALUE 'goto_source',
      homepage             TYPE string VALUE 'homepage',
      ie_devtools          TYPE string VALUE 'ie_devtools',
      jump                 TYPE string VALUE 'jump',
      jump_transaction     TYPE string VALUE 'jump_transaction',
      jump_transport       TYPE string VALUE 'jump_transport',
      jump_user            TYPE string VALUE 'jump_user',
      registry             TYPE string VALUE 'registry',
      show_callstack       TYPE string VALUE 'show_callstack',
      show_hotkeys         TYPE string VALUE 'show_hotkeys',
      sponsor              TYPE string VALUE 'sponsor',
      url                  TYPE string VALUE 'url',
    END OF c_action.

ENDINTERFACE.
