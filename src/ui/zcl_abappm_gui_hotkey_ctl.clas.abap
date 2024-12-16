CLASS zcl_abappm_gui_hotkey_ctl DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_gui_component
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm GUI Hotkey Controller
*
* Copyright 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
* adapted: gui_component and settings
  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_hotkeys.
    INTERFACES zif_abapgit_gui_hotkey_ctl.
    INTERFACES zif_abapgit_gui_renderable.

    CONSTANTS c_showhotkeys_action TYPE string VALUE `showHotkeys` ##NO_TEXT.

    CLASS-METHODS should_show_hint
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      hotkeys           TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
      keyboard_settings TYPE zif_abappm_settings=>ty_keyboard_settings,
      is_visible        TYPE abap_bool.

    CLASS-DATA was_hint_shown TYPE abap_bool.

    METHODS render_scripts
      IMPORTING
        !hotkeys      TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr
      RETURNING
        VALUE(result) TYPE REF TO zif_abapgit_html.

ENDCLASS.



CLASS zcl_abappm_gui_hotkey_ctl IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    TRY.
        keyboard_settings = zcl_abappm_settings=>factory( )->get( )-keyboard_settings. " apm
      CATCH zcx_abappm_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD render_scripts.

    DATA lv_json TYPE string.

    FIELD-SYMBOLS: <hotkey> LIKE LINE OF hotkeys.

    lv_json = `{`.

    LOOP AT hotkeys ASSIGNING <hotkey>.

      IF sy-tabix > 1.
        lv_json = lv_json && |,|.
      ENDIF.

      lv_json = lv_json && |  "{ <hotkey>-hotkey }" : "{ <hotkey>-action }" |.

    ENDLOOP.

    lv_json = lv_json && `}`.

    result = zcl_abapgit_html=>create( ).
    result->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    result->add( |setKeyBindings({ lv_json });| ).

  ENDMETHOD.


  METHOD should_show_hint.
    IF was_hint_shown = abap_false.
      result = abap_true.
      was_hint_shown = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA hotkey LIKE LINE OF rt_hotkey_actions.

    hotkey-ui_component = 'Hotkeys'.
    hotkey-action       = c_showhotkeys_action.
    hotkey-description  = 'Show Hotkeys Help'.
    hotkey-hotkey       = '?'.
    INSERT hotkey INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys.
    rt_registered_hotkeys = hotkeys.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~register_hotkeys.

    " Compress duplicates
    LOOP AT it_hotkeys ASSIGNING FIELD-SYMBOL(<hotkey>).
      READ TABLE hotkeys WITH KEY hotkey = <hotkey>-hotkey TRANSPORTING NO FIELDS.
      IF sy-subrc = 0. " If found command with same hotkey
        DELETE hotkeys INDEX sy-tabix. " Later registered commands enjoys the priority
      ENDIF.

      IF keyboard_settings-link_hints_enabled = abap_true AND
         keyboard_settings-link_hint_key      = <hotkey>-hotkey.
        " Link hint activation key is more important
        CONTINUE.
      ENDIF.

      APPEND <hotkey> TO hotkeys.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~reset.
    CLEAR hotkeys.
  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkey_ctl~set_visible.
    is_visible = iv_visible.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.
    register_handlers( ).

    DATA(result) = zcl_abapgit_html=>create( ).

    DATA(registered_hotkeys) = zif_abapgit_gui_hotkey_ctl~get_registered_hotkeys( ).
    SORT registered_hotkeys BY ui_component description.

    register_deferred_script( render_scripts( registered_hotkeys ) ).

    " Render hotkeys
    result->add( '<ul class="hotkeys">' ).
    LOOP AT registered_hotkeys ASSIGNING FIELD-SYMBOL(<hotkey>).
      result->add( |<li>|
        && |<span class="key-id">{ <hotkey>-hotkey }</span>|
        && |<span class="key-descr">{ <hotkey>-description }</span>|
        && |</li>| ).
    ENDLOOP.

    " render link hints activation key
    IF keyboard_settings-link_hints_enabled = abap_true.
      result->add( |<li>|
         && |<span class="key-id">{ keyboard_settings-link_hint_key }</span>|
         && |<span class="key-descr">Link Hints</span>|
         && |</li>| ).
      result->add( |<li>|
         && |<span class="key-id">y{ keyboard_settings-link_hint_key }</span>|
         && |<span class="key-descr">Copy Link Text</span>|
         && |</li>| ).
    ENDIF.

    result->add( '</ul>' ).

    DATA(hotkey) = ''.
    READ TABLE registered_hotkeys ASSIGNING <hotkey>
      WITH KEY action = c_showhotkeys_action.
    IF sy-subrc = 0.
      hotkey = <hotkey>-hotkey.
    ENDIF.

    DATA(hint) = |Close window with upper right corner 'X'|.
    IF hotkey IS NOT INITIAL.
      hint = hint && | or press '{ <hotkey>-hotkey }'|.
    ENDIF.

    result = zcl_abapgit_gui_chunk_lib=>render_infopanel(
      iv_div_id     = 'hotkeys'
      iv_title      = 'Hotkeys'
      iv_hint       = hint
      iv_hide       = boolc( is_visible = abap_false )
      iv_scrollable = abap_false
      io_content    = result ).

    IF hotkey IS NOT INITIAL AND should_show_hint( ) = abap_true.
      result->add( |<div id="hotkeys-hint" class="corner-hint">|
        && |Press '{ <hotkey>-hotkey }' to get keyboard shortcuts list|
        && |</div>| ).
    ENDIF.

    " Always reset visibility here. Closing of the popup has to be done by the
    " user and is handled in JS.
    is_visible = abap_false.

    ri_html = result.
  ENDMETHOD.
ENDCLASS.
