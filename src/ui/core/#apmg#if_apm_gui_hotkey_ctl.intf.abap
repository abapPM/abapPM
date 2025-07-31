INTERFACE /apmg/if_apm_gui_hotkey_ctl PUBLIC.

  METHODS register_hotkeys
    IMPORTING
      !it_hotkeys TYPE /apmg/if_apm_gui_hotkeys=>ty_hotkeys_with_descr.

  METHODS reset.

  METHODS get_registered_hotkeys
    RETURNING
      VALUE(rt_registered_hotkeys) TYPE /apmg/if_apm_gui_hotkeys=>ty_hotkeys_with_descr
    RAISING
      /apmg/cx_apm_error.

  METHODS set_visible
    IMPORTING
      !iv_visible TYPE abap_bool.

ENDINTERFACE.
