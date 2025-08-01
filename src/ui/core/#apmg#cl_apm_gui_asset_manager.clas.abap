CLASS /apmg/cl_apm_gui_asset_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_gui_asset_manager.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_asset_manager) TYPE REF TO /apmg/if_apm_gui_asset_manager.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_asset_entry.
        INCLUDE TYPE /apmg/if_apm_gui_asset_manager~ty_web_asset.
    TYPES: mime_name TYPE wwwdatatab-objid,
      END OF ty_asset_entry,
      ty_asset_register TYPE STANDARD TABLE OF ty_asset_entry WITH KEY url.

    DATA mt_asset_register TYPE ty_asset_register.

    METHODS get_mime_asset
      IMPORTING
        iv_mime_name    TYPE c
      RETURNING
        VALUE(rv_xdata) TYPE xstring
      RAISING
        /apmg/cx_apm_error.

    METHODS load_asset
      IMPORTING
        is_asset_entry  TYPE ty_asset_entry
      RETURNING
        VALUE(rs_asset) TYPE /apmg/if_apm_gui_asset_manager~ty_web_asset
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_gui_asset_manager IMPLEMENTATION.


  METHOD /apmg/if_apm_gui_asset_manager~get_all_assets.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    LOOP AT mt_asset_register ASSIGNING <ls_a>.
      APPEND load_asset( <ls_a> ) TO rt_assets.
    ENDLOOP.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_asset_manager~get_asset.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    READ TABLE mt_asset_register WITH KEY url = iv_url ASSIGNING <ls_a>.
    IF <ls_a> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Cannot find GUI asset: { iv_url }|.
    ENDIF.
    rs_asset = load_asset( <ls_a> ).

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_asset_manager~get_text_asset.

    DATA ls_asset TYPE /apmg/if_apm_gui_asset_manager~ty_web_asset.

    ls_asset = /apmg/if_apm_gui_asset_manager~get_asset( iv_url ).

    IF ls_asset-type <> 'text'.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Not a text asset: { iv_url }|.
    ENDIF.

    IF iv_assert_subtype IS NOT INITIAL AND ls_asset-subtype <> iv_assert_subtype.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Wrong subtype ({ iv_assert_subtype }): { iv_url }|.
    ENDIF.

    TRY.
        rv_asset = zcl_abapgit_convert=>xstring_to_string_utf8( ls_asset-content ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD /apmg/if_apm_gui_asset_manager~register_asset.

    DATA ls_asset LIKE LINE OF mt_asset_register.

    SPLIT iv_type AT '/' INTO ls_asset-type ls_asset-subtype.
    ls_asset-url          = iv_url.
    ls_asset-mime_name    = iv_mime_name.
    ls_asset-is_cacheable = iv_cacheable.

    TRY.
        IF iv_base64 IS NOT INITIAL.
          ls_asset-content = zcl_abapgit_convert=>base64_to_xstring( iv_base64 ).
        ELSEIF iv_inline IS NOT INITIAL.
          ls_asset-content = zcl_abapgit_convert=>string_to_xstring( iv_inline ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_prev EXPORTING previous = error.
    ENDTRY.

    DELETE mt_asset_register WHERE url = iv_url.
    " TODO: Maybe forbid overwriting cacheable assets as they were probably already cached ... arguable
    APPEND ls_asset TO mt_asset_register.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ri_asset_manager TYPE /apmg/cl_apm_gui_asset_manager.
  ENDMETHOD.


  METHOD get_mime_asset.

    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE STANDARD TABLE OF w3mime,
          ls_w3mime LIKE LINE OF lt_w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_w3mime INTO ls_w3mime.
      CONCATENATE rv_xdata ls_w3mime-line INTO rv_xdata IN BYTE MODE.
    ENDLOOP.
    rv_xdata = rv_xdata(lv_size).

  ENDMETHOD.


  METHOD load_asset.

    MOVE-CORRESPONDING is_asset_entry TO rs_asset.
    IF rs_asset-content IS INITIAL AND is_asset_entry-mime_name IS NOT INITIAL.
      " inline content has the priority
      rs_asset-content = get_mime_asset( is_asset_entry-mime_name ).
    ENDIF.
    IF rs_asset-content IS INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |failed to load GUI asset: { is_asset_entry-url }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
