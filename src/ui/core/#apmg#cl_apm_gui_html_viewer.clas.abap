CLASS /apmg/cl_apm_gui_html_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_html_viewer.

    METHODS constructor
      IMPORTING
        !io_container           TYPE REF TO cl_gui_container DEFAULT cl_gui_container=>screen0
        !iv_disable_query_table TYPE abap_bool DEFAULT abap_true.

  PROTECTED SECTION.

    DATA mo_html_viewer TYPE REF TO cl_gui_html_viewer.

    METHODS on_event
      FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
        !action
        !frame
        !getdata
        !postdata
        !query_table.

  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_gui_html_viewer IMPLEMENTATION.


  METHOD /apmg/if_apm_html_viewer~back.

    mo_html_viewer->go_back( ).

  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~close_document.

    mo_html_viewer->close_document( ).

  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~free.

    mo_html_viewer->free( ).

  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~get_url.

    DATA lv_url TYPE c LENGTH 250.
    mo_html_viewer->get_current_url( IMPORTING url = lv_url ).
    cl_gui_cfw=>flush( ).
    rv_url = lv_url.

  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~load_data.

    DATA lv_url TYPE c LENGTH 250.
    DATA lv_assigned TYPE c LENGTH 250.

    ASSERT strlen( iv_url ) <= 250.
    lv_url = iv_url.
    mo_html_viewer->load_data(
      EXPORTING
        url                    = lv_url
        type                   = iv_type
        subtype                = iv_subtype
        size                   = iv_size
      IMPORTING
        assigned_url           = lv_assigned
      CHANGING
        data_table             = ct_data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        " html_syntax_notcorrect = 4  " not in lower releases
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Error loading data for HTML viewer'.
    ENDIF.
    ev_assigned_url = lv_assigned.

  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~set_focus.
    cl_gui_control=>set_focus(
      EXPORTING
        control           = mo_html_viewer
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Error in: cl_gui_control=>set_focus - SUBRC = { sy-subrc }|.
    ENDIF.
  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~set_registered_events.

    mo_html_viewer->set_registered_events(
      EXPORTING
        events                    = it_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Error registering events for HTML viewer'.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~set_visiblity.
    DATA: lv_visible TYPE c LENGTH 1.

    IF iv_visible = abap_true.
      lv_visible = cl_gui_container=>visible_true.
    ELSE.
      lv_visible = cl_gui_container=>visible_false.
    ENDIF.

    mo_html_viewer->set_visible( lv_visible ).
  ENDMETHOD.


  METHOD /apmg/if_apm_html_viewer~show_url.

    DATA lv_url TYPE c LENGTH 250.
    lv_url = iv_url.
    mo_html_viewer->show_url(
      EXPORTING
        url                    = lv_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = 'Error showing URL in HTML viewer'.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = iv_disable_query_table
        parent               = io_container.

    ls_event-eventid    = /apmg/if_apm_html_viewer=>c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mo_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mo_html_viewer.

  ENDMETHOD.


  METHOD on_event.

    RAISE EVENT /apmg/if_apm_html_viewer~sapevent
      EXPORTING
        action      = action
        frame       = frame
        getdata     = getdata
        postdata    = postdata
        query_table = query_table.

  ENDMETHOD.
ENDCLASS.
