INTERFACE /apmg/if_apm_html PUBLIC.

  TYPES:
    BEGIN OF ty_data_attr,
      name  TYPE string,
      value TYPE string,
    END OF ty_data_attr,
    ty_data_attrs TYPE STANDARD TABLE OF ty_data_attr WITH KEY name,
    ty_table_of   TYPE STANDARD TABLE OF REF TO /apmg/if_apm_html WITH DEFAULT KEY.

  CONSTANTS:
    BEGIN OF c_action_type,
      sapevent  TYPE c VALUE 'E',
      url       TYPE c VALUE 'U',
      onclick   TYPE c VALUE 'C',
      separator TYPE c VALUE 'S',
      dummy     TYPE c VALUE '_',
    END OF c_action_type.

  CONSTANTS:
    BEGIN OF c_html_opt,
      strong   TYPE c VALUE 'E',
      cancel   TYPE c VALUE 'C',
      crossout TYPE c VALUE 'X',
    END OF c_html_opt.

  DATA mv_chunk_title TYPE string READ-ONLY. " Primarily for debug of postponed html parts

  METHODS set_title
    IMPORTING
      !iv_title      TYPE string
    RETURNING
      VALUE(ri_self) TYPE REF TO /apmg/if_apm_html.

  METHODS add
    IMPORTING
      !ig_chunk      TYPE any
    RETURNING
      VALUE(ri_self) TYPE REF TO /apmg/if_apm_html.

  METHODS render
    IMPORTING
      !iv_no_indent_jscss TYPE abap_bool DEFAULT abap_false
      !iv_no_line_breaks  TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rv_html)      TYPE string.

  METHODS is_empty
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

  METHODS add_a
    IMPORTING
      !iv_txt        TYPE string
      !iv_act        TYPE string
      !iv_query      TYPE string OPTIONAL
      !iv_typ        TYPE c DEFAULT c_action_type-sapevent
      !iv_opt        TYPE clike OPTIONAL
      !iv_class      TYPE string OPTIONAL
      !iv_id         TYPE string OPTIONAL
      !iv_style      TYPE string OPTIONAL
      !iv_title      TYPE string OPTIONAL
    RETURNING
      VALUE(ri_self) TYPE REF TO /apmg/if_apm_html.

  METHODS add_checkbox
    IMPORTING
      !iv_id         TYPE string
      !iv_checked    TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(ri_self) TYPE REF TO /apmg/if_apm_html.

  METHODS a
    IMPORTING
      !iv_txt       TYPE string
      !iv_act       TYPE string
      !iv_query     TYPE string OPTIONAL
      !iv_typ       TYPE c DEFAULT /apmg/if_apm_html=>c_action_type-sapevent
      !iv_opt       TYPE clike OPTIONAL
      !iv_class     TYPE string OPTIONAL
      !iv_id        TYPE string OPTIONAL
      !iv_style     TYPE string OPTIONAL
      !iv_title     TYPE string OPTIONAL
    RETURNING
      VALUE(rv_str) TYPE string.

  METHODS icon
    IMPORTING
      !iv_name      TYPE string
      !iv_hint      TYPE string OPTIONAL
      !iv_class     TYPE string OPTIONAL
      !iv_onclick   TYPE string OPTIONAL
    RETURNING
      VALUE(rv_str) TYPE string.

  METHODS add_icon
    IMPORTING
      !iv_name       TYPE string
      !iv_hint       TYPE string OPTIONAL
      !iv_class      TYPE string OPTIONAL
      !iv_onclick    TYPE string OPTIONAL
    RETURNING
      VALUE(ri_self) TYPE REF TO /apmg/if_apm_html.

  METHODS wrap
    IMPORTING
      !iv_tag                TYPE string
      !iv_content            TYPE string OPTIONAL
      !ii_content            TYPE REF TO /apmg/if_apm_html OPTIONAL
      !iv_id                 TYPE string OPTIONAL
      !iv_class              TYPE string OPTIONAL
      !iv_hint               TYPE string OPTIONAL
      !iv_format_single_line TYPE abap_bool DEFAULT abap_false
      !is_data_attr          TYPE ty_data_attr OPTIONAL
      !it_data_attrs         TYPE ty_data_attrs OPTIONAL
    RETURNING
      VALUE(ri_self)         TYPE REF TO /apmg/if_apm_html.

  METHODS td
    IMPORTING
      !iv_content            TYPE string OPTIONAL
      !ii_content            TYPE REF TO /apmg/if_apm_html OPTIONAL
      !iv_id                 TYPE string OPTIONAL
      !iv_class              TYPE string OPTIONAL
      !iv_hint               TYPE string OPTIONAL
      !iv_format_single_line TYPE abap_bool DEFAULT abap_true
      !is_data_attr          TYPE ty_data_attr OPTIONAL
      !it_data_attrs         TYPE ty_data_attrs OPTIONAL
        PREFERRED PARAMETER iv_content
    RETURNING
      VALUE(ri_self)         TYPE REF TO /apmg/if_apm_html.

  METHODS th
    IMPORTING
      !iv_content            TYPE string OPTIONAL
      !ii_content            TYPE REF TO /apmg/if_apm_html OPTIONAL
      !iv_id                 TYPE string OPTIONAL
      !iv_class              TYPE string OPTIONAL
      !iv_hint               TYPE string OPTIONAL
      !iv_format_single_line TYPE abap_bool DEFAULT abap_true
      !is_data_attr          TYPE ty_data_attr OPTIONAL
      !it_data_attrs         TYPE ty_data_attrs OPTIONAL
        PREFERRED PARAMETER iv_content
    RETURNING
      VALUE(ri_self)         TYPE REF TO /apmg/if_apm_html.

  METHODS div
    IMPORTING
      !iv_content    TYPE string OPTIONAL
      !ii_content    TYPE REF TO /apmg/if_apm_html OPTIONAL
      !iv_id         TYPE string OPTIONAL
      !iv_class      TYPE string OPTIONAL
      !is_data_attr  TYPE ty_data_attr OPTIONAL
      !it_data_attrs TYPE ty_data_attrs OPTIONAL
        PREFERRED PARAMETER iv_content
    RETURNING
      VALUE(ri_self) TYPE REF TO /apmg/if_apm_html.

ENDINTERFACE.
