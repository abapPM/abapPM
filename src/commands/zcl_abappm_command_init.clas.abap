CLASS zcl_abappm_command_init DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !iv_package      TYPE devclass
        !is_package_json TYPE zif_abappm_package_json_types=>ty_package_json
      RAISING
        zcx_abappm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_command_init IMPLEMENTATION.


  METHOD run.

    DATA:
      lv_markdown     TYPE string,
      ls_package_json TYPE zif_abappm_package_json_types=>ty_package_json,
      li_package_json TYPE REF TO zif_abappm_package_json,
      li_readme       TYPE REF TO zif_abappm_readme.

    " Package JSON
    li_package_json = zcl_abappm_package_json=>factory(
      iv_package = iv_package
      iv_name    = is_package_json-name
      iv_version = is_package_json-version ).

    IF li_package_json->exists( ) = abap_true.
      zcx_abappm_error=>raise( |Package { iv_package } is already initialized| ).
    ENDIF.

    " Remove readme which is stored separately
    ls_package_json = is_package_json.
    CLEAR ls_package_json-readme.

    li_package_json->set( ls_package_json )->save( ).

    " Readme
    IF is_package_json-readme IS INITIAL.
      lv_markdown = |# { is_package_json-name } - { is_package_json-description }|.
    ELSE.
      lv_markdown = is_package_json-readme.
      lv_markdown = replace(
        val   = lv_markdown
        sub   = '\n'
        with  = cl_abap_char_utilities=>newline
        occ   = 0 ).
    ENDIF.

    li_readme = zcl_abappm_readme=>factory(
      iv_package  = iv_package
      iv_markdown = lv_markdown ).

    li_readme->save( ).

    MESSAGE 'Package successfully initialized' TYPE 'S'.

  ENDMETHOD.
ENDCLASS.
