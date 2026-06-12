CLASS /apmg/cl_apm_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Auth
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_activity,
        create  TYPE activ_auth VALUE '01',
        change  TYPE activ_auth VALUE '02',
        display TYPE activ_auth VALUE '03',
        delete  TYPE activ_auth VALUE '06',
      END OF c_activity.

    CLASS-METHODS is_package_authorized
      IMPORTING
        package       TYPE devclass
        activity      TYPE activ_auth
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS check_package_allowed
      IMPORTING
        package       TYPE devclass
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS check_package_authorized
      IMPORTING
        package  TYPE devclass
        activity TYPE activ_auth
      RAISING
        /apmg/cx_apm_error.

    CLASS-METHODS check_transport_required
      IMPORTING
        package       TYPE devclass
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS is_package_allowed
      IMPORTING
        package       TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_package_transported
      IMPORTING
        package       TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS _get_activity_text
      IMPORTING
        activity      TYPE activ_auth
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_auth IMPLEMENTATION.


  METHOD check_package_allowed.

    CHECK package IS NOT INITIAL.

    TRY.
        zcl_abapgit_factory=>get_sap_package( package )->validate_name( ).
      CATCH zcx_abapgit_exception INTO DATA(error).
        result = error->get_text( ).
        RETURN.
    ENDTRY.

    IF NOT is_package_allowed( package ).
      result = |Package { package } not allowed (SAP is responsible user)|.
    ENDIF.

  ENDMETHOD.


  METHOD check_package_authorized.

    IF NOT is_package_authorized( package = package activity = activity ).
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Not authorized to { _get_activity_text( activity ) } package { package }|.
    ENDIF.

  ENDMETHOD.


  METHOD check_transport_required.

    CHECK package IS NOT INITIAL.

    IF is_package_transported( package ).
      result = 'A transport is required for this package'.
    ENDIF.

    " TODO: Potentially check type of transport required and auth to use transport

  ENDMETHOD.


  METHOD is_package_allowed.

    " Check if package owned by SAP is allowed (new packages are ok, since they are created automatically)
    DATA(username) = zcl_abapgit_factory=>get_sap_package( package )->read_responsible( ).

    " TODO: This uses abapGit exit. Replace with apm logic
    result = xsdbool( username <> 'SAP' OR zcl_abapgit_factory=>get_environment( )->is_sap_object_allowed( ) = abap_true ).

  ENDMETHOD.


  METHOD is_package_authorized.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
      ID 'DEVCLASS' FIELD package
      ID 'OBJTYPE' FIELD 'DEVC'
      ID 'OBJNAME' FIELD package
      ID 'P_GROUP' FIELD '*'
      ID 'ACTVT' FIELD activity.

    result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_package_transported.

    TRY.
        result = xsdbool( zcl_abapgit_factory=>get_sap_package( package )->are_changes_recorded_in_tr_req( ) = abap_true ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD _get_activity_text.

    SELECT SINGLE ltext FROM tactt INTO @result WHERE spras = @sy-langu AND actvt = @activity.
    IF sy-subrc <> 0.
      result = '<undefined>' ##NO_TEXT.
    ENDIF.

    result = to_lower( result ).

  ENDMETHOD.
ENDCLASS.
