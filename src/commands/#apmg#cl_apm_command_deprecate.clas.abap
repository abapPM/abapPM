CLASS /apmg/cl_apm_command_deprecate DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Deprecate
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Note: Use empty message to undeprecate package or version
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !range        TYPE string OPTIONAL
        !message_text TYPE string OPTIONAL
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS execute
      IMPORTING
        !registry     TYPE string
        !name         TYPE string
        !range        TYPE string
        !message_text TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS update_deprecate_message
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
        !range        TYPE string
        !message_text TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    METHODS deprecate_package_version
      IMPORTING
        !registry     TYPE string
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_deprecate IMPLEMENTATION.


  METHOD deprecate_package_version.

    DATA(payload) = /apmg/cl_apm_pacote=>convert_packument_to_json(
      packument     = packument
      is_deprecated = abap_true ).

    DATA(response) = /apmg/cl_apm_command_utils=>fetch_registry(
      registry = registry
      url      = |{ registry }/{ packument-name }/-rev/{ packument-_rev }|
      method   = /apmg/if_apm_http_agent=>c_method-put
      payload  = payload ).

    result = /apmg/cl_apm_command_utils=>check_response(
      response = response
      text     = 'Error deprecating package version' ).

  ENDMETHOD.


  METHOD execute.

    " 1. Get packument from registry
    DATA(packument) = /apmg/cl_apm_command_utils=>get_packument_from_registry(
      registry = registry
      name     = name
      write    = abap_true ).

    packument = update_deprecate_message(
      packument    = packument
      range        = range
      message_text = message_text ).

    DATA(message) = deprecate_package_version(
      registry  = registry
      packument = packument ).

    IF message IS NOT INITIAL.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
    ENDIF.

    MESSAGE 'Package version(s) deprecated successfully' TYPE 'S'.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_deprecate( ).

    command->execute(
      registry     = registry
      name         = name
      range        = range
      message_text = message_text ).

  ENDMETHOD.


  METHOD update_deprecate_message.

    result = packument.

    " TODO: Log all deprecated versions
    LOOP AT result-versions ASSIGNING FIELD-SYMBOL(<version>).
      DATA(satisfies) = /apmg/cl_apm_semver_functions=>satisfies(
        version = <version>-key
        range   = range ).

      IF satisfies = abap_true.
        " Note: Empty text will undeprecate
        <version>-manifest-deprecated = message_text.
      ENDIF.
    ENDLOOP.

    " Remove attachments which distinguishes this update from publishing a package
    DELETE result-_attachments WHERE key IS NOT INITIAL.

  ENDMETHOD.
ENDCLASS.
