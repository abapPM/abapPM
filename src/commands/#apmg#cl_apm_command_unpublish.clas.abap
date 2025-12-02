CLASS /apmg/cl_apm_command_unpublish DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Unpublish Command
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        !registry TYPE string
        !name     TYPE string
        !version  TYPE string OPTIONAL
      RAISING
        /apmg/cx_apm_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS execute
      IMPORTING
        !registry TYPE string
        !name     TYPE string
        !version  TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS get_tarball
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS remove_version
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
        !version      TYPE string
        !tarball      TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    METHODS update_dist_tags
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
        !version      TYPE string
      RETURNING
        VALUE(result) TYPE /apmg/if_apm_types=>ty_packument
      RAISING
        /apmg/cx_apm_error.

    METHODS delete_tarball
      IMPORTING
        !registry     TYPE string
        !packument    TYPE /apmg/if_apm_types=>ty_packument
        !tarball      TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS unpublish_complete_package
      IMPORTING
        !registry     TYPE string
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

    METHODS unpublish_package_version
      IMPORTING
        !registry     TYPE string
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string
      RAISING
        /apmg/cx_apm_error.

ENDCLASS.



CLASS /apmg/cl_apm_command_unpublish IMPLEMENTATION.


  METHOD delete_tarball.

    DATA(response) = /apmg/cl_apm_command_utils=>fetch_registry(
      registry = registry
      url      = |{ tarball }/-rev/{ packument-_rev }|
      method   = /apmg/if_apm_http_agent=>c_method-delete ).

    result = /apmg/cl_apm_command_utils=>check_response(
      response = response
      text     = 'Error deleting tarball' ).

  ENDMETHOD.


  METHOD execute.

    " 1. Get packument from registry
    DATA(packument) = /apmg/cl_apm_command_utils=>get_packument_from_registry(
      registry = registry
      name     = name
      write    = abap_true ).

    IF version IS INITIAL.

      " 2a. Delete complete package
      DATA(message) = unpublish_complete_package(
        registry  = registry
        packument = packument ).

      IF message IS NOT INITIAL.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
      ENDIF.

      MESSAGE 'Complete package unpublished successfully' TYPE 'S'.

    ELSE.

      " 2b. Get tarball name
      DATA(tarball) = get_tarball(
        packument = packument
        version   = version ).

      " 3. Remove version from packument
      packument = remove_version(
        packument = packument
        version   = version
        tarball   = tarball ).

      " 4. Update LATEST dist-tag (and others)
      packument = update_dist_tags(
        packument = packument
        version   = version ).

      " 5. Unpublish package from registry
      message = unpublish_package_version(
        registry  = registry
        packument = packument ).

      IF message IS NOT INITIAL.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
      ENDIF.

      " 6. Delete tarball from registry
      message = delete_tarball(
        registry  = registry
        packument = packument
        tarball   = tarball ).

      IF message IS NOT INITIAL.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text EXPORTING text = message.
      ENDIF.

      MESSAGE 'Package version unpublished successfully' TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD get_tarball.

    READ TABLE packument-versions ASSIGNING FIELD-SYMBOL(<version>) WITH KEY key = version.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Version { version } does not exist in package { packument-name }|.
    ENDIF.

    result = <version>-manifest-dist-tarball.

  ENDMETHOD.


  METHOD remove_version.

    result = packument.

    DELETE result-versions WHERE key = version.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Version { version } does not exist in package { packument-name }|.
    ENDIF.

    IF lines( result-versions ) = 0.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Version { version } is the last version of package { packument-name }. You may unpublish the complete package, instead|.
    ENDIF.

    DELETE result-time WHERE key = version ##SUBRC_OK.

    DELETE result-_attachments WHERE key = tarball.

  ENDMETHOD.


  METHOD run.

    DATA(command) = NEW /apmg/cl_apm_command_unpublish( ).

    command->execute(
      registry = registry
      name     = name
      version  = version ).

  ENDMETHOD.


  METHOD unpublish_complete_package.

    DATA(response) = /apmg/cl_apm_command_utils=>fetch_registry(
      registry = registry
      url      = |{ registry }/{ packument-name }/-rev/{ packument-_rev }|
      method   = /apmg/if_apm_http_agent=>c_method-delete ).

    result = /apmg/cl_apm_command_utils=>check_response(
      response = response
      text     = 'Error unpublishing complete package' ).

  ENDMETHOD.


  METHOD unpublish_package_version.

    DATA(payload) = /apmg/cl_apm_pacote=>convert_packument_to_json( packument ).

    DATA(response) = /apmg/cl_apm_command_utils=>fetch_registry(
      registry = registry
      url      = |{ registry }/{ packument-name }/-rev/{ packument-_rev }|
      method   = /apmg/if_apm_http_agent=>c_method-put
      payload  = payload ).

    result = /apmg/cl_apm_command_utils=>check_response(
      response = response
      text     = 'Error unpublishing package version' ).

  ENDMETHOD.


  METHOD update_dist_tags.

    " TODO
    DATA(ver) = version ##NEEDED.
    result = packument.

  ENDMETHOD.
ENDCLASS.
