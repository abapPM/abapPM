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

    METHODS find_latest_version
      IMPORTING
        !time         TYPE /apmg/if_apm_types=>ty_packument-time
      RETURNING
        VALUE(result) TYPE string.

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

    " 1. Get packument from registry for update
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


  METHOD find_latest_version.

    " Note: Returns the new latest version when sorted by timestamp. This can become confusing, if
    " versions are not published sequentially, for example when using pre-releases for a new major
    " mixed with patches for minor releases. It might be better to find the predecesor using
    " semver-sorting.

    DATA(time_list) = time.

    DELETE time_list
      WHERE key = /apmg/if_apm_types=>c_time_entries-created OR key = /apmg/if_apm_types=>c_time_entries-modified.

    SORT time_list BY timestamp DESCENDING.

    result = time_list[ 1 ]-key.

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
          text = |Version { version } is the only version of package { packument-name }. | &&
                 |You may unpublish the complete package, instead|.
    ENDIF.

    DELETE result-time WHERE key = version ##SUBRC_OK.

    " Must have at last "created" and "modified" entries
    IF lines( result-time ) < 2.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Inconsistent time list for package { packument-name }.|.
    ENDIF.

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

    result = packument.

    LOOP AT result-dist_tags ASSIGNING FIELD-SYMBOL(<dist_tag>) WHERE value = version.
      IF <dist_tag>-key = /apmg/if_apm_types=>c_latest_version.
        <dist_tag>-value = find_latest_version( result-time ).
      ELSE.
        DELETE result-dist_tags INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
