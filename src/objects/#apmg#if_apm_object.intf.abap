INTERFACE /apmg/if_apm_object PUBLIC.

************************************************************************
* apm Import Interface for Objects
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  TYPES ty_item TYPE /apmg/if_apm_importer=>ty_item.

  METHODS import
    IMPORTING
      !new_package   TYPE ty_item-package
      !new_object    TYPE ty_item-obj_name
      !language      TYPE ty_item-language
      !map           TYPE /apmg/if_apm_importer=>ty_map
      !files         TYPE REF TO /apmg/if_apm_file_importer OPTIONAL
      !is_dryrun     TYPE abap_bool DEFAULT abap_true
      !is_production TYPE abap_bool DEFAULT abap_true
    RAISING
      /apmg/cx_apm_error.

ENDINTERFACE.
