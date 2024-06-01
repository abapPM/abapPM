INTERFACE zif_abappm_semver_options PUBLIC.

************************************************************************
* SemVer Options
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* Ported to ABAP by apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: ISC
************************************************************************

  TYPES:
    BEGIN OF ty_options,
      loose  TYPE abap_bool,
      incpre TYPE abap_bool,
      rtl    TYPE abap_bool,
    END OF ty_options.

ENDINTERFACE.
