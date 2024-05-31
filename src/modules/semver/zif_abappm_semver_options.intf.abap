INTERFACE ZIF_ABAPPM_SEMVER_OPTIONS PUBLIC.

************************************************************************
* SemVer Options
*
* Copyright (c) Isaac Z. Schlueter and Contributors
* ABAP Port by Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: ISC
************************************************************************

  TYPES:
    BEGIN OF ty_options,
      loose  TYPE abap_bool,
      incpre TYPE abap_bool,
      rtl    TYPE abap_bool,
    END OF ty_options.

ENDINTERFACE.
