INTERFACE /apmg/if_apm_command PUBLIC.

************************************************************************
* apm Command Constants & Types
*
* Copyright 2026 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

  CONSTANTS:
    BEGIN OF c_access,
      public     TYPE string VALUE 'public',
      restricted TYPE string VALUE 'restricted',
      private    TYPE string VALUE 'private', " alias for restricted
    END OF c_access.

ENDINTERFACE.
