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

    CLASS-METHODS is_package_allowed
      IMPORTING
        package       TYPE devclass
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /apmg/cl_apm_auth IMPLEMENTATION.


  METHOD is_package_allowed.

    " Check if package owned by SAP is allowed (new packages are ok, since they are created automatically)
    DATA(username) = zcl_abapgit_factory=>get_sap_package( package )->read_responsible( ).

    " TODO: This uses abapGit exit. Replace with apm logic
    result = xsdbool( username <> 'SAP' OR zcl_abapgit_factory=>get_environment( )->is_sap_object_allowed( ) = abap_true ).

  ENDMETHOD.
ENDCLASS.
