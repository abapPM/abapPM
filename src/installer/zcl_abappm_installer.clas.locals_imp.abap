CLASS lcl_abapgit_data_supporter DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_supporter.

ENDCLASS.

CLASS lcl_abapgit_data_supporter IMPLEMENTATION.

  METHOD zif_abapgit_data_supporter~is_object_supported.
    " Allow all MBT tables for updates
    IF iv_type = 'TABU' AND iv_name CP '/MBTOOLS/*'.
      rv_supported = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
