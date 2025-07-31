CLASS /apmg/cl_apm_ajson_refs_init_l DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create_path_refs_init
      IMPORTING
        !it_data_refs       TYPE /apmg/if_apm_ajson_refs_init=>tty_data_refs
      RETURNING
        VALUE(ri_refs_init) TYPE REF TO /apmg/if_apm_ajson_refs_init
      RAISING
        /apmg/cx_apm_ajson_error.

ENDCLASS.



CLASS /apmg/cl_apm_ajson_refs_init_l IMPLEMENTATION.


  METHOD create_path_refs_init.
    CREATE OBJECT ri_refs_init TYPE lcl_path_refs_init
      EXPORTING
        it_data_refs = it_data_refs.
  ENDMETHOD.
ENDCLASS.
