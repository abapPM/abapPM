CLASS lcl_rename DEFINITION.

  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapper.

    METHODS constructor
      IMPORTING
        it_rename_map TYPE /apmg/if_apm_ajson_mapper~tty_rename_map
        iv_rename_by  TYPE i.

  PRIVATE SECTION.
    DATA mt_rename_map TYPE /apmg/if_apm_ajson_mapper~tty_rename_map.
    DATA mv_rename_by TYPE i.

ENDCLASS.

CLASS lcl_mapping_to_upper DEFINITION.

  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapper.

ENDCLASS.


CLASS lcl_mapping_to_lower DEFINITION.

  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapper.

ENDCLASS.


CLASS lcl_compound_mapper DEFINITION.

  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapper.

    METHODS constructor
      IMPORTING
        it_queue TYPE /apmg/if_apm_ajson_mapper=>ty_table_of.

  PRIVATE SECTION.
    DATA mt_queue TYPE /apmg/if_apm_ajson_mapper=>ty_table_of.

ENDCLASS.

CLASS lcl_to_snake DEFINITION.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapper.
ENDCLASS.

CLASS lcl_to_camel DEFINITION.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_mapper.
    METHODS constructor
      IMPORTING
        iv_first_json_upper TYPE abap_bool.
  PRIVATE SECTION.
    DATA mv_first_json_upper TYPE abap_bool.
ENDCLASS.
