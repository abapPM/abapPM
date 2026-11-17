INTERFACE /apmg/if_apm_ajson_mapper
  PUBLIC.

  TYPES:
    BEGIN OF ty_rename,
      from TYPE string,
      to   TYPE string,
    END OF ty_rename,
    tty_rename_map TYPE STANDARD TABLE OF ty_rename
      WITH UNIQUE SORTED KEY by_name COMPONENTS from.

  TYPES:
    ty_table_of TYPE STANDARD TABLE OF REF TO /apmg/if_apm_ajson_mapper.

  METHODS rename_node
    IMPORTING
      !is_node TYPE /apmg/if_apm_ajson_types=>ty_node
    CHANGING
      !cv_name TYPE /apmg/if_apm_ajson_types=>ty_node-name.

ENDINTERFACE.
