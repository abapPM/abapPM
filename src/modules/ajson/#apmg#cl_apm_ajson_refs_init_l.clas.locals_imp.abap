**********************************************************************
*  INITIALIZE REFS BY PATH
**********************************************************************

CLASS lcl_path_refs_init DEFINITION.
  PUBLIC SECTION.
    INTERFACES /apmg/if_apm_ajson_refs_init.

    METHODS constructor
      IMPORTING
        !it_data_refs TYPE /apmg/if_apm_ajson_refs_init~tty_data_refs.

  PRIVATE SECTION.
    DATA mt_data_refs TYPE /apmg/if_apm_ajson_refs_init~tty_data_refs.
ENDCLASS.

CLASS lcl_path_refs_init IMPLEMENTATION.

  METHOD constructor.
    mt_data_refs = it_data_refs.
  ENDMETHOD.

  METHOD /apmg/if_apm_ajson_refs_init~get_data_ref.

    FIELD-SYMBOLS <data_ref> LIKE LINE OF mt_data_refs.

    READ TABLE mt_data_refs ASSIGNING <data_ref>
      WITH KEY by_path COMPONENTS path = is_node-path name = is_node-name.
    IF sy-subrc = 0.
      ro_ref = <data_ref>-dref.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
