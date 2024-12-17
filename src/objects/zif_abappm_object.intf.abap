INTERFACE zif_abappm_object PUBLIC.

  TYPES:
    BEGIN OF ty_item,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
    END OF ty_item.

  METHODS import
    IMPORTING
      !new_package   TYPE devclass
      !new_object    TYPE tadir-obj_name
      !files         TYPE REF TO zcl_abappm_file_importer
      !map           TYPE zif_abappm_code_importer=>ty_map
      !is_dryrun     TYPE abap_bool DEFAULT abap_true
      !is_production TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
