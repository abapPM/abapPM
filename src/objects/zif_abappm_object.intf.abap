INTERFACE zif_abappm_object PUBLIC.

  TYPES ty_item TYPE zif_abappm_importer=>ty_item.

  METHODS import
    IMPORTING
      !new_package   TYPE devclass
      !new_object    TYPE tadir-obj_name
      !map           TYPE zif_abappm_importer=>ty_map
      !files         TYPE REF TO zif_abappm_file_importer OPTIONAL
      !is_dryrun     TYPE abap_bool DEFAULT abap_true
      !is_production TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_abappm_error.

ENDINTERFACE.
