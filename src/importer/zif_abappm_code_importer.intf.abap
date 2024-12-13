INTERFACE zif_abappm_code_importer PUBLIC.

  CONSTANTS:
    c_default_import_rule TYPE string VALUE '(?:\/.*\/|Y|Z)(..)(.*)'.

  TYPES:
    BEGIN OF ty_program,
      program        TYPE progname,
      package        TYPE devclass,
      source_package TYPE devclass,
    END OF ty_program,
    ty_programs TYPE STANDARD TABLE OF ty_program WITH KEY program.

  TYPES:
    BEGIN OF ty_map_item,
      object_type    TYPE tadir-object,
      old_object     TYPE tadir-obj_name,
      new_object     TYPE tadir-obj_name,
      source_package TYPE devclass,
      target_package TYPE devclass,
      name           TYPE string,
      version        TYPE string,
    END OF ty_map_item,
    ty_map TYPE STANDARD TABLE OF ty_map_item WITH KEY old_object.

  TYPES:
    ty_object_types TYPE RANGE OF tadir-object,
    ty_object_names TYPE RANGE OF tadir-obj_name.

ENDINTERFACE.
