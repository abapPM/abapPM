INTERFACE zif_abappm_importer PUBLIC.

  " Examples for class and interface regex matching
  " YCL_TEST -> $1 = CL, $2 = TEST
  " ZIF_TEST -> $1 = IF, $2 = TEST
  " /APMG/CL_TEST, $1 = CL, $2 = TEST
  " /APMG/IF_TEST, $1 = IF, $2 = TEST
  CONSTANTS c_default_import_rule TYPE string VALUE '(?:\/.+\/|Y|Z)(..)(.*)'.

  CONSTANTS:
    BEGIN OF c_action,
      none   TYPE string VALUE 'none',
      add    TYPE string VALUE 'add',
      remove TYPE string VALUE 'remove',
      update TYPE string VALUE 'update',
    END OF c_action.

  TYPES:
    BEGIN OF ty_dependency,
      name    TYPE string,
      version TYPE string,   " installed version
      package TYPE devclass, " location of installed version
      range   TYPE string,   " from dependencies{ }
      action  TYPE string,
    END OF ty_dependency,
    ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH KEY name.

  TYPES:
    BEGIN OF ty_program,
      program TYPE progname,
      package TYPE devclass,
    END OF ty_program,
    ty_programs TYPE STANDARD TABLE OF ty_program WITH KEY program.

  TYPES:
    BEGIN OF ty_rule,
      old_object     TYPE string,
      new_object     TYPE string,
      target_package TYPE string,
      name           TYPE string,
      version        TYPE string,
    END OF ty_rule,
    ty_rules TYPE STANDARD TABLE OF ty_rule WITH KEY old_object new_object target_package.

  TYPES:
    BEGIN OF ty_package,
      name           TYPE string,
      version        TYPE string,
      source_package TYPE devclass,
      target_package TYPE devclass,
    END OF ty_package,
    ty_packages TYPE STANDARD TABLE OF ty_package WITH KEY name.

  TYPES:
    BEGIN OF ty_item,
      obj_type TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
    END OF ty_item.

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
    ty_object_names TYPE RANGE OF tadir-obj_name,
    ty_code         TYPE rswsourcet.

ENDINTERFACE.
