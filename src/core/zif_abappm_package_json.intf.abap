INTERFACE zif_abappm_package_json PUBLIC.

  TYPES:
    ty_email TYPE string,
    ty_uri   TYPE string,
    BEGIN OF ty_person,
      name  TYPE string,
      url   TYPE ty_uri,
      email TYPE ty_email,
    END OF ty_person,
    BEGIN OF ty_dependency,
      name    TYPE string,
      version TYPE string,
    END OF ty_dependency,
    ty_funding_url TYPE ty_uri,
    BEGIN OF ty_funding_way,
      url  TYPE ty_funding_url,
      type TYPE string,
    END OF ty_funding_way,
    BEGIN OF ty_other_property,
      _ TYPE string,
    END OF ty_other_property,
    BEGIN OF ty_package_json,
      name                  TYPE string, " min 3, max 214
      version               TYPE string,
      description           TYPE string,
      keywords              TYPE string_table,
      homepage              TYPE string,
      BEGIN OF bugs,
        url   TYPE ty_uri,
        email TYPE ty_email,
      END OF bugs,
      license               TYPE string,
      author                TYPE ty_person,
      contributors          TYPE STANDARD TABLE OF ty_person WITH KEY name,
      maintainers           TYPE STANDARD TABLE OF ty_person WITH KEY name,
      main                  TYPE string,
      type                  TYPE string,
      BEGIN OF repository,
        type      TYPE string,
        url       TYPE ty_uri,
        directory TYPE string,
      END OF repository,
      funding_url           TYPE ty_funding_url,
      funding_way           TYPE ty_funding_way,
      dependencies          TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      dev_dependencies      TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      optional_dependencies TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      bundled_dependencies  TYPE string_table,
      package_manager       TYPE string,
      engines               TYPE STANDARD TABLE OF ty_dependency WITH DEFAULT KEY,
      os                    TYPE string_table,
      cpu                   TYPE string_table,
      db                    TYPE string_table,
      private               TYPE abap_bool,
      BEGIN OF dist,
        shasum  TYPE string,
        tarball TYPE string,
      END OF dist,
      readme                TYPE string,
      _                     TYPE STANDARD TABLE OF ty_other_property WITH DEFAULT KEY,
    END OF ty_package_json.

  CONSTANTS:
    BEGIN OF c_package_file,
      obj_name  TYPE c LENGTH 7 VALUE 'package',
      sep1      TYPE c LENGTH 1 VALUE '.',
      obj_type  TYPE c LENGTH 4 VALUE 'devc',
      sep2      TYPE c LENGTH 1 VALUE '.',
      extra     TYPE c LENGTH 3 VALUE 'apm',
      sep3      TYPE c LENGTH 1 VALUE '.',
      extension TYPE c LENGTH 4 VALUE 'json',
    END OF c_package_file.

  METHODS get
    RETURNING
      VALUE(result) TYPE REF TO zcl_ajson.

  METHODS get_string
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !io_json TYPE REF TO zcl_ajson.

  METHODS set_string
    IMPORTING
      !iv_json TYPE string.

  METHODS load.

  METHODS save.

  METHODS delete.

ENDINTERFACE.
