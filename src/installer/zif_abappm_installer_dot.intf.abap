INTERFACE zif_abappm_installer_dot PUBLIC.

  TYPES:
  " Former APACK
    BEGIN OF ty_dependency,
      name           TYPE string,
      version        TYPE string,
      sem_version    TYPE zif_abapgit_definitions=>ty_version,
      git_url        TYPE string,
      target_package TYPE devclass,
    END OF ty_dependency,
    ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH NON-UNIQUE DEFAULT KEY.

  TYPES:
    BEGIN OF ty_descriptor,
      name           TYPE string,
      version        TYPE string,
      sem_version    TYPE zif_abapgit_definitions=>ty_version,
      description    TYPE string,
      git_url        TYPE string,
      target_package TYPE devclass,
      logo           TYPE string,
    END OF ty_descriptor.

  TYPES
    BEGIN OF ty_packaging.
      INCLUDE TYPE ty_descriptor.
  TYPES:
      dependencies TYPE ty_dependencies,
    END OF ty_packaging.

ENDINTERFACE.
