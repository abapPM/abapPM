INTERFACE zif_abappm_installer_def PUBLIC.

  " TODO: apm doesn't need all of this...

  CONSTANTS:
    c_tabname         TYPE tabname VALUE 'ZABAPINST' ##NO_TEXT,
    c_lock            TYPE viewname VALUE 'EZABAPINST' ##NO_TEXT,
    c_english         TYPE sy-langu VALUE 'E' ##NO_TEXT,
    c_prog_developer  TYPE progname VALUE 'ZABAPINST_DEV' ##NO_TEXT,
    c_prog_standalone TYPE progname VALUE 'ZABAPINST' ##NO_TEXT,
    c_url_docs        TYPE string VALUE 'https://github.com/abapGit/abapinst' ##NO_TEXT,
    c_url_license     TYPE string VALUE 'https://github.com/abapGit/abapinst/blob/master/LICENSE' ##NO_TEXT,
    c_url_repo        TYPE string VALUE 'https://github.com/abapGit/abapinst' ##NO_TEXT.

  " Avoids warning due to key length
  CONSTANTS c_name_length TYPE i VALUE 90.

  TYPES:
    ty_name TYPE c LENGTH c_name_length,
    ty_pack TYPE devclass.

  TYPES:
    BEGIN OF ty_content,
      name TYPE ty_name,
      pack TYPE ty_pack,
      json TYPE string,
    END OF ty_content,
    ty_contents TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY name pack.

  TYPES:
    BEGIN OF ty_version,
      major           TYPE i,
      minor           TYPE i,
      patch           TYPE i,
      prerelase       TYPE string,
      prerelase_patch TYPE i,
    END OF ty_version.

  TYPES:
    BEGIN OF ty_inst,
      name            TYPE ty_name,
      pack            TYPE devclass,
      version         TYPE string,
      sem_version     TYPE ty_version,
      status          TYPE icon_d,
      description     TYPE string,
      source_type     TYPE string,
      source_name     TYPE string,
      transport       TYPE trkorr,
      folder_logic    TYPE string,
      installed_langu TYPE sy-langu,
      installed_by    TYPE xubname,
      installed_at    TYPE timestamp,
      updated_by      TYPE xubname,
      updated_at      TYPE timestamp,
    END OF ty_inst,
    ty_list TYPE STANDARD TABLE OF ty_inst WITH KEY name pack.

ENDINTERFACE.
