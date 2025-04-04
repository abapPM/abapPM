INTERFACE zif_abappm_types PUBLIC.

************************************************************************
* apm Types
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
* Schema for package.abap.json
*
* The definition of package.json for apm is closely aligned with npm
* but there are differences! Several fields are not included while
* others have been added.
*
* https://docs.npmjs.com/cli/v10/configuring-npm/package-json
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.

  TYPES:
    "! Email
    ty_email TYPE string,
    "! URI
    ty_uri   TYPE string,
    "! Person
    BEGIN OF ty_person,
      name   TYPE string,
      url    TYPE ty_uri,
      email  TYPE ty_email,
      avatar TYPE ty_uri,
    END OF ty_person,
    "! List of Persons
    ty_persons TYPE STANDARD TABLE OF ty_person WITH KEY name url email,
    "! Dependency with semver range
    BEGIN OF ty_dependency,
      name  TYPE string,
      range TYPE string,
    END OF ty_dependency,
    "! List of Dependencies
    ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH KEY name,
    "! Generic key value pair
    BEGIN OF ty_generic,
      key   TYPE string,
      value TYPE string,
    END OF ty_generic,
    "! Timestamp
    BEGIN OF ty_time,
      key       TYPE string,
      timestamp TYPE timestamp,
    END OF ty_time,
    "! Signature
    BEGIN OF ty_signature,
      keyid TYPE string,
      sig   TYPE string,
    END OF ty_signature,
    "! User Rating
    BEGIN OF ty_user,
      name  TYPE string,
      stars TYPE i,
    END OF ty_user,
    "! Dist Details
    BEGIN OF ty_dist,
      file_count    TYPE i,
      integrity     TYPE string,
      shasum        TYPE string,
      signatures    TYPE STANDARD TABLE OF ty_signature WITH KEY keyid,
      tarball       TYPE string,
      unpacked_size TYPE i,
    END OF ty_dist.

  " *** PACKAGE.ABAP.JSON ***

  TYPES:
    "! Schema for package.abap.json
    BEGIN OF ty_package_json,
      name                  TYPE string,
      version               TYPE string,
      description           TYPE string,
      type                  TYPE string,
      keywords              TYPE string_table,
      homepage              TYPE string,
      BEGIN OF bugs,
        url   TYPE ty_uri,
        email TYPE ty_email,
      END OF bugs,
      license               TYPE string,
      author                TYPE ty_person,
      contributors          TYPE ty_persons,
      maintainers           TYPE ty_persons,
      main                  TYPE string,
      man                   TYPE string_table,
      BEGIN OF repository,
        type      TYPE string,
        url       TYPE ty_uri,
        directory TYPE string,
      END OF repository,
      BEGIN OF funding,
        type TYPE string,
        url  TYPE ty_uri,
      END OF funding,
      dependencies          TYPE ty_dependencies,
      dev_dependencies      TYPE ty_dependencies,
      optional_dependencies TYPE ty_dependencies,
      peer_dependencies     TYPE ty_dependencies,
      bundle_dependencies   TYPE string_table,
      engines               TYPE ty_dependencies,
      os                    TYPE string_table,
      cpu                   TYPE string_table,
      db                    TYPE string_table,
      private               TYPE abap_bool,
      readme                TYPE string,
    END OF ty_package_json.

  " *** MANIFEST ***

  "! Full manifest (fetched with "accept: application/json" in HTTP headers)
  TYPES BEGIN OF ty_manifest.
  INCLUDE TYPE ty_package_json.
  TYPES:
    dist           TYPE ty_dist,
    deprecated     TYPE abap_bool,
    __id           TYPE string, " external: _id
    __abap_version TYPE string, " external: _abapVersion
    __apm_version  TYPE string, " external: _apmVersion
    END OF ty_manifest.

  TYPES:
    "! Abbreviated manifest
    "! (fetched with "accept: application/vnd.npm.install-v1+json" in the HTTP headers)
    BEGIN OF ty_manifest_abbreviated ##NEEDED,
      name                  TYPE string,
      version               TYPE string,
      dependencies          TYPE ty_dependencies,
      dev_dependencies      TYPE ty_dependencies,
      optional_dependencies TYPE ty_dependencies,
      peer_dependencies     TYPE ty_dependencies,
      bundle_dependencies   TYPE string_table,
      engines               TYPE ty_dependencies,
      os                    TYPE string_table,
      cpu                   TYPE string_table,
      db                    TYPE string_table,
      dist                  TYPE ty_dist,
      deprecated            TYPE abap_bool,
    END OF ty_manifest_abbreviated.

  " *** PACKUMENT ***

  TYPES:
    "! Version Manifest
    BEGIN OF ty_version,
      key     TYPE string,
      version TYPE ty_manifest,
    END OF ty_version.

  TYPES:
    "! Tarball Attachment
    BEGIN OF ty_attachment,
      key TYPE string,
      BEGIN OF tarball,
        content_type TYPE string,
        data         TYPE string,
        length       TYPE i,
      END OF tarball,
    END OF ty_attachment.

  TYPES:
    "! Full packument (as fetched from registry)
    "! Some fields are hoisted from latest version to root
    BEGIN OF ty_packument ##NEEDED,
      name          TYPE string,
      description   TYPE string,
      dist_tags     TYPE STANDARD TABLE OF ty_generic WITH KEY key,
      time          TYPE STANDARD TABLE OF ty_time WITH KEY key,
      versions      TYPE STANDARD TABLE OF ty_version WITH KEY key,
      maintainers   TYPE ty_persons,
      readme        TYPE string,
      users         TYPE STANDARD TABLE OF ty_user WITH KEY name,
      homepage      TYPE string,
      BEGIN OF bugs,
        url   TYPE ty_uri,
        email TYPE ty_email,
      END OF bugs,
      license       TYPE string,
      keywords      TYPE string_table,
      author        TYPE ty_person,
      BEGIN OF repository,
        type      TYPE string,
        url       TYPE ty_uri,
        directory TYPE string,
      END OF repository,
      __id          TYPE string,
      __rev         TYPE string,
      __attachments TYPE STANDARD TABLE OF ty_attachment WITH KEY key,
      access        TYPE string,
    END OF ty_packument.


  CONSTANTS:
    "! Package Name Specs
    BEGIN OF c_package_name,
      min_length TYPE i VALUE 3,
      max_length TYPE i VALUE 214,
      regex      TYPE string VALUE '^(?:@[a-z0-9\-*~][a-z0-9\-*._~]*/)?[a-z0-9\-~][a-z0-9\-._~]*$',
    END OF c_package_name.

  CONSTANTS:
    "! Package Manifest File
    BEGIN OF c_package_json_file,
      obj_name  TYPE c LENGTH 7 VALUE 'package',
      sep1      TYPE c LENGTH 1 VALUE '.',
      obj_type  TYPE c LENGTH 4 VALUE 'abap',
      sep2      TYPE c LENGTH 1 VALUE '.',
      extension TYPE c LENGTH 4 VALUE 'json',
    END OF c_package_json_file.

  "! Package Readme File
  CONSTANTS c_readme_file TYPE string VALUE 'README.md'.

  CONSTANTS:
    "! Package Types
    BEGIN OF c_package_type,
      common_abap TYPE string VALUE 'commonabap',
      module      TYPE string VALUE 'module',
    END OF c_package_type.

  CONSTANTS:
    "! Supported Engines
    BEGIN OF c_engine,
      abap TYPE string VALUE 'abap',
      apm  TYPE string VALUE 'apm',
      btp  TYPE string VALUE 'btp',
    END OF c_engine.

  CONSTANTS:
    "! Most Common Licenses (https://spdx.org/licenses/)
    BEGIN OF c_license,
      agpl_3_0_only     TYPE string VALUE 'AGPL-3.0-only',
      apache_2_0        TYPE string VALUE 'Apache-2.0',
      bsd_2_clause      TYPE string VALUE 'BSD-2-Clause',
      bsd_3_clause      TYPE string VALUE 'BSD-3-Clause',
      bsl_1_0           TYPE string VALUE 'BSL-1.0',
      cc0_1_0           TYPE string VALUE 'CC0-1.0',
      cddl_1_0          TYPE string VALUE 'CDDL-1.0',
      cddl_1_1          TYPE string VALUE 'CDDL-1.1',
      epl_1_0           TYPE string VALUE 'EPL-1.0',
      epl_2_0           TYPE string VALUE 'EPL-2.0',
      gpl_2_0_only      TYPE string VALUE 'GPL-2.0-only',
      gpl_3_0_only      TYPE string VALUE 'GPL-3.0-only',
      isc               TYPE string VALUE 'ISC',
      lgpl_2_0_only     TYPE string VALUE 'LGPL-2.0-only',
      lgpl_2_1_only     TYPE string VALUE 'LGPL-2.1-only',
      lgpl_2_1_or_later TYPE string VALUE 'LGPL-2.1-or-later',
      lgpl_3_0_only     TYPE string VALUE 'LGPL-3.0-only',
      lgpl_3_0_or_later TYPE string VALUE 'LGPL-3.0-or-later',
      mit               TYPE string VALUE 'MIT',
      mpl_2_0           TYPE string VALUE 'MPL-2.0',
      ms_pl             TYPE string VALUE 'MS-PL',
      unlicensed        TYPE string VALUE 'UNLICENSED',
    END OF c_license.

  CONSTANTS:
    "! Operating System Platforms
    BEGIN OF c_os,
      aix        TYPE string VALUE 'aix',
      hp_ux      TYPE string VALUE 'hp-ux',
      linux      TYPE string VALUE 'linux',
      ms_windows TYPE string VALUE 'ms-windows',
      solaris    TYPE string VALUE 'solaris',
      os_390     TYPE string VALUE 'os/390',
      os_400     TYPE string VALUE 'os/400',
    END OF c_os.

  CONSTANTS:
    "! Hardware Platforms
    BEGIN OF c_cpu,
      x86_64   TYPE string VALUE 'x86-64',
      power_pc TYPE string VALUE 'power-pc',
      sparc    TYPE string VALUE 'sparc',
    END OF c_cpu.

  CONSTANTS:
    "! Database Platforms
    BEGIN OF c_db,
      db2      TYPE string VALUE 'db2',
      db400    TYPE string VALUE 'db400',
      db6      TYPE string VALUE 'db6',
      hdb      TYPE string VALUE 'hdb',
      informix TYPE string VALUE 'informix',
      mssql    TYPE string VALUE 'mssql',
      oracle   TYPE string VALUE 'oracle',
      sap_db   TYPE string VALUE 'sap-db',
      sybase   TYPE string VALUE 'sybase',
    END OF c_db.

ENDINTERFACE.
