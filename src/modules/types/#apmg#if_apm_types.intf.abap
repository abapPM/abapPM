INTERFACE /apmg/if_apm_types PUBLIC.

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
    "! SAP Package (always upper case)
    ty_devclass TYPE devclass,
    "! Name of package in registry (always lower case)
    ty_name     TYPE string,
    "! Semantic version of package
    ty_version  TYPE string,
    "! Package specification (version, range, tag name, git url, or tarball URL)
    ty_spec     TYPE string ##NEEDED,
    "! Email
    ty_email    TYPE string,
    "! URI
    ty_uri      TYPE string,
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
      key   TYPE string,
      range TYPE string,
    END OF ty_dependency,
    "! List of Dependencies
    ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH KEY key,
    "! Generic key value pair
    BEGIN OF ty_generic,
      key   TYPE string,
      value TYPE string,
    END OF ty_generic,
    ty_dist_tags TYPE STANDARD TABLE OF ty_generic WITH KEY key,
    "! Timestamp
    BEGIN OF ty_time,
      key       TYPE string,
      timestamp TYPE timestampl,
    END OF ty_time,
    ty_times TYPE STANDARD TABLE OF ty_time WITH KEY key,
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
    ty_users TYPE STANDARD TABLE OF ty_user WITH KEY name,
    "! Bugs (Issues)
    BEGIN OF ty_bugs,
      url   TYPE ty_uri,
      email TYPE ty_email,
    END OF ty_bugs,
    "! Repository
    BEGIN OF ty_repository,
      type      TYPE string,
      url       TYPE ty_uri,
      directory TYPE string,
    END OF ty_repository,
    "! Funding
    BEGIN OF ty_funding,
      type TYPE string,
      url  TYPE ty_uri,
    END OF ty_funding,
    "! Dist Details
    BEGIN OF ty_dist,
      file_count    TYPE i,
      shasum        TYPE string,
      tarball       TYPE string,
      unpacked_size TYPE i,
      integrity     TYPE string,
      signatures    TYPE STANDARD TABLE OF ty_signature WITH KEY keyid,
    END OF ty_dist,
    "! SAP Package
    BEGIN OF ty_sap_package,
      default               TYPE ty_devclass,
      software_component    TYPE dlvunit,
      abap_language_version TYPE string,
    END OF ty_sap_package.

  " *** PACKAGE.ABAP.JSON ***

  TYPES:
    "! Schema for package.abap.json
    "! Everything but "icon" and "devclass" is also in regular npm package.json
    BEGIN OF ty_package_json,
      name                  TYPE ty_name,
      version               TYPE ty_version,
      description           TYPE string,
      type                  TYPE string,
      keywords              TYPE string_table,
      homepage              TYPE string,
      icon                  TYPE string,
      bugs                  TYPE ty_bugs,
      license               TYPE string,
      author                TYPE ty_person,
      contributors          TYPE ty_persons,
      maintainers           TYPE ty_persons,
      main                  TYPE string,
      man                   TYPE string_table,
      repository            TYPE ty_repository,
      funding               TYPE ty_funding,
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
      sap_package           TYPE ty_sap_package,
    END OF ty_package_json.

  " *** MANIFEST ***

  "! Full manifest
  "!
  "! fetched with "accept: application/json" in HTTP headers
  TYPES BEGIN OF ty_manifest.
  INCLUDE TYPE ty_package_json.
  TYPES:
    dist          TYPE ty_dist,
    deprecated    TYPE string,
    _id           TYPE string,
    _abap_version TYPE string,
    _apm_version  TYPE string,
    END OF ty_manifest.

  TYPES:
    "! Abbreviated manifest
    "!
    "! fetched with "accept: application/vnd.npm.install-v1+json" in the HTTP headers
    BEGIN OF ty_manifest_abbreviated ##NEEDED,
      name                  TYPE ty_name,
      version               TYPE ty_version,
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
      deprecated            TYPE string,
    END OF ty_manifest_abbreviated.

  " *** PACKUMENT ***

  TYPES:
    "! Version Manifest
    BEGIN OF ty_version_manifest,
      key      TYPE string,
      manifest TYPE ty_manifest,
    END OF ty_version_manifest,
    ty_version_manifests TYPE STANDARD TABLE OF ty_version_manifest WITH KEY key.

  TYPES:
    "! Tarball Attachment
    BEGIN OF ty_attachment,
      key TYPE string,
      BEGIN OF tarball,
        content_type TYPE string,
        data         TYPE string,
        length       TYPE i,
      END OF tarball,
    END OF ty_attachment,
    ty_attachments TYPE STANDARD TABLE OF ty_attachment WITH KEY key.

  TYPES:
    "! Full packument (as fetched from registry)
    "! Some fields are hoisted from latest version to root
    BEGIN OF ty_packument ##NEEDED,
      name         TYPE ty_name,
      description  TYPE string,
      dist_tags    TYPE ty_dist_tags,
      time         TYPE ty_times,
      versions     TYPE ty_version_manifests,
      maintainers  TYPE ty_persons,
      readme       TYPE string,
      users        TYPE ty_users,
      homepage     TYPE string,
      icon         TYPE string,
      bugs         TYPE ty_bugs,
      license      TYPE string,
      keywords     TYPE string_table,
      author       TYPE ty_person,
      repository   TYPE ty_repository,
      _id          TYPE string,
      _rev         TYPE string,
      _attachments TYPE ty_attachments,
      _objects     TYPE string_table,
      access       TYPE string,
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
      fsl_1_1_alv2      TYPE string VALUE 'FSL-1.1-ALv2',
      fsl_1_1_mit       TYPE string VALUE 'FSL-1.1-MIT',
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

  CONSTANTS:
    "! ABAP Language Version (same as zif_abapgit_dot_abapgit)
    BEGIN OF c_abap_language_version,
      standard          TYPE string VALUE 'standard',
      key_user          TYPE string VALUE 'keyUser',
      cloud_development TYPE string VALUE 'cloudDevelopment',
      ignore            TYPE string VALUE 'ignore',
      undefined         TYPE string VALUE 'undefined', " any
    END OF c_abap_language_version.

ENDINTERFACE.
