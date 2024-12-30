CLASS ltcl_pacote DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS get_complete FOR TESTING RAISING zcx_abappm_error.

ENDCLASS.

CLASS zcl_abappm_pacote DEFINITION LOCAL FRIENDS ltcl_pacote.

CLASS ltcl_pacote IMPLEMENTATION.

  METHOD get_complete.

    DATA packument TYPE zif_abappm_types=>ty_packument.

    DATA(version) = VALUE zif_abappm_types=>ty_manifest(
      name         = 'test'
      version      = '1.0.0'
      author-name  = 'Marc'
      author-email = 'marc@test.com' ).

    DATA(dependency) = VALUE zif_abappm_types=>ty_dependency(
      name  = 'dep2'
      range = '2.0.0' ).
    INSERT dependency INTO TABLE version-dependencies.
    dependency-name  = 'dep3'.
    dependency-range = '>3'.
    INSERT dependency INTO TABLE version-dev_dependencies.
    dependency-name  = 'dep4'.
    dependency-range = '^4.1.0'.
    INSERT dependency INTO TABLE version-optional_dependencies.
    dependency-name  = 'dep5'.
    dependency-range = '^5.0.1'.
    INSERT dependency INTO TABLE version-peer_dependencies.
    INSERT `dep2` INTO TABLE version-bundle_dependencies.
    dependency-name  = 'abap'.
    dependency-range = '>=7.50'.
    INSERT dependency INTO TABLE version-engines.
    dependency-name  = 'apm'.
    dependency-range = '>=1'.
    INSERT dependency INTO TABLE version-engines.

    DATA(json) = |\{\n|
      && |  "_id": "@registrytest/package",\n|
      && |  "_rev": "5-c21daa8aefda6b83f0f0cfd7302e0f81",\n|
      && |  "author": \{\n|
      && |    "email": "owner@registrytest.org",\n|
      && |    "name": "Olivia Ortega",\n|
      && |    "url": "https://github.com/rt-owner"\n|
      && |  \},\n|
      && |  "bugs": \{\n|
      && |    "email": "bugs@registrytest.org",\n|
      && |    "url": "https://github.com/registrytest/package/issues"\n|
      && |  \},\n|
      && |  "contributors": [\n|
      && |    \{\n|
      && |      "email": "owner@registrytest.org",\n|
      && |      "name": "Olivia Ortega",\n|
      && |      "url": "https://github.com/rt-owner"\n|
      && |    \},\n|
      && |    \{\n|
      && |      "email": "contributor@registrytest.org",\n|
      && |      "name": "Cindy Collins",\n|
      && |      "url": "https://github.com/rt-contributors"\n|
      && |    \}\n|
      && |  ],\n|
      && |  "description": "This is an example package",\n|
      && |  "dist-tags": \{\n|
      && |    "latest": "1.0.1"\n|
      && |  \},\n|
      && |  "homepage": "https://registrytest.org",\n|
      && |  "keywords": [\n|
      && |    "example",\n|
      && |    "package",\n|
      && |    "test"\n|
      && |  ],\n|
      && |  "license": "MIT",\n|
      && |  "maintainers": [\n|
      && |    \{\n|
      && |      "email": "owner@registrytest.org",\n|
      && |      "name": "rt-owner"\n|
      && |    \},\n|
      && |    \{\n|
      && |      "email": "maintainer@registrytest.org",\n|
      && |      "name": "rt-maintainer"\n|
      && |    \}\n|
      && |  ],\n|
      && |  "name": "@registrytest/package",\n|
      && |  "readme": "# A Package for Registry Test",\n|
      && |  "repository": \{\n|
      && |    "type": "git",\n|
      && |    "url": "git+https://github.com/registrytest/package.git"\n|
      && |  \},\n|
      && |  "time": \{\n|
      && |    "1.0.0": "2024-05-02T12:17:47.805Z",\n|
      && |    "created": "2024-05-02T17:59:06.250Z",\n|
      && |    "modified": "2024-11-10T19:51:04.774Z"\n|
      && |  \},\n|
      && |  "versions": \{\n|
      && |    "1.0.0": \{\n|
      && |      "_id": "@registrytest/package@1.0.1",\n|
      && |      "_abapVersion": "7.54.0",\n|
      && |      "_apmVersion": "1.0.0",\n|
      && |      "author": \{\n|
      && |        "email": "owner@registrytest.org",\n|
      && |        "name": "Olivia Ortega",\n|
      && |        "url": "https://github.com/rt-owner"\n|
      && |      \},\n|
      && |      "bugs": \{\n|
      && |        "email": "bugs@registrytest.org",\n|
      && |        "url": "https://github.com/registrytest/package/issues"\n|
      && |      \},\n|
      && |      "bundleDependencies": [\n|
      && |        "semver"\n|
      && |      ],\n|
      && |      "contributors": [\n|
      && |        \{\n|
      && |          "email": "owner@registrytest.org",\n|
      && |          "name": "Olivia Ortega",\n|
      && |          "url": "https://github.com/rt-owner"\n|
      && |        \},\n|
      && |        \{\n|
      && |          "email": "contributor@registrytest.org",\n|
      && |          "name": "Cindy Collins",\n|
      && |          "url": "https://github.com/rt-contributors"\n|
      && |        \}\n|
      && |      ],\n|
      && |      "cpu": [\n|
      && |        "x86-64"\n|
      && |      ],\n|
      && |      "dependencies": \{\n|
      && |        "express": "^4.19.2",\n|
      && |        "lodash": "^4.17.21",\n|
      && |        "semver": "~7.6.0"\n|
      && |      \},\n|
      && |      "description": "This is an example package",\n|
      && |      "devDependencies": \{\n|
      && |        "eslint": "^9.1.1",\n|
      && |        "jest": "^29.7.0",\n|
      && |        "prettier": "^3.2.5"\n|
      && |      \},\n|
      && |      "dist": \{\n|
      && |        "fileCount": 6,\n|
      && |        "integrity": "sha512-Z5UFg8g3X4eT/1wl1HCVENNAvNh9z5By7z3OL7XZQnsBh1SQ1dtUu1InlT3b2D|
      && |kpOOxwagDAVTX16hS25TU/rw==",\n|
      && |        "shasum": "c689ad02174b9a45ee0f2350f74a0b503a109389",\n|
      && |        "signatures": [\n|
      && |          \{\n|
      && |            "keyid": "SHA256:jl3bwswu80PjjokCgh0o2w5c2U4LhQAE57gj9cz1kzA",\n|
      && |            "sig": "MEUCIQDGjADf3mDXMzryyGvffT0/s1IqISZC00BF8WctcYzq3gIgHCzxFPNAuwe616Fcvr4|
      && |tOM2FRrP6MKfhtIpuRBI2pjg="\n|
      && |          \}\n|
      && |        ],\n|
      && |        "tarball": "https://registry.npmjs.org/@registrytest/package/-/package-1.0.1.tgz",\n|
      && |        "unpackedSize": 3987\n|
      && |      \},\n|
      && |      "engines": \{\n|
      && |        "abap": ">=7.50",\n|
      && |        "apm": ">=1"\n|
      && |      \},\n|
      && |      "funding": \{\n|
      && |        "type": "github",\n|
      && |        "url": "https://github.com/registrytest"\n|
      && |      \},\n|
      && |      "homepage": "https://registrytest.org",\n|
      && |      "keywords": [\n|
      && |        "example",\n|
      && |        "package",\n|
      && |        "test"\n|
      && |      ],\n|
      && |      "license": "MIT",\n|
      && |      "maintainers": [\n|
      && |        \{\n|
      && |          "email": "owner@registrytest.org",\n|
      && |          "name": "rt-owner"\n|
      && |        \},\n|
      && |        \{\n|
      && |          "email": "maintainer@registrytest.org",\n|
      && |          "name": "rt-maintainer"\n|
      && |        \},\n|
      && |        \{\n|
      && |          "email": "contributor@registrytest.org",\n|
      && |          "name": "rt-contributor"\n|
      && |        \},\n|
      && |        \{\n|
      && |          "email": "marc@marcbernardtools.com",\n|
      && |          "name": "mbtools"\n|
      && |        \}\n|
      && |      ],\n|
      && |      "name": "@registrytest/package",\n|
      && |      "optionalDependencies": \{\n|
      && |        "tap": "18.7.2"\n|
      && |      \},\n|
      && |      "os": [\n|
      && |        "linux"\n|
      && |      ],\n|
      && |      "peerDependencies": \{\n|
      && |        "lodash": "^4.17.21",\n|
      && |        "react": ">=17 <19.0.0"\n|
      && |      \},\n|
      && |      "repository": \{\n|
      && |        "type": "git",\n|
      && |        "url": "git+https://github.com/registrytest/package.git"\n|
      && |      \},\n|
      && |      "type": "module",\n|
      && |      "version": "1.0.0"\n|
      && |    \}\n|
      && |  \}\n|
      && |\}\n|.

    DATA(act) = zcl_abappm_pacote=>convert_json_to_packument( json ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = packument ).

  ENDMETHOD.


ENDCLASS.
