*&---------------------------------------------------------------------*
*& Include          ZABAPPM_IMPORTS
*&---------------------------------------------------------------------*

* core apm modules
IMPORT '*' TO 'package-json/z$1_abappm$2'            FROM 'apm-package-json'.
IMPORT '*' TO 'pacote/z$1_abappm$2'                  FROM 'apm-pacote'.
IMPORT '*' TO 'persist/z$1_abappm$2'                 FROM 'apm-persist'.
IMPORT '*' TO 'readme/z$1_abappm$2'                  FROM 'apm-readme'.
IMPORT '*' TO 'settings/z$1_abappm$2'                FROM 'apm-settings'.
IMPORT '*' TO 'types/z$1_abappm$2'                   FROM 'apm-types'.

* other modules by apm
IMPORT '*' TO 'emoji/z$1_abappm$2'                   FROM 'emoji'.
IMPORT '*' TO 'error/z$1_abappm$2'                   FROM 'error'.
IMPORT '*' TO 'highlighter/z$1_abappm$2'             FROM 'syntax-highlighter'.
IMPORT '*' TO 'http-agent/z$1_abappm$2'              FROM 'http-agent'.
IMPORT '*' TO 'markdown/z$1_abappm$2'                FROM 'markdown'.
IMPORT '*' TO 'semver/z$1_abappm$2'                  FROM 'semver'.
IMPORT '*' TO 'semver-sap/z$1_abappm$2'              FROM 'semver-sap'.
IMPORT '*' TO 'tar/z$1_abappm$2'                     FROM 'tar'.
IMPORT '*' TO 'url/z$1_abappm$2'                     FROM 'url'.

* other modules
IMPORT '*' TO 'ajson/z$1_abappm$2'                   FROM 'ajson'.
IMPORT 'z(..)_abap(.*)' TO 'string-map/z$1_abappm$2' FROM 'string-map'.
