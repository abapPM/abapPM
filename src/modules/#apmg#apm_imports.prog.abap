*&---------------------------------------------------------------------*
*& Include          /APMG/IMPORTS
*&---------------------------------------------------------------------*

* core apm modules
IMPORT '*' TO 'package-json:/apmg/$1_apm$2'            FROM '@apm/package-json'.
IMPORT '*' TO 'pacote:/apmg/$1_apm$2'                  FROM '@apm/pacote'.
IMPORT '*' TO 'persist:/apmg/$1_apm$2'                 FROM '@apm/persistence'.
IMPORT '*' TO 'readme:/apmg/$1_apm$2'                  FROM '@apm/readme'.
IMPORT '*' TO 'settings:/apmg/$1_apm$2'                FROM '@apm/settings'.
IMPORT '*' TO 'types:/apmg/$1_apm$2'                   FROM '@apm/types'.

* other apm modules
IMPORT '*' TO 'ajson-ext:/apmg/$1_apm$2'               FROM 'ajson-extensions'.
IMPORT '*' TO 'emoji:/apmg/$1_apm$2'                   FROM 'emoji'.
IMPORT '*' TO 'error:/apmg/$1_apm$2'                   FROM 'error'.
IMPORT '*' TO 'highlighter:/apmg/$1_apm$2'             FROM 'syntax-highlighter'.
IMPORT '*' TO 'http-agent:/apmg/$1_apm$2'              FROM 'http-agent'.
IMPORT '*' TO 'markdown:/apmg/$1_apm$2'                FROM 'markdown'.
IMPORT '*' TO 'semver:/apmg/$1_apm$2'                  FROM 'semver'.
IMPORT '*' TO 'semver-sap:/apmg/$1_apm$2'              FROM 'semver-sap'.
IMPORT '*' TO 'tar:/apmg/$1_apm$2'                     FROM 'tar'.
IMPORT '*' TO 'url:/apmg/$1_apm$2'                     FROM 'url'.

* modules from Alexander Tsybulsky
IMPORT '*' TO 'ajson:/apmg/$1_apm$2'                   FROM 'ajson'.
IMPORT 'z(..)_abap(.*)' TO 'string_map:/apmg/$1_apm$2' FROM 'string-map'.
