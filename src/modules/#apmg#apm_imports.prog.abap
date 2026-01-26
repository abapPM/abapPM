*&---------------------------------------------------------------------*
*& Include          /APMG/IMPORTS
*&---------------------------------------------------------------------*

* core apm modules
IMPORT '*' TO '/apmg/$1_apm$2'              FROM '@apm/arborist'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM '@apm/package-json'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM '@apm/pacote'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM '@apm/persistence'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM '@apm/readme'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM '@apm/settings'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM '@apm/types'.

* other apm modules
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'ajson-extensions'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'emoji'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'error'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'syntax-highlighter'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'http-agent'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'markdown'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'progress-bar'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'semver'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'semver-sap'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'tar'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'url'.

* modules from Alexander Tsybulsky
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'ajson'.
IMPORT 'z(..)_abap(.*)' TO '/apmg/$1_apm$2' FROM 'string-map'.
