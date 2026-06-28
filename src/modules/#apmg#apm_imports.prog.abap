*&---------------------------------------------------------------------*
*& Include          /APMG/IMPORTS
*&
*& Note: These IMPORT statements are not executable code but are
*& interpreted by apm when running the UPDATE command for dependencies.
*&
*& Each import statement represents a from/to regex mapping for the
*& objects included in the packagae. The default from rule (*) is regex
*& (?:\/.+\/|Y|Z)(..)(.*).
*&
*& Examples for class and interface regex matching:
*&
*& IMPORT '*' TO '/apmg/$1_apm$2'
*&
*& From              -> Prefix, Name       -> Result
*& --------------------------------------------------------------------
*& YCL_TEST          -> $1 = CL, $2 = TEST -> /APMG/CL_APM_TEST
*& ZIF_TEST          -> $1 = IF, $2 = TEST -> /APMG/IF_APM_TEST
*& /MBTOOLS/CL_UTIL  -> $1 = CL, $2 = UTIL -> /APMG/CL_APM_UTIL
*& /MBTOOLS/IF_UTIL  -> $1 = IF, $2 = UTIL -> /APMG/CL_APM_UTIL
*&
*& - You can define multiple rules per package (first match wins)
*& - You can comment out the IMPORT statements (works the same)
*& - The FROM package must be type "module"
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
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'env'.
IMPORT '*' TO '/apmg/$1_apm$2'              FROM 'exceptions'.
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
