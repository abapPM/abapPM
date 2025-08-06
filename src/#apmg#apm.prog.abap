REPORT /apmg/apm LINE-SIZE 100.

* See http://www.abappm.com

********************************************************************************
* The MIT License (MIT)
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* Copyright 2014 abapGit Contributors [ where noted in code ]
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

* @@require devc /apmg/apm_abapgit
* @@require devc /apmg/apm_commands*
* @@require devc /apmg/apm_importer
* @@require devc /apmg/apm_installer
* @@require devc /apmg/apm_main
* @@require devc /apmg/apm_modules*
* @@require devc /apmg/apm_objects
* @@require devc /apmg/apm_ui*
* @@require devc /apmg/apm_utils

* From abapGit Objects
* @@require devc $abapgit_objects*
* The following are excluded or replaced during build process
* - zcl_abapgit_objects
* - zcl_abapgit_objects_bridge
* - zcl_abapgit_objects_check
* - zcl_abapgit_objects_injector
* @@require devc $abapgit_xml

* @@require clas zcl_abapgit_abap_language_vers
* @@require clas zcl_abapgit_adt_link
* @@require clas zcl_abapgit_convert
* @@require clas zcl_abapgit_cts_api
* @@require intf zif_abapgit_cts_api
* @@require intf zif_abapgit_data_config
* @@require intf zif_abapgit_data_supporter
* @@require clas zcl_abapgit_default_transport
* @@require intf zif_abapgit_default_transport
* @@require intf zif_abapgit_definitions
* @@require clas zcl_abapgit_dot_abapgit
* @@require intf zif_abapgit_dot_abapgit
* @@require clas zcl_abapgit_env_factory
* @@require clas zcl_abapgit_env_injector
* @@require clas zcl_abapgit_environment
* @@require intf zif_abapgit_environment
* @@require clas zcx_abapgit_exception
* @@require clas zcl_abapgit_exit
* @@require intf zif_abapgit_exit
* @@require clas zcl_abapgit_factory
* @@require clas zcl_abapgit_injector
* @@require intf zif_abapgit_git_definitions
* @@require clas zcl_abapgit_hash
* @@require clas zcl_abapgit_language
* @@require clas zcl_abapgit_log
* @@require intf zif_abapgit_log
* @@require clas zcx_abapgit_not_found
* @@require clas zcl_abapgit_path
* @@require intf zif_abapgit_persistence
* @@require clas zcl_abapgit_progress
* @@require intf zif_abapgit_progress
* @@require clas zcl_abapgit_status_calc
* @@require intf zif_abapgit_status_calc
* @@require clas zcl_abapgit_string_buffer
* @@require clas zcl_abapgit_timer
* @@require clas zcx_abapgit_type_not_supported
* @@require clas zcl_abapgit_url
* @@require clas zcl_abapgit_user_record
* @@require intf zif_abapgit_user_record
* @@require clas zcl_abapgit_utils
* @@require clas zcl_abapgit_version
* @@require intf zif_abapgit_version

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

TABLES sscrfields.

INCLUDE /apmg/apm_password_dialog. " !!! Contains SELECTION SCREEN

INCLUDE /apmg/apm_forms.

**********************************************************************
INITIALIZATION.
  PERFORM adjust_toolbar USING '1001'.
  lcl_password_dialog=>on_screen_init( ).

START-OF-SELECTION.
  PERFORM run.

* Hide Execute button from screen
AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ELSE.
    PERFORM output.
  ENDIF.

* SAP back command re-direction
AT SELECTION-SCREEN ON EXIT-COMMAND.
  PERFORM exit.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.
