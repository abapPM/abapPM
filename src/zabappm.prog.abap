REPORT zabappm LINE-SIZE 100.

* See http://www.abappm.com

********************************************************************************
* The MIT License (MIT)
*
* Copyright 2024 apm.to Inc. <https://apm.to>  [ where noted in code ]
* Copyright 2014 abapGit Contributors
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

* @@require devc $abapgit_data
* @@require devc $abapgit_objects
* @@require devc $abapgit_objects_ecatt
* @@require devc $abapgit_objects_enh
* @@require devc $abapgit_objects_oo
* @@require devc $abapgit_objects_sap
* @@require devc $abapgit_objects_tabl
* @@require devc $abapgit_objects_texts
* @@require devc $abapgit_xml

* @@require clas zcl_abapgit_dependencies
* @@require clas zcl_abapgit_repo_status
* @@require clas zcl_abapgit_filename_logic
* @@require clas zcl_abapgit_folder_logic
* @@require clas zcl_abapgit_objects_activation
* @@require clas zcl_abapgit_objects_files
* @@require clas zcl_abapgit_tadir
* @@require intf zif_abapgit_tadir

* @@require clas zcl_abapgit_ajson
* @@require clas zcl_abapgit_ajson_filter_lib
* @@require clas zcl_abapgit_ajson_mapping
* @@require clas zcx_abapgit_ajson_error
* @@require intf zif_abapgit_ajson
* @@require intf zif_abapgit_ajson_filter
* @@require intf zif_abapgit_ajson_mapping
* @@require intf zif_abapgit_ajson_types

* @@require clas zcl_abapgit_aff_registry
* @@require intf zif_abapgit_aff_intf_v1
* @@require intf zif_abapgit_aff_oo_types_v1
* @@require intf zif_abapgit_aff_types_v1

* @@require clas zcl_abapgit_abap_language_vers
* @@require clas zcl_abapgit_adt_link
* @@require intf zif_abapgit_apack_definitions
* @@require clas zcx_abapgit_cancel
* @@require clas zcl_abapgit_convert
* @@require clas zcl_abapgit_cts_api
* @@require intf zif_abapgit_cts_api
* @@require clas zcl_abapgit_default_transport
* @@require intf zif_abapgit_default_transport
* @@require intf zif_abapgit_definitions
* @@require clas zcl_abapgit_dependencies
* @@require clas zcl_abapgit_dot_abapgit
* @@require intf zif_abapgit_dot_abapgit
* @@require clas zcl_abapgit_environment
* @@require intf zif_abapgit_environment
* @@require clas zcx_abapgit_exception
* @@require clas zcl_abapgit_exit
* @@require intf zif_abapgit_exit
* @@require clas zcl_abapgit_field_rules
* @@require intf zif_abapgit_field_rules
* @@require clas zcl_abapgit_frontend_services
* @@require intf zif_abapgit_frontend_services
* @@require clas zcl_abapgit_function_module
* @@require intf zif_abapgit_function_module
* @@require intf zif_abapgit_git_definitions
* @@require intf zif_abapgit_gui_event_handler
* @@require clas zcl_abapgit_gui_jumper
* @@require intf zif_abapgit_gui_jumper
* @@require clas zcl_abapgit_hash
* @@require clas zcl_abapgit_i18n_params
* @@require clas zcl_abapgit_item_graph
* @@require clas zcl_abapgit_language
* @@require clas zcl_abapgit_log
* @@require intf zif_abapgit_log
* @@require clas zcx_abapgit_not_found
* @@require clas zcl_abapgit_path
* @@require intf zif_abapgit_persistence
* @@require clas zcl_abapgit_persistence_db
* @@require clas zcl_abapgit_persist_packages
* @@require clas zcl_abapgit_progress
* @@require intf zif_abapgit_progress
* @@require clas zcl_abapgit_timer
* @@require clas zcl_abapgit_url
* @@require clas zcl_abapgit_version
* @@require intf zif_abapgit_version

* @@require clas zcl_tar

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

TABLES sscrfields.

INCLUDE zabappm_password_dialog. " !!! Contains SELECTION SCREEN

INCLUDE zabappm_forms.

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
