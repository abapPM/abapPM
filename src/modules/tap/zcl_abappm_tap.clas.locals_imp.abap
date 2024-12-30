CLASS lcl_snapshot DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES ty_snapshot TYPE abaptxt255_tab.

    METHODS constructor
      IMPORTING
        include TYPE progname
        package TYPE devclass OPTIONAL
        title   TYPE sy-title OPTIONAL
        new     TYPE abap_bool DEFAULT abap_false.

    METHODS begin
      IMPORTING
        id   TYPE csequence
        text TYPE csequence OPTIONAL.

    METHODS write
      IMPORTING
        line TYPE string OPTIONAL
        list TYPE string_table OPTIONAL
          PREFERRED PARAMETER line.

    METHODS end.

    METHODS actual
      RETURNING
        VALUE(result) TYPE ty_snapshot.

    METHODS expected
      RETURNING
        VALUE(result) TYPE ty_snapshot.

  PRIVATE SECTION.

    CONSTANTS c_tap_snapshot TYPE string VALUE 'TAP_SNAPSHOT'. " DO NOT CHANGE!

    DATA:
      BEGIN OF snap,
        package TYPE devclass,
        include TYPE progname,
        title   TYPE sy-title,
        source  TYPE ty_snapshot,
      END OF snap.

    DATA:
      BEGIN OF snap_test,
        id     TYPE string,
        text   TYPE string,
        output TYPE ty_snapshot,
      END OF snap_test.

    METHODS class
      RETURNING
        VALUE(result) TYPE seoclsname.

    METHODS package
      RETURNING
        VALUE(result) TYPE devclass.

    METHODS include
      RETURNING
        VALUE(result) TYPE progname.

    METHODS header
      RETURNING
        VALUE(result) TYPE ty_snapshot.

    METHODS test_begin
      IMPORTING
        id            TYPE csequence
        text          TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS test_end
      IMPORTING
        id            TYPE csequence
      RETURNING
        VALUE(result) TYPE string.

    METHODS enqueue.

    METHODS save.

    METHODS dequeue.

    METHODS update_include.

    METHODS insert_include.

    METHODS activate.

    METHODS tadir.

    METHODS transport.

ENDCLASS.

CLASS lcl_snapshot IMPLEMENTATION.

  METHOD constructor.
    snap-package = COND #( WHEN package IS INITIAL THEN package( ) ELSE package ).
    snap-include = COND #( WHEN include IS INITIAL THEN include( ) ELSE include ).
    snap-title   = title.

    IF new = abap_false.
      " If program exists, get content and check that it's a snapshot
      READ REPORT snap-include INTO snap-source.
      IF sy-subrc = 0.
        ASSERT snap-source IS INITIAL OR snap-source[ 1 ] CS c_tap_snapshot.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD begin.
    CLEAR snap_test.
    snap_test-id = id.
    snap_test-text = text.
  ENDMETHOD.

  METHOD write.
    DATA output TYPE LINE OF ty_snapshot.

    IF line IS NOT INITIAL AND list IS INITIAL.
      output-line = |* { line }|.
      APPEND output TO snap_test-output.
    ELSEIF line IS INITIAL AND list IS NOT INITIAL.
      LOOP AT list ASSIGNING FIELD-SYMBOL(<line>).
        output-line = |* { <line> }|.
        APPEND output TO snap_test-output.
      ENDLOOP.
    ELSE.
      ASSERT 0 = 1. " either fill line or list parameters
    ENDIF.
  ENDMETHOD.

  METHOD end.
    IF snap-source IS INITIAL OR snap-source[ 1 ] NS c_tap_snapshot.
      INSERT LINES OF header( ) INTO snap-source INDEX 1.
    ENDIF.

    APPEND `` TO snap-source.
    APPEND test_begin( id = snap_test-id text = snap_test-text ) TO snap-source.
    APPEND LINES OF snap_test-output TO snap-source.
    APPEND test_end( snap_test-id ) TO snap-source.

    IF snap-include CP '*CCMAC'.
      enqueue( ).
      save( ).
      dequeue( ).
    ELSE.
      SELECT SINGLE progname FROM reposrc INTO @snap-include WHERE progname = @snap-include ##WARN_OK.
      IF sy-subrc = 0.
        update_include( ).
      ELSE.
        insert_include( ).
      ENDIF.
      activate( ).
      tadir( ).
      transport( ).
    ENDIF.
  ENDMETHOD.

  METHOD actual.
    LOOP AT snap_test-output ASSIGNING FIELD-SYMBOL(<line>).
      APPEND <line>+2 TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD expected.
    FIND test_begin( snap_test-id ) IN TABLE snap-source MATCH LINE DATA(from).
    ASSERT sy-subrc = 0.
    FIND test_end( snap_test-id ) IN TABLE snap-source MATCH LINE DATA(to).
    ASSERT sy-subrc = 0 AND to > from.

    LOOP AT snap-source ASSIGNING FIELD-SYMBOL(<line>) FROM from + 1 TO to - 1.
      ASSERT strlen( <line> ) > 2.
      APPEND <line>+2 TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD class.
    DATA callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.

    DATA(class) = callstack[ 3 ]-mainprogram(30).
    result = replace(
      val  = class
      sub  = '='
      with = ''
      occ  = 0 ).
  ENDMETHOD.

  METHOD package.
    " Return package of class which called TAP
    DATA(class) = class( ).

    SELECT SINGLE devclass FROM tadir INTO @result
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = @class.
    IF sy-subrc <> 0.
      result = '$TMP'.
    ENDIF.
  ENDMETHOD.

  METHOD include.
    " Return macro include of class which called TAP
    result = cl_oo_classname_service=>get_ccmac_name( class( ) ).
  ENDMETHOD.

  METHOD header.
    DATA(header) = |* { c_tap_snapshot }\n|
      && |*\n|
      && |* IMPORTANT\n|
      && |* This snapshot file is auto-generated, but designed for humans.\n|
      && |* It should be checked into source control and tracked carefully.\n|
      && |* Re-generate by setting SNAPSHOT = ABAP_TRUE and running tests.\n|
      && |* Make sure to inspect the output below.  Do not ignore changes!\n|
      && |*| ##NO_TEXT.

    SPLIT header AT |\n| INTO TABLE result.
  ENDMETHOD.

  METHOD test_begin.
    result = |* { c_tap_snapshot }: BEGIN OF { id }{ COND #( WHEN text IS NOT INITIAL THEN `| ` && text ) }|.
  ENDMETHOD.

  METHOD test_end.
    result = |* { c_tap_snapshot }: END OF { id }|.
  ENDMETHOD.

  METHOD enqueue.
    DATA(class) = class( ).
    TRANSLATE class USING ' ='.
    CALL FUNCTION 'ENQUEUE_ESEOCLASS'
      EXPORTING
        _scope         = '1'
        mode_seoclsenq = 'X'
        clsname        = class
        inctype        = 'C'
        cpdname        = 'MAC'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    ASSERT sy-subrc = 0. " Macro include is locked
  ENDMETHOD.

  METHOD save.
    DATA progdir TYPE progdir.
    " Saves snapshot in macro include
    SELECT SINGLE * FROM progdir INTO @progdir WHERE name = @snap-include AND state = 'A'.
    ASSERT sy-subrc = 0. " Macro include not found

    INSERT REPORT snap-include FROM snap-source STATE 'A' EXTENSION TYPE 'CC' KEEPING DIRECTORY ENTRY.
    ASSERT sy-subrc = 0.
    DELETE REPORT snap-include STATE 'I'.

    progdir-unam  = sy-uname.
    progdir-udat  = sy-datum.
    progdir-state = 'A'.

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = progdir
        i_progname   = snap-include
        i_state      = progdir-state
      EXCEPTIONS
        not_executed = 1.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s203(ed).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue.
    DATA(class) = class( ).
    TRANSLATE class USING ' ='.
    CALL FUNCTION 'DEQUEUE_ESEOCLASS'
      EXPORTING
        _synchron      = 'X'
        _scope         = '1'
        mode_seoclsenq = 'X'
        clsname        = class
        inctype        = 'C'
        cpdname        = 'MAC'.
  ENDMETHOD.

  METHOD update_include.
    " Saves snapshot in program include
    CALL FUNCTION 'RPY_PROGRAM_UPDATE'
      EXPORTING
        program_name     = snap-include
        title_string     = snap-title
        save_inactive    = 'A'
      TABLES
        source_extended  = snap-source
      EXCEPTIONS
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        OTHERS           = 4.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD insert_include.
    " Saves snapshot in program include
    CALL FUNCTION 'RPY_PROGRAM_INSERT'
      EXPORTING
        development_class = snap-package
        program_name      = snap-include
        program_type      = 'I' "include
        title_string      = snap-title
        save_inactive     = 'A'
        suppress_dialog   = abap_true
      TABLES
        source_extended   = snap-source
      EXCEPTIONS
        already_exists    = 1
        cancelled         = 2
        name_not_allowed  = 3
        permission_error  = 4
        OTHERS            = 5.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD activate.
    DATA objects TYPE STANDARD TABLE OF dwinactiv WITH KEY object obj_name uname.

    APPEND INITIAL LINE TO objects ASSIGNING FIELD-SYMBOL(<object>).
    <object>-object   = 'PROG'.
    <object>-obj_name = snap-include.

    CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
      EXPORTING
        activate_ddic_objects  = abap_false
        with_popup             = abap_false
        ui_decoupled           = abap_true
      TABLES
        objects                = objects
      EXCEPTIONS
        excecution_error       = 1
        cancelled              = 2
        insert_into_corr_error = 3
        OTHERS                 = 4.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD tadir.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = abap_false
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = 'PROG'
        wi_tadir_obj_name              = snap-include
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    ASSERT sy-subrc >= 0. "ignore
  ENDMETHOD.

  METHOD transport.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = snap-include
        object_class        = 'ABAP'
        devclass            = snap-package
        master_language     = sy-langu
        mode                = 'I'
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    ASSERT sy-subrc <= 1.
  ENDMETHOD.

ENDCLASS.
