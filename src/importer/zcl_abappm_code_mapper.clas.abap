CLASS zcl_abappm_code_mapper DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !program      TYPE zif_abappm_code_importer=>ty_program
        !object_types TYPE zif_abappm_code_importer=>ty_object_types
        !object_names TYPE zif_abappm_code_importer=>ty_object_names
        !is_logging   TYPE abap_bool DEFAULT abap_false
        !default_rule TYPE string DEFAULT zif_abappm_code_importer=>c_default_import_rule
      RETURNING
        VALUE(result) TYPE zif_abappm_code_importer=>ty_map
      RAISING
        zcx_abappm_error.

  PRIVATE SECTION.

    " TODO: replace with logger
    CONSTANTS c_width TYPE i VALUE 150.

    TYPES:
      BEGIN OF ty_rule,
        old_object  TYPE string,
        new_object  TYPE string,
        new_package TYPE string,
        name        TYPE string,
        version     TYPE string,
      END OF ty_rule,
      ty_rules TYPE STANDARD TABLE OF ty_rule WITH DEFAULT KEY.

    CLASS-METHODS get_import_rules
      IMPORTING
        !program      TYPE zif_abappm_code_importer=>ty_program
        !is_logging   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE ty_rules
      RAISING
        zcx_abappm_error.

    CLASS-METHODS get_tadir_objects
      IMPORTING
        !source_package TYPE devclass
        !object_types   TYPE zif_abappm_code_importer=>ty_object_types
        !object_names   TYPE zif_abappm_code_importer=>ty_object_names
        !is_logging     TYPE abap_bool
      RETURNING
        VALUE(result)   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abappm_error.

ENDCLASS.



CLASS zcl_abappm_code_mapper IMPLEMENTATION.


  METHOD get.

    DATA:
      import_all TYPE abap_bool,
      rules      TYPE ty_rules,
      map        TYPE zif_abappm_code_importer=>ty_map_item,
      tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt.

    " TODO: replace with logger
    IF is_logging = abap_true.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'Include:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      WRITE: / program-program.
      SKIP.
    ENDIF.

    " Get list of rules for importing objects
    rules = get_import_rules(
      program    = program
      is_logging = is_logging ).

    READ TABLE rules ASSIGNING FIELD-SYMBOL(<rule>) WITH KEY old_object = '*'.
    IF sy-subrc = 0.
      import_all = abap_true.
      <rule>-old_object = default_rule.
    ENDIF.

    " Get list of original objects
    tadir = get_tadir_objects(
      source_package = program-source_package
      object_types   = object_types
      object_names   = object_names
      is_logging     = is_logging ).

    " Process all objects and apply rules
    LOOP AT tadir ASSIGNING FIELD-SYMBOL(<tadir>).
      CLEAR map.
      map-source_package = program-source_package.
      map-target_package = program-package.
      map-object_type    = <tadir>-object.
      map-old_object     = <tadir>-obj_name.

      " Collect first matching rule
      DATA(found) = abap_false.
      LOOP AT rules ASSIGNING <rule>.
        map-new_object = <tadir>-obj_name.

        IF <rule>-old_object = <tadir>-obj_name.
          " Direct mapping
          map-new_object = <rule>-new_object.
        ELSE.
          " Regex replacement
          map-new_object = replace(
            val   = <tadir>-obj_name
            regex = <rule>-old_object
            with  = <rule>-new_object ).
        ENDIF.
        IF map-new_object <> <tadir>-obj_name AND strlen( map-new_object ) <= 30.
          found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF found = abap_true.
        INSERT map INTO TABLE result.
      ELSEIF import_all = abap_true.
        " With IMPORT '*' we expect all objects to be mapped
        " Most likely, the object name became too long requiring an additional rule
        zcx_abappm_error=>raise( |No mapping rule found for { <tadir>-obj_name }| ).
      ENDIF.
    ENDLOOP.

    " TODO: replace with logger
    IF is_logging = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Mapping:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<map>).
        WRITE: / <map>-source_package, <map>-target_package, <map>-old_object, <map>-new_object.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.


  METHOD get_import_rules.

    DATA:
      tabix  TYPE sy-tabix,
      pos    TYPE string,
      tokens TYPE TABLE OF stokesx,
      rule   TYPE ty_rule.

    tokens = zcl_abappm_code_importer=>scan( program-program ).

    " Process all IMPORT statements
    "
    " Example 1:
    "
    " IMPORT zif_ajson TO zif_my_app_ajson FROM '@sbcgua/ajson'.
    "
    " IMPORT  c
    " ZIF_AJSON  r
    " TO  b
    " ZIF_MY_APP_AJSON  m
    " FROM  b
    " '@sbcgua/ajson'  !
    "
    " Example 2:
    "
    " IMPORT '*' TO 'z$1_my_app$2' FROM '@sbcgua/ajson'.
    "
    " IMPORT  c
    " '*'  !
    " TO  b
    " 'z$1_my_app$2'  m
    " FROM  b
    " '@sbcgua/ajson'  m
    LOOP AT tokens TRANSPORTING NO FIELDS WHERE type = 'c' AND str = 'IMPORT'.
      tabix = sy-tabix.
      CLEAR rule.
      DO 5 TIMES.
        pos = |Tabix { tabix } Index { sy-index }|.
        READ TABLE tokens ASSIGNING FIELD-SYMBOL(<token>) INDEX tabix + sy-index.
        IF sy-subrc <> 0.
          zcx_abappm_error=>raise( |Error parsing IMPORT statement. { pos }| ).
        ENDIF.

        CASE sy-index.
          WHEN 1.
            CASE <token>-type.
              WHEN 'r'. " reference
                rule-old_object = <token>-str.
              WHEN '!' OR 'm'. " literal
                rule-old_object = to_upper( replace(
                  val  = <token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
              WHEN OTHERS.
                zcx_abappm_error=>raise( |Unknown identifier { <token>-str }. { pos }| ).
            ENDCASE.
          WHEN 2.
            IF <token>-type <> 'b' OR <token>-str <> 'TO'.
              zcx_abappm_error=>raise( |Error parsing IMPORT statement. Expecting "TO". { pos }| ).
            ENDIF.
          WHEN 3.
            CASE <token>-type.
              WHEN 'r'. " reference
                rule-new_object = <token>-str.
              WHEN '!' OR 'm'. " literal
                rule-new_object = to_upper( replace(
                  val  = <token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
                IF rule-new_object CS '/'.
                  SPLIT rule-new_object AT '/' INTO rule-new_package rule-new_object.
                  " Install into a sub package of where the IMPORT was found
                  " Note: new_package is the folder name, which is mapped to
                  " an ABAP package based on prefix folder rules
                  rule-new_package = |{ program-package }_{ rule-new_package }|.
                ELSE.
                  " Install into the same package where the IMPORT was found
                  rule-new_package = program-package.
                ENDIF.
              WHEN OTHERS.
                zcx_abappm_error=>raise( |Unknown identifier { <token>-str }. { pos }| ).
            ENDCASE.
          WHEN 4.
            IF <token>-type <> 'b' OR <token>-str <> 'FROM'.
              zcx_abappm_error=>raise( |Error parsing IMPORT statement. Expecting "FROM". { pos }| ).
            ENDIF.
          WHEN 5.
            CASE <token>-type.
              WHEN 'r'. " reference
                rule-name = <token>-str.
              WHEN '!' OR 'm'. " literal
                rule-name = to_lower( replace(
                  val  = <token>-str
                  sub  = ''''
                  with = ''
                  occ  = 0 ) ).
              WHEN OTHERS.
                zcx_abappm_error=>raise( |Unknown identifier { <token>-str }. { pos }| ).
            ENDCASE.
            IF rule-name+1(*) CS '@'.
              SPLIT rule-name AT '@' INTO rule-name rule-version.
            ELSE.
              rule-version = 'latest'.
            ENDIF.
        ENDCASE.
      ENDDO.

      INSERT rule INTO TABLE result.
    ENDLOOP.

    " TODO: replace with logger
    IF is_logging = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Rules:', AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<rule>).
        WRITE: / <rule>-old_object,
          AT 32 <rule>-new_package, <rule>-new_object,
          AT 94 <rule>-name, <rule>-version.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.


  METHOD get_tadir_objects.

    " Get list of original objects
    TRY.
        result = zcl_abapgit_factory=>get_tadir( )->read( iv_package = source_package ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        zcx_abappm_error=>raise_with_text( lx_error ).
    ENDTRY.

    " Only classes and interfaces
    " FUTURE: support other object types
    DELETE result WHERE NOT ( object = 'CLAS' OR object = 'INTF' ).

    " Filter objects (for testing)
    DELETE result WHERE NOT ( object IN object_types AND obj_name IN object_names ).

    " TODO: replace with logger
    IF is_logging = abap_true.
      FORMAT COLOR COL_NORMAL.
      WRITE: / 'Objects:' COLOR COL_NORMAL, AT c_width space.
      SKIP.
      FORMAT COLOR OFF.
      LOOP AT result ASSIGNING FIELD-SYMBOL(<tadir>).
        WRITE: / <tadir>-object, <tadir>-obj_name.
      ENDLOOP.
      SKIP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
