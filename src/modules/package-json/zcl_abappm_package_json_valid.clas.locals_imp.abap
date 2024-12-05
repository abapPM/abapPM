CLASS lcl_validate DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS validate_single_values
      IMPORTING
        !is_manifest  TYPE zif_abappm_package_json_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_persons
      IMPORTING
        !is_manifest  TYPE zif_abappm_package_json_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_arrays
      IMPORTING
        !is_manifest  TYPE zif_abappm_package_json_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_dependencies
      IMPORTING
        !is_manifest  TYPE zif_abappm_package_json_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

ENDCLASS.

CLASS lcl_validate IMPLEMENTATION.

  METHOD validate_single_values.

    IF zcl_abappm_package_json_valid=>is_valid_name( is_manifest-name ) = abap_false.
      INSERT |Invalid name: { is_manifest-name }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_version( is_manifest-version ) = abap_false.
      INSERT |Invalid version: { is_manifest-version }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_package_type( is_manifest-type ) = abap_false.
      INSERT |Invalid package type: { is_manifest-type }| INTO TABLE result.
    ENDIF.

    IF is_manifest-private <> abap_false AND is_manifest-private <> abap_true.
      INSERT |Invalid private flag: { is_manifest-private }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( is_manifest-homepage ) = abap_false.
      INSERT |Invalid homepage URL: { is_manifest-homepage }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_email( is_manifest-bugs-email ) = abap_false.
      INSERT |Invalid bugs email: { is_manifest-bugs-email }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( is_manifest-bugs-url ) = abap_false.
      INSERT |Invalid bugs URL: { is_manifest-bugs-url }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( is_manifest-repository-url ) = abap_false.
      INSERT |Invalid repository URL: { is_manifest-repository-url }| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_persons.

    DATA:
      ls_person TYPE zif_abappm_package_json_types=>ty_person,
      lv_value  TYPE string,
      lt_values TYPE string_table.

    IF zcl_abappm_package_json_valid=>is_valid_email( is_manifest-author-email ) = abap_false.
      INSERT |Invalid author email: { is_manifest-author-email }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( is_manifest-author-url ) = abap_false.
      INSERT |Invalid author URL: { is_manifest-author-url }| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-contributors INTO ls_person.
      COLLECT ls_person-name INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_email( ls_person-email ) = abap_false.
        INSERT |Invalid contributor email: { ls_person-name } { ls_person-email }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_url( ls_person-url ) = abap_false.
        INSERT |Invalid contributor URL: { ls_person-name } { ls_person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-contributors ) <> lines( lt_values ).
      INSERT |Duplicate contributors| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-maintainers INTO ls_person.
      COLLECT ls_person-name INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_email( ls_person-email ) = abap_false.
        INSERT |Invalid maintainer email: { ls_person-name } { ls_person-email }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_url( ls_person-url ) = abap_false.
        INSERT |Invalid maintainer URL: { ls_person-name } { ls_person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-maintainers ) <> lines( lt_values ).
      INSERT |Duplicate maintainers| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_arrays.

    DATA:
      lv_value  TYPE string,
      lt_values TYPE string_table.

    CLEAR lt_values.
    LOOP AT is_manifest-cpu INTO lv_value.
      COLLECT lv_value INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_cpu( lv_value ) = abap_false.
        INSERT |Invalid CPU: { lv_value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-cpu ) <> lines( lt_values ).
      INSERT |Duplicate CPU values| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-db INTO lv_value.
      COLLECT lv_value INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_db( lv_value ) = abap_false.
        INSERT |Invalid database: { lv_value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-db ) <> lines( lt_values ).
      INSERT |Duplicate database values| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-os INTO lv_value.
      COLLECT lv_value INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_os( lv_value ) = abap_false.
        INSERT |Invalid operating system: { lv_value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-os ) <> lines( lt_values ).
      INSERT |Duplicate operating system values| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_dependencies.

    DATA:
      ls_dependency TYPE zif_abappm_package_json_types=>ty_dependency,
      lv_value      TYPE string,
      lt_values     TYPE string_table.

    CLEAR lt_values.
    LOOP AT is_manifest-engines INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_engine( ls_dependency-name ) = abap_false.
        INSERT |Invalid engine: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid engine version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-engines ) <> lines( lt_values ).
      INSERT |Duplicate engines| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_name( ls_dependency-name ) = abap_false.
        INSERT |Invalid dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid dependency version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-dependencies ) <> lines( lt_values ).
      INSERT |Duplicate dependencies| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-dev_dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_name( ls_dependency-name ) = abap_false.
        INSERT |Invalid dev dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid dev dependency version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
      READ TABLE is_manifest-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        INSERT |Dev dependency { ls_dependency-name } already included in dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-dev_dependencies ) <> lines( lt_values ).
      INSERT |Duplicate dev dependencies| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-optional_dependencies INTO ls_dependency.
      COLLECT ls_dependency-name INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_name( ls_dependency-name ) = abap_false.
        INSERT |Invalid optional dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( ls_dependency-range ) = abap_false.
        INSERT |Invalid optional dependency version: { ls_dependency-name } { ls_dependency-range }| INTO TABLE result.
      ENDIF.
      READ TABLE is_manifest-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        INSERT |Optional dependency { ls_dependency-name } already included in dependencies| INTO TABLE result.
      ENDIF.
      READ TABLE is_manifest-dev_dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc = 0.
        INSERT |Optional dependency { ls_dependency-name } already included in dev dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-optional_dependencies ) <> lines( lt_values ).
      INSERT |Duplicate optional dependencies| INTO TABLE result.
    ENDIF.

    CLEAR lt_values.
    LOOP AT is_manifest-bundle_dependencies INTO lv_value.
      COLLECT lv_value INTO lt_values.
      IF zcl_abappm_package_json_valid=>is_valid_name( lv_value ) = abap_false.
        INSERT |Invalid bundle dependency: { ls_dependency-name }| INTO TABLE result.
      ENDIF.
      READ TABLE is_manifest-dependencies TRANSPORTING NO FIELDS WITH KEY name = ls_dependency-name.
      IF sy-subrc <> 0.
        INSERT |Bundle dependency { ls_dependency-name } not included in dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( is_manifest-bundle_dependencies ) <> lines( lt_values ).
      INSERT |Duplicate bundle dependencies| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
