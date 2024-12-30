CLASS lcl_validate DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS validate_single_values
      IMPORTING
        !manifest     TYPE zif_abappm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_persons
      IMPORTING
        !manifest     TYPE zif_abappm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_arrays
      IMPORTING
        !manifest     TYPE zif_abappm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_engines
      IMPORTING
        !manifest     TYPE zif_abappm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_dependencies
      IMPORTING
        !manifest     TYPE zif_abappm_types=>ty_manifest
      RETURNING
        VALUE(result) TYPE string_table.

ENDCLASS.

CLASS lcl_validate IMPLEMENTATION.

  METHOD validate_single_values.

    IF zcl_abappm_package_json_valid=>is_valid_name( manifest-name ) = abap_false.
      INSERT |Invalid name: { manifest-name }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_version( manifest-version ) = abap_false.
      INSERT |Invalid version: { manifest-version }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_package_type( manifest-type ) = abap_false.
      INSERT |Invalid package type: { manifest-type }| INTO TABLE result.
    ENDIF.

    IF manifest-private <> abap_false AND manifest-private <> abap_true.
      INSERT |Invalid private flag: { manifest-private }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( manifest-homepage ) = abap_false.
      INSERT |Invalid homepage URL: { manifest-homepage }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_email( manifest-bugs-email ) = abap_false.
      INSERT |Invalid bugs email: { manifest-bugs-email }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( manifest-bugs-url ) = abap_false.
      INSERT |Invalid bugs URL: { manifest-bugs-url }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( manifest-repository-url ) = abap_false.
      INSERT |Invalid repository URL: { manifest-repository-url }| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_persons.

    DATA values TYPE string_table.

    IF zcl_abappm_package_json_valid=>is_valid_email( manifest-author-email ) = abap_false.
      INSERT |Invalid author email: { manifest-author-email }| INTO TABLE result.
    ENDIF.

    IF zcl_abappm_package_json_valid=>is_valid_url( manifest-author-url ) = abap_false.
      INSERT |Invalid author URL: { manifest-author-url }| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-contributors INTO DATA(person).
      COLLECT person-name INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_email( person-email ) = abap_false.
        INSERT |Invalid contributor email: { person-name } { person-email }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_url( person-url ) = abap_false.
        INSERT |Invalid contributor URL: { person-name } { person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-contributors ) <> lines( values ).
      INSERT |Duplicate contributors| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-maintainers INTO person.
      COLLECT person-name INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_email( person-email ) = abap_false.
        INSERT |Invalid maintainer email: { person-name } { person-email }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_url( person-url ) = abap_false.
        INSERT |Invalid maintainer URL: { person-name } { person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-maintainers ) <> lines( values ).
      INSERT |Duplicate maintainers| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_arrays.

    DATA values TYPE string_table.

    CLEAR values.
    LOOP AT manifest-cpu INTO DATA(value).
      COLLECT value INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_cpu( value ) = abap_false.
        INSERT |Invalid CPU: { value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-cpu ) <> lines( values ).
      INSERT |Duplicate CPU values| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-db INTO value.
      COLLECT value INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_db( value ) = abap_false.
        INSERT |Invalid database: { value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-db ) <> lines( values ).
      INSERT |Duplicate database values| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-os INTO value.
      COLLECT value INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_os( value ) = abap_false.
        INSERT |Invalid operating system: { value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-os ) <> lines( values ).
      INSERT |Duplicate operating system values| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_engines.

    DATA values TYPE string_table.

    CLEAR values.
    LOOP AT manifest-engines INTO DATA(dependency).
      COLLECT dependency-name INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_engine( dependency-name ) = abap_false.
        INSERT |Invalid engine: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( dependency-range ) = abap_false.
        INSERT |Invalid engine version: { dependency-name } { dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-engines ) <> lines( values ).
      INSERT |Duplicate engines| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_dependencies.

    DATA values TYPE string_table.

    CLEAR values.
    LOOP AT manifest-dependencies INTO DATA(dependency).
      COLLECT dependency-name INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_name( dependency-name ) = abap_false.
        INSERT |Invalid dependency: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( dependency-range ) = abap_false.
        INSERT |Invalid dependency version: { dependency-name } { dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-dependencies ) <> lines( values ).
      INSERT |Duplicate dependencies| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-dev_dependencies INTO dependency.
      COLLECT dependency-name INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_name( dependency-name ) = abap_false.
        INSERT |Invalid dev dependency: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( dependency-range ) = abap_false.
        INSERT |Invalid dev dependency version: { dependency-name } { dependency-range }| INTO TABLE result.
      ENDIF.
      READ TABLE manifest-dependencies TRANSPORTING NO FIELDS WITH KEY name = dependency-name.
      IF sy-subrc = 0.
        INSERT |Dev dependency { dependency-name } already included in dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-dev_dependencies ) <> lines( values ).
      INSERT |Duplicate dev dependencies| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-optional_dependencies INTO dependency.
      COLLECT dependency-name INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_name( dependency-name ) = abap_false.
        INSERT |Invalid optional dependency: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF zcl_abappm_package_json_valid=>is_valid_version_range( dependency-range ) = abap_false.
        INSERT |Invalid optional dependency version: { dependency-name } { dependency-range }| INTO TABLE result.
      ENDIF.
      IF line_exists( manifest-dependencies[ name = dependency-name ] ).
        INSERT |Optional dependency { dependency-name } already included in dependencies| INTO TABLE result.
      ENDIF.
      IF line_exists( manifest-dev_dependencies[ name = dependency-name ] ).
        INSERT |Optional dependency { dependency-name } already included in dev dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-optional_dependencies ) <> lines( values ).
      INSERT |Duplicate optional dependencies| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-bundle_dependencies INTO DATA(value).
      COLLECT value INTO values.
      IF zcl_abappm_package_json_valid=>is_valid_name( value ) = abap_false.
        INSERT |Invalid bundle dependency: { value }| INTO TABLE result.
      ENDIF.
      IF NOT line_exists( manifest-dependencies[ name = value ] ).
        INSERT |Bundle dependency { value } not included in dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-bundle_dependencies ) <> lines( values ).
      INSERT |Duplicate bundle dependencies| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
