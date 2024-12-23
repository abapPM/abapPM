CLASS lcl_validate DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS validate_single_values
      IMPORTING
        !manifest     TYPE ZIF_ABAPPM_TYPES=>TY_MANIFEST
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_persons
      IMPORTING
        !manifest     TYPE ZIF_ABAPPM_TYPES=>TY_MANIFEST
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_arrays
      IMPORTING
        !manifest     TYPE ZIF_ABAPPM_TYPES=>TY_MANIFEST
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_dependencies
      IMPORTING
        !manifest     TYPE ZIF_ABAPPM_TYPES=>TY_MANIFEST
      RETURNING
        VALUE(result) TYPE string_table.

ENDCLASS.

CLASS lcl_validate IMPLEMENTATION.

  METHOD validate_single_values.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_NAME( manifest-name ) = abap_false.
      INSERT |Invalid name: { manifest-name }| INTO TABLE result.
    ENDIF.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_VERSION( manifest-version ) = abap_false.
      INSERT |Invalid version: { manifest-version }| INTO TABLE result.
    ENDIF.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_PACKAGE_TYPE( manifest-type ) = abap_false.
      INSERT |Invalid package type: { manifest-type }| INTO TABLE result.
    ENDIF.

    IF manifest-private <> abap_false AND manifest-private <> abap_true.
      INSERT |Invalid private flag: { manifest-private }| INTO TABLE result.
    ENDIF.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_URL( manifest-homepage ) = abap_false.
      INSERT |Invalid homepage URL: { manifest-homepage }| INTO TABLE result.
    ENDIF.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_EMAIL( manifest-bugs-email ) = abap_false.
      INSERT |Invalid bugs email: { manifest-bugs-email }| INTO TABLE result.
    ENDIF.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_URL( manifest-bugs-url ) = abap_false.
      INSERT |Invalid bugs URL: { manifest-bugs-url }| INTO TABLE result.
    ENDIF.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_URL( manifest-repository-url ) = abap_false.
      INSERT |Invalid repository URL: { manifest-repository-url }| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_persons.

    DATA:
      value  TYPE string,
      values TYPE string_table.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_EMAIL( manifest-author-email ) = abap_false.
      INSERT |Invalid author email: { manifest-author-email }| INTO TABLE result.
    ENDIF.

    IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_URL( manifest-author-url ) = abap_false.
      INSERT |Invalid author URL: { manifest-author-url }| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-contributors INTO DATA(person).
      COLLECT person-name INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_EMAIL( person-email ) = abap_false.
        INSERT |Invalid contributor email: { person-name } { person-email }| INTO TABLE result.
      ENDIF.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_URL( person-url ) = abap_false.
        INSERT |Invalid contributor URL: { person-name } { person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-contributors ) <> lines( values ).
      INSERT |Duplicate contributors| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-maintainers INTO person.
      COLLECT person-name INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_EMAIL( person-email ) = abap_false.
        INSERT |Invalid maintainer email: { person-name } { person-email }| INTO TABLE result.
      ENDIF.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_URL( person-url ) = abap_false.
        INSERT |Invalid maintainer URL: { person-name } { person-url }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-maintainers ) <> lines( values ).
      INSERT |Duplicate maintainers| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_arrays.

    DATA:
      value  TYPE string,
      values TYPE string_table.

    CLEAR values.
    LOOP AT manifest-cpu INTO value.
      COLLECT value INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_CPU( value ) = abap_false.
        INSERT |Invalid CPU: { value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-cpu ) <> lines( values ).
      INSERT |Duplicate CPU values| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-db INTO value.
      COLLECT value INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_DB( value ) = abap_false.
        INSERT |Invalid database: { value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-db ) <> lines( values ).
      INSERT |Duplicate database values| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-os INTO value.
      COLLECT value INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_OS( value ) = abap_false.
        INSERT |Invalid operating system: { value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-os ) <> lines( values ).
      INSERT |Duplicate operating system values| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_dependencies.

    DATA:
      value  TYPE string,
      values TYPE string_table.

    CLEAR values.
    LOOP AT manifest-engines INTO DATA(dependency).
      COLLECT dependency-name INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_ENGINE( dependency-name ) = abap_false.
        INSERT |Invalid engine: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_VERSION_RANGE( dependency-range ) = abap_false.
        INSERT |Invalid engine version: { dependency-name } { dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-engines ) <> lines( values ).
      INSERT |Duplicate engines| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-dependencies INTO dependency.
      COLLECT dependency-name INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_NAME( dependency-name ) = abap_false.
        INSERT |Invalid dependency: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_VERSION_RANGE( dependency-range ) = abap_false.
        INSERT |Invalid dependency version: { dependency-name } { dependency-range }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-dependencies ) <> lines( values ).
      INSERT |Duplicate dependencies| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-dev_dependencies INTO dependency.
      COLLECT dependency-name INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_NAME( dependency-name ) = abap_false.
        INSERT |Invalid dev dependency: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_VERSION_RANGE( dependency-range ) = abap_false.
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
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_NAME( dependency-name ) = abap_false.
        INSERT |Invalid optional dependency: { dependency-name }| INTO TABLE result.
      ENDIF.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_VERSION_RANGE( dependency-range ) = abap_false.
        INSERT |Invalid optional dependency version: { dependency-name } { dependency-range }| INTO TABLE result.
      ENDIF.
      READ TABLE manifest-dependencies TRANSPORTING NO FIELDS WITH KEY name = dependency-name.
      IF sy-subrc = 0.
        INSERT |Optional dependency { dependency-name } already included in dependencies| INTO TABLE result.
      ENDIF.
      READ TABLE manifest-dev_dependencies TRANSPORTING NO FIELDS WITH KEY name = dependency-name.
      IF sy-subrc = 0.
        INSERT |Optional dependency { dependency-name } already included in dev dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-optional_dependencies ) <> lines( values ).
      INSERT |Duplicate optional dependencies| INTO TABLE result.
    ENDIF.

    CLEAR values.
    LOOP AT manifest-bundle_dependencies INTO value.
      COLLECT value INTO values.
      IF ZCL_ABAPPM_PACKAGE_JSON_VALID=>IS_VALID_NAME( value ) = abap_false.
        INSERT |Invalid bundle dependency: { value }| INTO TABLE result.
      ENDIF.
      READ TABLE manifest-dependencies TRANSPORTING NO FIELDS WITH KEY name = value.
      IF sy-subrc <> 0.
        INSERT |Bundle dependency { value } not included in dependencies| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( manifest-bundle_dependencies ) <> lines( values ).
      INSERT |Duplicate bundle dependencies| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
