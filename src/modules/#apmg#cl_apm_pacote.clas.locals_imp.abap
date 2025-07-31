CLASS lcl_validate DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS validate_single_values
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_dist_tags
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_times
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS validate_users
      IMPORTING
        !packument    TYPE /apmg/if_apm_types=>ty_packument
      RETURNING
        VALUE(result) TYPE string_table.

ENDCLASS.

CLASS lcl_validate IMPLEMENTATION.

  METHOD validate_single_values.

    IF packument-_id IS INITIAL.
      INSERT |Missing id| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_dist_tags.

    DATA values TYPE string_table.

    CLEAR values.
    LOOP AT packument-dist_tags INTO DATA(dist_tag).
      COLLECT dist_tag-key INTO values.

      IF /apmg/cl_apm_package_json_vali=>is_valid_version( dist_tag-value ) = abap_false.
        INSERT |Invalid dist-tag version: { dist_tag-key } { dist_tag-value }| INTO TABLE result.
      ENDIF.
      IF NOT line_exists( packument-versions[ key = dist_tag-value ] ).
        INSERT |Dist-tag version does not exist: { dist_tag-key } { dist_tag-value }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( packument-dist_tags ) <> lines( values ).
      INSERT |Duplicate dist-tags| INTO TABLE result.
    ENDIF.

    IF NOT line_exists( packument-dist_tags[ key = 'latest' ] ).
      INSERT |"latest" dist-tags is missing| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_times.

    DATA values TYPE string_table.

    CLEAR values.
    LOOP AT packument-time INTO DATA(time).
      COLLECT time-key INTO values.

      IF time-key <> 'created' AND time-key <> 'modified' AND NOT line_exists( packument-versions[ key = time-key ] ).
        INSERT |Timestamp version does not exist: { time-key }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( packument-time ) <> lines( values ).
      INSERT |Duplicate timestamps| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

  METHOD validate_users.

    DATA values TYPE string_table.

    CLEAR values.
    LOOP AT packument-users INTO DATA(user).
      COLLECT user-name INTO values.

      IF user-stars NOT BETWEEN 0 AND 5.
        INSERT |Invalid number of stars: { user-name } { user-stars }| INTO TABLE result.
      ENDIF.
    ENDLOOP.
    IF lines( packument-users ) <> lines( values ).
      INSERT |Duplicate user names| INTO TABLE result.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
