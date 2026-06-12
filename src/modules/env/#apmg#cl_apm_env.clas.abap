CLASS /apmg/cl_apm_env DEFINITION
  PUBLIC
  CREATE PRIVATE.

************************************************************************
* Environment
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES /apmg/if_apm_env.

    DATA env TYPE /apmg/if_apm_env=>ty_environment READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        !env_type     TYPE string DEFAULT /apmg/if_apm_env=>c_env-abap
      RETURNING
        VALUE(result) TYPE REF TO /apmg/cl_apm_env.

    METHODS constructor
      IMPORTING
        !env_type TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA env_type TYPE string.

    METHODS init.

    METHODS init_abap.

    METHODS init_os.

    METHODS init_profile.

    METHODS get_value
      IMPORTING
        !name         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS format
      IMPORTING
        !number       TYPE simple
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS /apmg/cl_apm_env IMPLEMENTATION.


  METHOD /apmg/if_apm_env~delete.

    DATA(env_name) = condense( val = name from = `` ).

    READ TABLE env ASSIGNING FIELD-SYMBOL(<env>) WITH TABLE KEY name = env_name.
    IF sy-subrc = 0.
      IF <env>-type = /apmg/if_apm_env=>c_user_defined.
        DELETE env WHERE name = env_name.
      ELSE.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Cannot delete system-defined variable { env_name }|.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Variable { env_name } not found|.
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD /apmg/if_apm_env~get.

    READ TABLE env ASSIGNING FIELD-SYMBOL(<env>)
      WITH TABLE KEY name = condense( val = name from = `` ).
    IF sy-subrc = 0.
      IF <env>-value = /apmg/if_apm_env=>c_undefined.
        result = get_value( name ).
        <env>-value = result.
      ELSE.
        result = <env>-value.
      ENDIF.
    ELSE.
      result = /apmg/if_apm_env=>c_undefined.
    ENDIF.

  ENDMETHOD.


  METHOD /apmg/if_apm_env~get_all.

    LOOP AT env ASSIGNING FIELD-SYMBOL(<env>).
      DATA(value) = ``.
      IF <env>-value = /apmg/if_apm_env=>c_undefined.
        TRY.
            value = get_value( <env>-name ).
          CATCH cx_root INTO DATA(error).
            value = |Error: { error->get_text( ) }|.
        ENDTRY.
      ELSE.
        value = <env>-value.
      ENDIF.
      INSERT |{ <env>-name }={ value }| INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD /apmg/if_apm_env~set.

    DATA new_env TYPE /apmg/if_apm_env~ty_env.

    IF name CS '='.
      IF value IS INITIAL.
        SPLIT name AT '=' INTO new_env-name new_env-value.
      ELSE.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = 'Invalid format. Use "NAME=VALUE" or pass NAME and VALUE separately'.
      ENDIF.
    ELSE.
      new_env = VALUE #(
        name  = name
        value = value
        type  = type ).
    ENDIF.

    IF type <> /apmg/if_apm_env=>c_system_defined AND type <> /apmg/if_apm_env=>c_user_defined.
      RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
        EXPORTING
          text = |Invalid type. Use "{ /apmg/if_apm_env=>c_user_defined }" or "{ /apmg/if_apm_env=>c_system_defined }"|.
    ENDIF.

    " Trim
    new_env-name  = condense( val = new_env-name from = `` ).
    new_env-value = condense( val = new_env-value from = `` ).

    READ TABLE env ASSIGNING FIELD-SYMBOL(<env>) WITH TABLE KEY name = new_env-name.
    IF sy-subrc = 0.
      IF <env>-type = type.
        <env>-value = new_env-value.
      ELSE.
        RAISE EXCEPTION TYPE /apmg/cx_apm_error_text
          EXPORTING
            text = |Cannot change type of variable { new_env-name }|.
      ENDIF.
    ELSE.
      INSERT new_env INTO TABLE env.
      ASSERT sy-subrc = 0.
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD constructor.

    me->env_type = env_type.

    init( ).

  ENDMETHOD.


  METHOD create.

    result = NEW #( env_type ).

  ENDMETHOD.


  METHOD format.

    result = number.

    SHIFT result LEFT DELETING LEADING '0'.
    IF result IS INITIAL.
      result = '0'.
    ENDIF.

  ENDMETHOD.


  METHOD get_value.

    " Other types don't have undefined values
    ASSERT env_type = /apmg/if_apm_env=>c_env-abap.

    result = lcl_abap_environment=>new( )->get( name ).

  ENDMETHOD.


  METHOD init.

    CASE env_type.
      WHEN /apmg/if_apm_env=>c_env-abap.
        init_abap( ).
      WHEN /apmg/if_apm_env=>c_env-os.
        init_os( ).
      WHEN /apmg/if_apm_env=>c_env-profile.
        init_profile( ).
    ENDCASE.

    SORT env.

  ENDMETHOD.


  METHOD init_abap.

    DATA:
      env_names  TYPE string_table,
      components TYPE STANDARD TABLE OF comp_props WITH KEY component,
      swproducts TYPE tt_instswprod_sps.

    " Initialize all environment variables with "undefined"
    " Getting the proper value for all variables would take too long
    DATA(class_name) = cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).

    SELECT SINGLE refclsname FROM seometarel INTO @DATA(interface_name)
      WHERE clsname = @class_name AND version = '1'.
    ASSERT sy-subrc = 0.

    SELECT attvalue FROM seocompodf INTO TABLE @env_names
      WHERE clsname = @interface_name AND cmpname NOT LIKE 'C#_%' ESCAPE '#' AND attdecltyp = '2'.
    ASSERT sy-subrc = 0.

    LOOP AT env_names INTO DATA(env_name).
      env_name = replace(
        val  = env_name
        sub  = ''''
        with = ''
        occ  = 0 ).
      TRY.
          /apmg/if_apm_env~set(
            name  = env_name
            value = /apmg/if_apm_env=>c_undefined
            type  = /apmg/if_apm_env=>c_system_defined ).
        CATCH /apmg/cx_apm_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    " Add all software components
    CALL FUNCTION 'OCS_GET_INSTALLED_COMPS'
      TABLES
        et_components    = components
      EXCEPTIONS
        no_release_found = 1
        wrong_release    = 2
        OTHERS           = 9.
    ASSERT sy-subrc = 0.

    LOOP AT components INTO DATA(component).
      TRY.
          /apmg/if_apm_env~set(
            name  = |{ component-component }|
            value = format( component-release )
            type  = /apmg/if_apm_env=>c_system_defined ).

          /apmg/if_apm_env~set(
            name  = |{ component-component }_SP|
            value = format( component-sp_level )
            type  = /apmg/if_apm_env=>c_system_defined ).
        CATCH /apmg/cx_apm_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    " Add all software products
    CALL FUNCTION 'OCS_GET_INSTALLED_SWPRODUCTS'
      IMPORTING
        et_swprod_spstack = swproducts
      EXCEPTIONS
        internal_error    = 1
        OTHERS            = 2.
    ASSERT sy-subrc = 0.

    LOOP AT swproducts INTO DATA(product).
      DATA(name) = replace(
        val  = product-prod_name
        sub  = ` `
        with = `_`
        occ  = 0 ).
      TRY.
          /apmg/if_apm_env~set(
            name  = |{ name }|
            value = format( product-prod_version )
            type  = /apmg/if_apm_env=>c_system_defined ).

          SPLIT product-stack_caption AT space INTO DATA(value) DATA(rest) ##NEEDED.

          /apmg/if_apm_env~set(
            name  = |{ name }_SP|
            value = format( value )
            type  = /apmg/if_apm_env=>c_system_defined ).
        CATCH /apmg/cx_apm_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD init_os.

    DATA environment TYPE STANDARD TABLE OF thenv WITH KEY line.

    CALL FUNCTION 'TH_ENVIRONMENT'
      TABLES
        environment = environment.

    LOOP AT environment ASSIGNING FIELD-SYMBOL(<param>).
      SPLIT <param> AT '=' INTO DATA(name) DATA(value).

      TRY.
          /apmg/if_apm_env~set(
            name  = name
            value = value
            type  = /apmg/if_apm_env=>c_system_defined ).
        CATCH /apmg/cx_apm_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD init_profile.

    DATA parameters_sub TYPE spfl_parameter_list_t.

    cl_spfl_profile_parameter=>get_all_parameter( IMPORTING parameter_sub = parameters_sub ).

    LOOP AT parameters_sub ASSIGNING FIELD-SYMBOL(<param>).
      TRY.
          CASE <param>-state.
            WHEN '1' OR '3'.
              /apmg/if_apm_env~set(
                name  = <param>-name
                value = <param>-user_value
                type  = /apmg/if_apm_env=>c_system_defined ).
            WHEN '2'.
              /apmg/if_apm_env~set(
                name  = <param>-name
                value = <param>-default_value
                type  = /apmg/if_apm_env=>c_system_defined ).
            WHEN OTHERS.
              ASSERT 0 = 1.
          ENDCASE.
        CATCH /apmg/cx_apm_error ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
