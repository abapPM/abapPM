CLASS ltcl_code_importer DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      class_name_from_token FOR TESTING,
      interface_name_from_token FOR TESTING,
      object_name_from_abapdoc FOR TESTING.

ENDCLASS.

CLASS zcl_abappm_code_importer DEFINITION LOCAL FRIENDS ltcl_code_importer.

CLASS ltcl_code_importer IMPLEMENTATION.

  METHOD class_name_from_token.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |ZIF_TEST~METHOD| )
      exp = 'ZIF_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |ZCL_TEST( )| )
      exp = 'ZCL_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |ZCL_TEST=>METHOD| )
      exp = 'ZCL_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |ZCL_TEST->METHOD| )
      exp = 'ZCL_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |@ZCL_TEST=>CONSTANT| )
      exp = 'ZCL_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |('ZCL_TEST')=>METHOD| )
      exp = 'ZCL_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |(ZCL_TEST=>CONST)->METHOD| )
      exp = 'ZCL_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_class_name_from_token( |(ZCL_TEST=>CONST)| )
      exp = 'ZCL_TEST' ).

  ENDMETHOD.

  METHOD interface_name_from_token.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_interface_name_from_token( |ZCL_TEST=>ZIF_TEST~METHOD| )
      exp = 'ZIF_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_interface_name_from_token( |ZCL_TEST->ZIF_TEST~METHOD| )
      exp = 'ZIF_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_interface_name_from_token( |ZIF_TEST~METHOD| )
      exp = 'ZIF_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_interface_name_from_token( |ZCL_TEST->('ZIF_TEST~METHOD')| )
      exp = 'ZIF_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_interface_name_from_token( |ZCL_TEST->(ZIF_TEST=>CONST)| )
      exp = 'ZIF_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_interface_name_from_token( |ZCL_TEST=>(ZIF_TEST~CONST)| )
      exp = 'ZIF_TEST' ).

  ENDMETHOD.

  METHOD object_name_from_abapdoc.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abappm_code_importer=>get_object_name_from_abapdoc( |"! @raising zcx_error \| Exception| )
      exp = 'ZCX_ERROR' ).

  ENDMETHOD.


ENDCLASS.
