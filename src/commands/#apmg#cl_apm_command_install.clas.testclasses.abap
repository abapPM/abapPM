CLASS ltcl_install_plan DEFINITION DEFERRED.
CLASS /apmg/cl_apm_command_install DEFINITION LOCAL FRIENDS ltcl_install_plan.

CLASS ltcl_install_plan DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA command TYPE REF TO /apmg/cl_apm_command_install.

    METHODS setup.
    METHODS create_diff
      RETURNING
        VALUE(result) TYPE REF TO /apmg/if_apm_arborist_diff.
    METHODS dependency_first FOR TESTING
      RAISING
        /apmg/cx_apm_error.
    METHODS requires_all_assignments FOR TESTING.

ENDCLASS.


CLASS ltcl_install_plan IMPLEMENTATION.

  METHOD create_diff.

    DATA(actual_tree) = NEW /apmg/cl_apm_arborist_tree( ).
    DATA(ideal_tree) = NEW /apmg/cl_apm_arborist_tree( ).

    DATA(root_manifest) = VALUE /apmg/if_apm_types=>ty_manifest(
      name                = 'root'
      version             = '1.0.0'
      dependencies        = VALUE #( ( key = 'dependency' range = '^1.0.0' ) )
      sap_package-default = '$ROOT' ).
    DATA(dependency_manifest) = VALUE /apmg/if_apm_types=>ty_manifest(
      name                = 'dependency'
      version             = '1.2.0'
      sap_package-default = '$DEPENDENCY' ).

    DATA(root) = ideal_tree->add_node(
      manifest  = root_manifest
      installed = abap_false ).
    ideal_tree->add_node(
      manifest  = dependency_manifest
      installed = abap_false ).
    /apmg/cl_apm_arborist_edge=>create(
      tree = ideal_tree
      from = root
      type = /apmg/if_apm_arborist=>c_dependency_type-prod
      name = 'dependency'
      spec = '^1.0.0' ).

    result = /apmg/cl_apm_arborist_diff=>calculate(
      actual = actual_tree
      ideal  = ideal_tree ).

  ENDMETHOD.


  METHOD dependency_first.

    DATA(assignments) = VALUE /apmg/cl_apm_command_install=>ty_package_assignments(
      ( name = 'root' package = '$ROOT' )
      ( name = 'dependency' package = '$DEPENDENCY' ) ).

    DATA(changes) = command->normalize_changes(
      root_name   = 'root'
      diff        = create_diff( )
      assignments = assignments ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( changes )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = changes[ 1 ]-name
      exp = 'dependency' ).
    cl_abap_unit_assert=>assert_equals(
      act = changes[ 1 ]-package
      exp = '$DEPENDENCY' ).
    cl_abap_unit_assert=>assert_equals(
      act = changes[ 2 ]-name
      exp = 'root' ).
    cl_abap_unit_assert=>assert_equals(
      act = changes[ 2 ]-manifest-sap_package-default
      exp = '$ROOT' ).

  ENDMETHOD.


  METHOD requires_all_assignments.

    DATA(assignments) = VALUE /apmg/cl_apm_command_install=>ty_package_assignments(
      ( name = 'root' package = '$ROOT' ) ).
    DATA(raised) = abap_false.

    TRY.
        command->normalize_changes(
          root_name   = 'root'
          diff        = create_diff( )
          assignments = assignments ).
      CATCH /apmg/cx_apm_error INTO DATA(error).
        raised = abap_true.
        cl_abap_unit_assert=>assert_char_cp(
          act = error->get_text( )
          exp = '*dependency*' ).
    ENDTRY.

    cl_abap_unit_assert=>assert_true( raised ).

  ENDMETHOD.


  METHOD setup.

    command = NEW /apmg/cl_apm_command_install( ).

  ENDMETHOD.

ENDCLASS.
