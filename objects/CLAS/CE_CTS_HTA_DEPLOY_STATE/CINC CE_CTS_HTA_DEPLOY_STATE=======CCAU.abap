CLASS ltcl_cts_hta_comp_type DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      test_enum_equals FOR TESTING RAISING cx_static_check,
      "! Test that all enums are different
      test_enum_not_equals FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_cts_hta_comp_type IMPLEMENTATION.

  METHOD test_enum_equals.
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_deploy_state=>not_deployed exp = ce_cts_hta_deploy_state=>not_deployed ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_deploy_state=>partly_deployed exp = ce_cts_hta_deploy_state=>partly_deployed ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_deploy_state=>deployed exp = ce_cts_hta_deploy_state=>deployed ).

    IF ce_cts_hta_deploy_state=>not_deployed = ce_cts_hta_deploy_state=>not_deployed.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for not_deployed' ).
    ENDIF.

    IF ce_cts_hta_deploy_state=>partly_deployed = ce_cts_hta_deploy_state=>partly_deployed.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for partly_deployed' ).
    ENDIF.

    IF ce_cts_hta_deploy_state=>deployed = ce_cts_hta_deploy_state=>deployed.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for deployed' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_enum_not_equals.
    IF ce_cts_hta_deploy_state=>not_deployed = ce_cts_hta_deploy_state=>partly_deployed.
      cl_abap_unit_assert=>fail( 'not_deployed must not have same reference as partly_deployed' ).
    ENDIF.

    IF ce_cts_hta_deploy_state=>not_deployed = ce_cts_hta_deploy_state=>deployed.
      cl_abap_unit_assert=>fail( 'not_deployed must not have same reference as deployed' ).
    ENDIF.

    IF ce_cts_hta_deploy_state=>partly_deployed = ce_cts_hta_deploy_state=>deployed.
      cl_abap_unit_assert=>fail( 'partly_deployed must not have same reference as deployed' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.