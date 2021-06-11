CLASS ltcl_cts_hta_comp_type DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      test_enum_equals FOR TESTING RAISING cx_static_check,
      "! Test that all enums are different
      test_enum_not_equals FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_cts_hta_comp_type IMPLEMENTATION.

  METHOD test_enum_equals.
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_sync_state=>in_sync exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_sync_state=>not_in_sync exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_sync_state=>can_not_be_synchronized exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).

    IF ce_cts_hta_sync_state=>in_sync = ce_cts_hta_sync_state=>in_sync.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for in_sync' ).
    ENDIF.

    IF ce_cts_hta_sync_state=>not_in_sync = ce_cts_hta_sync_state=>not_in_sync.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for not_in_sync' ).
    ENDIF.

    IF ce_cts_hta_sync_state=>can_not_be_synchronized = ce_cts_hta_sync_state=>can_not_be_synchronized.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for can_not_be_synchronized' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_enum_not_equals.
    IF ce_cts_hta_sync_state=>in_sync = ce_cts_hta_sync_state=>not_in_sync.
      cl_abap_unit_assert=>fail( 'in_sync must not have same reference as not_in_sync' ).
    ENDIF.

    IF ce_cts_hta_sync_state=>in_sync = ce_cts_hta_sync_state=>can_not_be_synchronized.
      cl_abap_unit_assert=>fail( 'in_sync must not have same reference as can_not_be_synchronized' ).
    ENDIF.

    IF ce_cts_hta_sync_state=>not_in_sync = ce_cts_hta_sync_state=>can_not_be_synchronized.
      cl_abap_unit_assert=>fail( 'not_in_sync must not have same reference as can_not_be_synchronized' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.