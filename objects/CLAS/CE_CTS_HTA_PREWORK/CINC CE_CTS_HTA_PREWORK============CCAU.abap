CLASS ltcl_cts_hta_comp_type DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      "! Test value for prework_done
      test_prework_done FOR TESTING RAISING cx_static_check,
      "! Test value for prework_not_done
      test_prework_not_done FOR TESTING RAISING cx_static_check,
      "! Test that always same instance is returned
      test_enum_equals FOR TESTING RAISING cx_static_check,
      "! Test that all enums are different
      test_enum_not_equals FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_cts_hta_comp_type IMPLEMENTATION.

  METHOD test_prework_done.
    cl_abap_unit_assert=>assert_equals( act = 'X' exp = ce_cts_hta_prework=>prework_done->value ).
  ENDMETHOD.

  METHOD test_prework_not_done.
    cl_abap_unit_assert=>assert_equals( act = ' ' exp = ce_cts_hta_prework=>prework_not_done->value ).
  ENDMETHOD.

  METHOD test_enum_equals.
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_prework=>prework_done exp = ce_cts_hta_prework=>prework_done ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_prework=>prework_not_done exp = ce_cts_hta_prework=>prework_not_done ).

    IF ce_cts_hta_prework=>prework_done = ce_cts_hta_prework=>prework_done.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for prework_done' ).
    ENDIF.

    IF ce_cts_hta_prework=>prework_not_done = ce_cts_hta_prework=>prework_not_done.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for prework_not_done' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_enum_not_equals.
    IF ce_cts_hta_prework=>prework_done = ce_cts_hta_prework=>prework_not_done.
      cl_abap_unit_assert=>fail( 'prework_done must not have same reference as prework_not_done' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.