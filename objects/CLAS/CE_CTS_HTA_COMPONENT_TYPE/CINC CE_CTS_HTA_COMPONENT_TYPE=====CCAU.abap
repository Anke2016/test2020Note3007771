CLASS ltcl_cts_hta_comp_type DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      "! Test that always same instance is returned
      test_enum_equals FOR TESTING RAISING cx_static_check,
      "! Test that all enums are different
      test_enum_not_equals FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_cts_hta_comp_type IMPLEMENTATION.
  METHOD test_enum_equals.
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_component_type=>ct_if_cts_hta_component_list exp = ce_cts_hta_component_type=>ct_if_cts_hta_component_list ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_component_type=>ct_if_cts_hta_full_package exp = ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_component_type=>ct_if_cts_hta_object exp = ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_component_type=>ct_if_cts_hta_package exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = ce_cts_hta_component_type=>ct_unknown exp = ce_cts_hta_component_type=>ct_unknown ).

    IF ce_cts_hta_component_type=>ct_if_cts_hta_component_list = ce_cts_hta_component_type=>ct_if_cts_hta_component_list.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for ct_if_cts_hta_component_list' ).
    ENDIF.

    IF ce_cts_hta_component_type=>ct_if_cts_hta_full_package = ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for ct_if_cts_hta_full_package' ).
    ENDIF.

    IF ce_cts_hta_component_type=>ct_if_cts_hta_object = ce_cts_hta_component_type=>ct_if_cts_hta_object.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for ct_if_cts_hta_object' ).
    ENDIF.

    IF ce_cts_hta_component_type=>ct_if_cts_hta_package = ce_cts_hta_component_type=>ct_if_cts_hta_package.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for ct_if_cts_hta_package' ).
    ENDIF.

    IF ce_cts_hta_component_type=>ct_unknown = ce_cts_hta_component_type=>ct_unknown.
      "OK
    ELSE.
      cl_abap_unit_assert=>fail( 'not the same for ct_unknown' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_enum_not_equals.
    IF ce_cts_hta_component_type=>ct_if_cts_hta_component_list = ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_component_list must not have same reference as ct_if_cts_hta_full_package' ).
    ENDIF.
    IF ce_cts_hta_component_type=>ct_if_cts_hta_component_list = ce_cts_hta_component_type=>ct_if_cts_hta_object.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_component_list must not have same reference as ct_if_cts_hta_object' ).
    ENDIF.
    IF ce_cts_hta_component_type=>ct_if_cts_hta_component_list = ce_cts_hta_component_type=>ct_if_cts_hta_package.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_component_list must not have same reference as ct_if_cts_hta_package' ).
    ENDIF.
    IF ce_cts_hta_component_type=>ct_if_cts_hta_component_list = ce_cts_hta_component_type=>ct_unknown.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_component_list must not have same reference as ct_unknown' ).
    ENDIF.

    IF ce_cts_hta_component_type=>ct_if_cts_hta_full_package = ce_cts_hta_component_type=>ct_if_cts_hta_object.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_full_package must not have same reference as ct_if_cts_hta_object' ).
    ENDIF.
    IF ce_cts_hta_component_type=>ct_if_cts_hta_full_package = ce_cts_hta_component_type=>ct_if_cts_hta_package.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_full_package must not have same reference as ct_if_cts_hta_package' ).
    ENDIF.
    IF ce_cts_hta_component_type=>ct_if_cts_hta_full_package = ce_cts_hta_component_type=>ct_unknown.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_full_package must not have same reference as ct_unknown' ).
    ENDIF.

    IF ce_cts_hta_component_type=>ct_if_cts_hta_object = ce_cts_hta_component_type=>ct_if_cts_hta_package.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_object must not have same reference as ct_if_cts_hta_package' ).
    ENDIF.
    IF ce_cts_hta_component_type=>ct_if_cts_hta_object = ce_cts_hta_component_type=>ct_unknown.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_object must not have same reference as ct_unknown' ).
    ENDIF.

    IF ce_cts_hta_component_type=>ct_if_cts_hta_package = ce_cts_hta_component_type=>ct_unknown.
      cl_abap_unit_assert=>fail( 'ct_if_cts_hta_package must not have same reference as ct_unknown' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.