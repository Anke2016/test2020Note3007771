CLASS ltd_hta_package DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_package PARTIALLY IMPLEMENTED.

    METHODS:
      constructor
        IMPORTING
          transport_object_name TYPE if_cts_hta_types=>ty_transport_object_name.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_hta_package IMPLEMENTATION.
  METHOD constructor.
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_package.
    me->if_cts_hta_component~transport_object_name = transport_object_name.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_hta_object DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_object PARTIALLY IMPLEMENTED.

    METHODS:
      constructor
        IMPORTING
          transport_object_name TYPE if_cts_hta_types=>ty_transport_object_name.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_hta_object IMPLEMENTATION.
  METHOD constructor.
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_object.
    me->if_cts_hta_component~transport_object_name = transport_object_name.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_hta_full_package DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_full_package PARTIALLY IMPLEMENTED.

    METHODS:
      constructor
        IMPORTING
          transport_object_name TYPE if_cts_hta_types=>ty_transport_object_name.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_hta_full_package IMPLEMENTATION.
  METHOD constructor.
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
    me->if_cts_hta_component~transport_object_name = transport_object_name.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_cx_cts_hta DEFINITION FINAL FOR TESTING
         DURATION SHORT
         RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Test that cts_hta_component is not set to full package if it is not bound
      set_full_package_no_hta_comp FOR TESTING RAISING cx_static_check,
      "! Test that cts_hta_component is not set to full package if it does not belong to same package. Current cts_hta_component is a package.
      set_full_package_different_pk1 FOR TESTING RAISING cx_static_check,
      "! Test that cts_hta_component is not set to full package if it does not belong to same package. Current cts_hta_component is an object.
      set_full_package_different_pk2 FOR TESTING RAISING cx_static_check,
      "! Test that cts_hta_component is set to full package if it belongs to same package. Current cts_hta_component is a package.
      set_full_package_same_package1 FOR TESTING RAISING cx_static_check,
      "! Test that cts_hta_component is set to full package if it belongs to same package. Current cts_hta_component is an object.
      set_full_package_same_package2 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_cx_cts_hta IMPLEMENTATION.

  METHOD set_full_package_no_hta_comp.
    DATA(lr_cx) = NEW cx_cts_hta( ).

    lr_cx->set_full_package_as_component( CAST if_cts_hta_full_package( cl_abap_testdouble=>create( 'if_cts_hta_full_package' ) ) ).

    cl_abap_unit_assert=>assert_not_bound( lr_cx->cts_hta_component ).
  ENDMETHOD.

  METHOD set_full_package_different_pk1.
    DATA(lr_package) = NEW ltd_hta_package( 'ABC' ).
    DATA(lr_full_package) = NEW ltd_hta_full_package( 'XYZ' ).

    DATA(lr_cx) = NEW cx_cts_hta( cts_hta_component = lr_package ).

    lr_cx->set_full_package_as_component( lr_full_package ).

    cl_abap_unit_assert=>assert_equals( act = lr_cx->cts_hta_component exp = lr_package ).
  ENDMETHOD.

  METHOD set_full_package_different_pk2.
    DATA(lr_object) = NEW ltd_hta_object( 'ABC' ).
    DATA(lr_full_package) = NEW ltd_hta_full_package( 'XYZ' ).

    DATA(lr_cx) = NEW cx_cts_hta( cts_hta_component = lr_object ).

    lr_cx->set_full_package_as_component( lr_full_package ).

    cl_abap_unit_assert=>assert_equals( act = lr_cx->cts_hta_component exp = lr_object ).
  ENDMETHOD.

  METHOD set_full_package_same_package1.
    DATA(lr_package) = NEW ltd_hta_package( 'ABC' ).
    DATA(lr_full_package) = NEW ltd_hta_full_package( 'ABC' ).

    DATA(lr_cx) = NEW cx_cts_hta( cts_hta_component = lr_package ).

    lr_cx->set_full_package_as_component( lr_full_package ).

    cl_abap_unit_assert=>assert_equals( act = lr_cx->cts_hta_component exp = lr_full_package ).
  ENDMETHOD.

  METHOD set_full_package_same_package2.
    DATA(lr_object) = NEW ltd_hta_object( 'ABC' ).
    DATA(lr_full_package) = NEW ltd_hta_full_package( 'ABC' ).

    DATA(lr_cx) = NEW cx_cts_hta( cts_hta_component = lr_object ).

    lr_cx->set_full_package_as_component( lr_full_package ).

    cl_abap_unit_assert=>assert_equals( act = lr_cx->cts_hta_component exp = lr_full_package ).
  ENDMETHOD.

ENDCLASS.