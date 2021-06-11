*"* use this source file for your ABAP unit test classes
CLASS ltcl_cts_hta_api_factory DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup RAISING cx_static_check,
      create_package_by_hana_name FOR TESTING RAISING cx_static_check,
      create_object_by_hana_name FOR TESTING RAISING cx_static_check.

    DATA:
      m_cut TYPE REF TO if_cts_hta_api_factory.
ENDCLASS.


CLASS ltcl_cts_hta_api_factory IMPLEMENTATION.
  METHOD setup.
    m_cut = cl_cts_hta_api_factory=>create_instance( ).
  ENDMETHOD.

  METHOD create_package_by_hana_name.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package.

    lr_hta_package = m_cut->create_package_by_hana_name( 'this.is.my.Package' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = 'THIS.IS.MY.PACKAGE' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = 'this.is.my.Package' ).
  ENDMETHOD.

  METHOD create_object_by_hana_name.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package,
          lr_hta_object  TYPE REF TO if_cts_hta_object.

    lr_hta_object = m_cut->create_object_by_hana_name(
                                i_hana_package_name = 'this.is.my.Package'
                                i_hana_object_name = 'Object_Name'
                                i_hana_object_suffix = 'Suffix' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object->if_cts_hta_component~transport_object_name exp = 'THIS.IS.MY.PACKAGE                      OBJECT_NAME' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object->object_key-hana_package_name exp = 'this.is.my.Package' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object->object_key-hana_object_name exp = 'Object_Name' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object->object_key-hana_object_suffix exp = 'Suffix' ).
  ENDMETHOD.

ENDCLASS.