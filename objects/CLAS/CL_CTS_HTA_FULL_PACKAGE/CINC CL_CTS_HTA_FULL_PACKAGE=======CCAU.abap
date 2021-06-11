*"* use this source file for your ABAP unit test classes
CLASS ltd_cts_hta_component DEFINITION FOR TESTING
INHERITING FROM cl_cts_hta_component
CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~get_deploy_state REDEFINITION,
      if_cts_hta_component~get_sync_state REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      execute_deploy REDEFINITION,
      rs_corr_check REDEFINITION,
      rs_corr_insert REDEFINITION,
      execute_sync REDEFINITION,
      hta_pre_sync_check REDEFINITION,
      read_hana_data REDEFINITION,
      read_hta_data REDEFINITION.

ENDCLASS.

CLASS ltd_cts_hta_component IMPLEMENTATION.

  METHOD if_cts_hta_component~deploy.
    cl_abap_unit_assert=>fail( 'Call to deploy not expected in testdouble' ).
  ENDMETHOD.

  METHOD execute_deploy.
    cl_abap_unit_assert=>fail( 'Call to execute_deploy not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~get_deploy_state.
    cl_abap_unit_assert=>fail( 'Call to get_deploy_state not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~get_sync_state.
    cl_abap_unit_assert=>fail( 'Call to get_sync_state not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~set_prework.
    cl_abap_unit_assert=>fail( 'Call to set_prework not expected in testdouble' ).
  ENDMETHOD.

  METHOD execute_sync.
    cl_abap_unit_assert=>fail( 'Call to execute_sync not expected in testdouble' ).
  ENDMETHOD.

  METHOD rs_corr_check.
    cl_abap_unit_assert=>fail( 'Call to rs_corr_check not expected in testdouble' ).
  ENDMETHOD.

  METHOD rs_corr_insert.
    cl_abap_unit_assert=>fail( 'Call to rs_corr_insert not expected in testdouble' ).
  ENDMETHOD.

  METHOD hta_pre_sync_check.
    cl_abap_unit_assert=>fail( 'Call to hta_pre_sync_check not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~set_deploy_mode.
    cl_abap_unit_assert=>fail( 'Call to set_deploy_mode not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~set_translation_relevance.
    cl_abap_unit_assert=>fail( 'Call to set_translation_relevance not expected in testdouble' ).
  ENDMETHOD.

  METHOD read_hana_data.
    cl_abap_unit_assert=>fail( 'Call to read_hana_data not expected in testdouble' ).
  ENDMETHOD.

  METHOD read_hta_data.
    cl_abap_unit_assert=>fail( 'Call to read_hta_data not expected in testdouble' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltd_cts_hta_package DEFINITION FINAL FOR TESTING
INHERITING FROM ltd_cts_hta_component
CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_package.

    DATA:
      hana_name TYPE cts_hot_hana_package_id READ-ONLY.

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,

      constructor
        IMPORTING
          i_hana_name   TYPE cts_hot_hana_package_id
          i_abap_status TYPE c DEFAULT cl_cts_hta_component=>co_active_version.
ENDCLASS.

CLASS ltd_cts_hta_package IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_package.
    me->if_cts_hta_component~transport_object_name = to_upper( i_hana_name ). "here only to_upper instead of correct usage of max 40 chars...
    me->hana_name = i_hana_name.
    me->m_abap_status = i_abap_status.
  ENDMETHOD.

  METHOD if_cts_hta_component~set_prework.
    cl_abap_unit_assert=>fail( 'Call to set_prework not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~deploy.
    cl_abap_unit_assert=>fail( 'Call to deploy not expected in testdouble' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltd_cts_hta_object DEFINITION FINAL FOR TESTING
INHERITING FROM ltd_cts_hta_component
CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_object.

    DATA:
      hana_package_name  TYPE cts_hot_hana_package_id READ-ONLY,
      hana_object_name   TYPE cts_hot_hana_object_name READ-ONLY,
      hana_object_suffix TYPE cts_hot_hana_object_suffix READ-ONLY.

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,

      constructor
        IMPORTING
          i_hana_package_name  TYPE cts_hot_hana_package_id
          i_hana_object_name   TYPE cts_hot_hana_object_name
          i_hana_object_suffix TYPE cts_hot_hana_object_suffix
          i_abap_status        TYPE c DEFAULT cl_cts_hta_component=>co_active_version.
ENDCLASS.

CLASS ltd_cts_hta_object IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_object.
    me->if_cts_hta_component~transport_object_name = to_upper( i_hana_package_name ). "here only to_upper instead of correct usage of max 40 chars...
    me->if_cts_hta_component~transport_object_name+40 = to_upper( i_hana_object_name && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && i_hana_object_suffix ). "here only to_upper instead of correct usage of max 40 chars...
    me->hana_package_name = i_hana_package_name.
    me->hana_object_name = i_hana_object_name.
    me->hana_object_suffix = i_hana_object_suffix.
    me->m_abap_status = i_abap_status.
  ENDMETHOD.

  METHOD if_cts_hta_component~deploy.
    cl_abap_unit_assert=>fail( 'Call to deploy not expected in testdouble' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cts_hta_full_package DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      m_cut            TYPE REF TO if_cts_hta_full_package,
      m_test_package1  TYPE REF TO if_cts_hta_package,
      m_test_package2  TYPE REF TO if_cts_hta_package,
      m_test_object1_0 TYPE REF TO if_cts_hta_object,
      m_test_object1_1 TYPE REF TO if_cts_hta_object,
      m_test_object1_2 TYPE REF TO if_cts_hta_object,
      m_test_object2   TYPE REF TO if_cts_hta_object,
      m_test_object3   TYPE REF TO if_cts_hta_object.

    METHODS:
      setup RAISING cx_static_check,
      "! Make sure that m_cut has still same pakcage and objects assigned as in setup method
      verify_m_cut_is_unchanged,
      "! Tests create instance with correct data only (1 package and 2 objects of this package)
      create_instance FOR TESTING RAISING cx_static_check,
      "! Tests create instance with wrong data (1 package and 3 objects, 1 of this package and 2 "wrong" objects from different package)
      "! Expect only 1 object part of object list
      create_instance_2 FOR TESTING RAISING cx_static_check,
      "! Test add_component with a different package and expect nothing happens because full_packages data can only be created with create_instance
      add_package FOR TESTING RAISING cx_static_check,
      "! Test add_component with a object that matches the package but must not be added because full_packages data can only be created with create_instance
      add_object FOR TESTING RAISING cx_static_check,
      "! Test remove_component with a different package and expect nothing happens because full_packages data can only be created with create_instance
      remove_package FOR TESTING RAISING cx_static_check,
      "! Test remove_component with a object that matches the package but must not be removed because full_packages data can only be created with create_instance
      remove_object FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_cts_hta_full_package IMPLEMENTATION.

  METHOD setup.
    m_test_package1 = NEW ltd_cts_hta_package( 'demo.package.1' ).
    m_test_package2 = NEW ltd_cts_hta_package( 'demo.package.2' ).
    m_test_object1_0 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.1' i_hana_object_name = 'Object1_0' i_hana_object_suffix = 'suffix1_0' ).
    m_test_object1_1 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.1' i_hana_object_name = 'Object1_1' i_hana_object_suffix = 'suffix1_1' ).
    m_test_object1_2 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.1' i_hana_object_name = 'Object1_2' i_hana_object_suffix = 'suffix1_2' ).
    m_test_object2 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.2' i_hana_object_name = 'Object2' i_hana_object_suffix = 'suffix2' ).
    m_test_object3 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.3' i_hana_object_name = 'Object3' i_hana_object_suffix = 'suffix3' ).

    m_cut = cl_cts_hta_full_package=>create_instance_full_package( i_cts_hta_package = m_test_package1
                                                                   i_cts_hta_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects(
                                                                                            ( m_test_object1_0 ) ( m_test_object1_1 ) ) ).
  ENDMETHOD.

  METHOD create_instance.
    DATA: lr_full_package TYPE REF TO if_cts_hta_full_package,
          lt_components   TYPE if_cts_hta_types=>ty_cts_hta_components.

    "no usage of m_cut here as we test creation of instance
    lr_full_package = cl_cts_hta_full_package=>create_instance_full_package( i_cts_hta_package = m_test_package1
                                                                             i_cts_hta_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects(
                                                                                            ( m_test_object1_0 ) ( m_test_object1_1 ) ) ).

    cl_abap_unit_assert=>assert_equals( act = lr_full_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_full_package->transport_object_name exp = m_test_package1->transport_object_name ).
    lt_components = lr_full_package->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).
    lt_components = lr_full_package->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1_0 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_object1_1 ).
  ENDMETHOD.

  METHOD create_instance_2.
    DATA: lr_full_package TYPE REF TO if_cts_hta_full_package,
          lt_components   TYPE if_cts_hta_types=>ty_cts_hta_components.

    "no usage of m_cut here as we test creation of instance
    lr_full_package = cl_cts_hta_full_package=>create_instance_full_package( i_cts_hta_package = m_test_package1
                                                                             i_cts_hta_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects(
                                                                                            ( m_test_object1_0 ) ( m_test_object2 ) ( m_test_object3 ) ) ).

    cl_abap_unit_assert=>assert_equals( act = lr_full_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_full_package->transport_object_name exp = m_test_package1->transport_object_name ).
    lt_components = lr_full_package->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).
    lt_components = lr_full_package->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1_0 ).
  ENDMETHOD.

  METHOD add_object.
    DATA: lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_2 ).

    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    verify_m_cut_is_unchanged( ).
  ENDMETHOD.

  METHOD add_package.
    DATA: lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).

    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    verify_m_cut_is_unchanged( ).
  ENDMETHOD.

  METHOD remove_object.
    DATA: lv_remove_result TYPE abap_bool.

    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_object1_0 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    verify_m_cut_is_unchanged( ).
  ENDMETHOD.

  METHOD remove_package.
    DATA: lv_remove_result TYPE abap_bool.

    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    verify_m_cut_is_unchanged( ).
  ENDMETHOD.

  METHOD verify_m_cut_is_unchanged.
    DATA: lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).
    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1_0 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_object1_1 ).
  ENDMETHOD.


ENDCLASS.