*"* use this source file for your ABAP unit test classes
CLASS ltd_cts_hta_component DEFINITION FOR TESTING
INHERITING FROM cl_cts_hta_component
CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~get_deploy_state REDEFINITION,
      if_cts_hta_component~get_sync_state REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION.

    DATA:
      deploy_state         TYPE REF TO ce_cts_hta_deploy_state,
      sync_state           TYPE REF TO ce_cts_hta_sync_state,
      reasons_not_syncable TYPE if_cts_hta_types=>ty_cx_cts_htas.

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
    cl_abap_unit_assert=>fail( 'Call to set_prework not expected in testdouble' ).
  ENDMETHOD.

  METHOD execute_deploy.
    cl_abap_unit_assert=>fail( 'Call to set_prework not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~get_deploy_state.
    r_result = me->deploy_state.
  ENDMETHOD.

  METHOD if_cts_hta_component~get_sync_state.
    r_result = me->sync_state.
    e_reasons_can_not_be_synced = reasons_not_syncable.
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
      hana_name    TYPE cts_hot_hana_package_id READ-ONLY.

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,

      constructor
        IMPORTING
          i_hana_name   TYPE cts_hot_hana_package_id
          i_abap_status TYPE c DEFAULT cl_cts_hta_component=>co_active_version
        RAISING
          cx_hana_object_transport.
ENDCLASS.

CLASS ltd_cts_hta_package IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_package.
    me->if_cts_hta_component~transport_object_name = cl_cts_hot_package=>create_instance( i_hana_name )->abap_hana_package_id.
    me->hana_name = i_hana_name.
    me->m_abap_status = i_abap_status.
    me->deploy_state =  ce_cts_hta_deploy_state=>not_deployed.
    me->sync_state = ce_cts_hta_sync_state=>in_sync.
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

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,

      constructor
        IMPORTING
          i_hana_package_name  TYPE cts_hot_hana_package_id
          i_hana_object_name   TYPE cts_hot_hana_object_name
          i_hana_object_suffix TYPE cts_hot_hana_object_suffix
          i_abap_status        TYPE c DEFAULT cl_cts_hta_component=>co_active_version
        RAISING
          cx_hana_object_transport.
ENDCLASS.

CLASS ltd_cts_hta_object IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_object.
    me->if_cts_hta_component~transport_object_name = cl_cts_hot_object_v1=>create_instance( iv_hana_package_id = i_hana_package_name
                                                                                            iv_hana_object_name = i_hana_object_name
                                                                                            iv_hana_object_suffix = i_hana_object_suffix )->transport_object_name.
    me->if_cts_hta_object~object_key-hana_package_name = i_hana_package_name.
    me->if_cts_hta_object~object_key-hana_object_name = i_hana_object_name.
    me->if_cts_hta_object~object_key-hana_object_suffix = i_hana_object_suffix.
    me->m_abap_status = i_abap_status.
    me->deploy_state = ce_cts_hta_deploy_state=>not_deployed.
    me->sync_state = ce_cts_hta_sync_state=>in_sync.
  ENDMETHOD.

  METHOD if_cts_hta_component~deploy.
    cl_abap_unit_assert=>fail( 'Call to deploy not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~set_prework.
    cl_abap_unit_assert=>fail( 'Call to set_prework not expected in testdouble' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltd_cts_hta_full_package DEFINITION FINAL FOR TESTING
INHERITING FROM ltd_cts_hta_component
CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_full_package.

    DATA:
      package TYPE REF TO if_cts_hta_package READ-ONLY,
      objects TYPE if_cts_hta_types=>ty_cts_hta_objects READ-ONLY.

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,

      constructor
        IMPORTING
          i_package     TYPE REF TO if_cts_hta_package
          i_objects     TYPE if_cts_hta_types=>ty_cts_hta_objects
          i_abap_status TYPE c DEFAULT cl_cts_hta_component=>co_active_version.
ENDCLASS.

CLASS ltd_cts_hta_full_package IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
    me->if_cts_hta_component~transport_object_name = i_package->transport_object_name.
    me->package = i_package.
    me->objects = i_objects.
    me->m_abap_status = i_abap_status.
    me->deploy_state =  ce_cts_hta_deploy_state=>not_deployed.
    me->sync_state =  ce_cts_hta_sync_state=>in_sync.
  ENDMETHOD.

  METHOD if_cts_hta_component~deploy.
    cl_abap_unit_assert=>fail( 'Call to deploy not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component_list~add_component.
    cl_abap_unit_assert=>fail( 'Call to add_component not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component_list~get_components.
    IF i_component_type = ce_cts_hta_component_type=>ct_if_cts_hta_package.
      APPEND me->package TO r_result.
    ELSEIF  i_component_type = ce_cts_hta_component_type=>ct_if_cts_hta_object.
      r_result = me->objects.
    ELSE.
      cl_abap_unit_assert=>fail( 'Call to get_components not expected in testdouble' ).
    ENDIF.
  ENDMETHOD.

  METHOD if_cts_hta_component_list~remove_component.
    cl_abap_unit_assert=>fail( 'Call to remove_component not expected in testdouble' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_cts_hta_component_list DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      m_cut                         TYPE REF TO cl_cts_hta_component_list,
      m_test_package1               TYPE REF TO if_cts_hta_package,
      m_test_package2               TYPE REF TO if_cts_hta_package,
      m_test_package3               TYPE REF TO if_cts_hta_package,
      m_test_package1_inactive      TYPE REF TO if_cts_hta_package,
      m_test_package2_inactive      TYPE REF TO if_cts_hta_package,
      m_test_package3_inactive      TYPE REF TO if_cts_hta_package,
      m_test_object1                TYPE REF TO if_cts_hta_object,
      m_test_object2                TYPE REF TO if_cts_hta_object,
      m_test_object3                TYPE REF TO if_cts_hta_object,
      m_test_object1_inactive       TYPE REF TO if_cts_hta_object,
      m_test_object2_inactive       TYPE REF TO if_cts_hta_object,
      m_test_object3_inactive       TYPE REF TO if_cts_hta_object,
      m_test_full_package1          TYPE REF TO if_cts_hta_full_package,
      m_test_full_package2          TYPE REF TO if_cts_hta_full_package,
      m_test_full_package3          TYPE REF TO if_cts_hta_full_package,
      m_test_full_package1_inactive TYPE REF TO if_cts_hta_full_package,
      m_test_full_package2_inactive TYPE REF TO if_cts_hta_full_package,
      m_test_full_package3_inactive TYPE REF TO if_cts_hta_full_package.

    METHODS:
      setup RAISING cx_static_check,
      "! Adds a not bound component
      add_not_bound FOR TESTING RAISING cx_static_check,
      "! Adds a package to the list
      add_package FOR TESTING RAISING cx_static_check,
      "! Adds same package to the list (expect that 2nd add does not add the package as it only can exist once in the list)
      add_package_twice FOR TESTING RAISING cx_static_check,
      "! Adds several packages to the list
      add_packages FOR TESTING RAISING cx_static_check,
      "! Adds a package (limu hotp) to the list where the full package (r3tr hota) already exists. Add should reutn abap_false.
      add_package_full_package_exist FOR TESTING RAISING cx_static_check,
      "! Adds a full package to the list
      add_full_package FOR TESTING RAISING cx_static_check,
      "! Adds same full package to the list (expect that 2nd add does not add the package as it only can exist once in the list)
      add_full_package_twice FOR TESTING RAISING cx_static_check,
      "! Adds several full packages to the list
      add_full_packages FOR TESTING RAISING cx_static_check,
      "! Adds an object to the list
      add_object FOR TESTING RAISING cx_static_check,
      "! Adds same object to the list (expect that 2nd add does not add the object as it only can exist once in the list)
      add_object_twice FOR TESTING RAISING cx_static_check,
      "! Adds several objects to the list
      add_objects FOR TESTING RAISING cx_static_check,
      "! Adds an object (limu hoto) to the list where the full package (r3tr hota) already exists. Add should reutn abap_false.
      add_object_full_package_exist FOR TESTING RAISING cx_static_check,
      "! Adds 2 inactive packages, 2 inactive objects and 1 inactive full package
      add_packages_and_objects_inact FOR TESTING RAISING cx_static_check,
      "! Adds 2 packages, first inactive then active and then vice versa<br/>
      "! Mixed abap_status not allowed and therefore 2nd add should not be done.
      add_packages_active_and_inact FOR TESTING RAISING cx_static_check,
      "! Adds 2 full packages, first inactive then active and then vice versa<br/>
      "! Mixed abap_status not allowed and therefore 2nd add should not be done.
      add_full_packages_act_and_inac FOR TESTING RAISING cx_static_check,
      "! Adds 2 objects, first inactive then active and then vice versa<br/>
      "! Mixed abap_status not allowed and therefore 2nd add should not be done.
      add_objects_active_and_inact FOR TESTING RAISING cx_static_check,
      "! Adds package, full package and object. package ctive and full package inactive and then vice versa.
      "! package active and object inactive and then vice versa. full package active and object inactive and then vice versa.
      "! and finally mixing inactive and active<br/>
      "! Mixed abap_status not allowed and therefore 2nd add should not be done.
      add_pack_obj_fullpack_act_inac FOR TESTING RAISING cx_static_check,
      "! Tests add of a list. (List itself should not be added, but it's parts
      add_list FOR TESTING RAISING cx_static_check,
      "! Tests add of a list with result abap_false because at least 1 package was not added but some items were added.
      add_list_false_packages FOR TESTING RAISING cx_static_check,
      "! Tests add of a list with result abap_false because at least 1 full package was not added but some items were added.
      add_list_false_full_packages FOR TESTING RAISING cx_static_check,
      "! Tests add of a list with result abap_false because at least 1 object was not added but some items were added.
      add_list_false_objects FOR TESTING RAISING cx_static_check,
      "! Tests add of a list to a list of different abap_status
      add_list_active_and_inact FOR TESTING RAISING cx_static_check,
      "! Test that an add of a full package removes the package (hotp) if it is part of the full package.
      add_full_package_replaces_hotp FOR TESTING RAISING cx_static_check,
      "! Test that an add of a full package removes 2 objects if they are part of the full package.
      add_full_package_replaces_hoto FOR TESTING RAISING cx_static_check,
      "! Tests remove for not bound input.
      remove_not_bound FOR TESTING RAISING cx_static_check,
      "! Tests removal of a package (active and inactive) and for not existing package
      remove_package FOR TESTING RAISING cx_static_check,
      "! Tests removal of a full package (active and inactive) and for not existing full package
      remove_full_package FOR TESTING RAISING cx_static_check,
      "! Tests removal of an object (active and inactive) and for not existing object
      remove_object FOR TESTING RAISING cx_static_check,
      "! Tests removal of a list (it's parts should be removed)
      remove_list FOR TESTING RAISING cx_static_check,
      "! Tests removal of a list with result abap_false because at least 1 package was not removed but some items were removed.
      remove_list_false_packages FOR TESTING RAISING cx_static_check,
      "! Tests removal of a list with result abap_false because at least 1 full package was not removed but some items were removed.
      remove_list_false_full_packags FOR TESTING RAISING cx_static_check,
      "! Tests removal of a list with result abap_false because at least 1 object was not removed but some items were removed.
      remove_list_false_objects FOR TESTING RAISING cx_static_check,
      "! Tests remove of a list from a list of different abap_status
      remove_list_active_and_inact FOR TESTING RAISING cx_static_check,
      "! Tests get_deploy_state for list with ALL packages/full packages/objects are deployed
      get_deploy_state_deployed FOR TESTING RAISING cx_static_check,
      "! Tests get_deploy_state for list for which only some packages/full packages/objects are deployed
      get_deploy_state_partly_deploy FOR TESTING RAISING cx_static_check,
      "! Tests get_deploy_state for list with not any deployed packages/full packages/objects
      get_deploy_state_not_deployed FOR TESTING RAISING cx_static_check,

      "! Tests get_sync_state for list of components all in sync.
      get_sync_state_in_sync FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for an empty list of components. Result should be in_sync
      get_sync_state_in_sync_empty FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 2 packages not in sync.
      get_sync_state_not_in_sync_pk FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 2 objects not in sync.
      get_sync_state_not_in_sync_ob FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 2 full packages not in sync.
      get_sync_state_not_in_sync_fp FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 1 package, 1 object and 1 full packages not in sync.
      get_sync_state_not_in_sync_all FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 2 packages can not by synchronized, 1 due to wrong status, 1 due to name conflict
      get_sync_state_not_syncable_pk FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 2 objects can not by synchronized, 1 due to wrong status, 1 due to name conflict
      get_sync_state_not_syncable_ob FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 2 full packages can not by synchronized, 1 due to wrong status, 1 due to name conflict
      get_sync_state_not_syncable_fp FOR TESTING RAISING cx_static_check,
      "! Tests get_sync_state for list of components and 1 package, 1 object and 1 full package can not by synchronized.
      get_sync_state_not_syncable_al FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS cl_cts_hta_component_list DEFINITION LOCAL FRIENDS ltcl_cts_hta_component_list.
CLASS ltcl_cts_hta_component_list IMPLEMENTATION.

  METHOD setup.
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    m_test_package1 = NEW ltd_cts_hta_package( 'demo.package.1' ).
    m_test_package2 = NEW ltd_cts_hta_package( 'demo.package.2' ).
    m_test_package3 = NEW ltd_cts_hta_package( 'demo.package.3' ).
    m_test_package1_inactive = NEW ltd_cts_hta_package( i_hana_name = 'demo.package.1' i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_package2_inactive = NEW ltd_cts_hta_package( i_hana_name = 'demo.package.2' i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_package3_inactive = NEW ltd_cts_hta_package( i_hana_name = 'demo.package.3' i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_object1 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.1' i_hana_object_name = 'Object1' i_hana_object_suffix = 'suffix1' ).
    m_test_object2 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.2' i_hana_object_name = 'Object2' i_hana_object_suffix = 'suffix2' ).
    m_test_object3 = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.3' i_hana_object_name = 'Object3' i_hana_object_suffix = 'suffix3' ).
    m_test_object1_inactive = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.1' i_hana_object_name = 'Object1' i_hana_object_suffix = 'suffix1' i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_object2_inactive = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.2' i_hana_object_name = 'Object2' i_hana_object_suffix = 'suffix2' i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_object3_inactive = NEW ltd_cts_hta_object( i_hana_package_name = 'demo.package.3' i_hana_object_name = 'Object3' i_hana_object_suffix = 'suffix3' i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_full_package1 = NEW ltd_cts_hta_full_package( i_package = m_test_package1 i_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( m_test_object1 ) ) ).
    m_test_full_package2 = NEW ltd_cts_hta_full_package( i_package = m_test_package2 i_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( m_test_object2 ) ) ).
    m_test_full_package3 = NEW ltd_cts_hta_full_package( i_package = m_test_package3 i_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( m_test_object3 ) ) ).
    m_test_full_package1_inactive = NEW ltd_cts_hta_full_package( i_package = m_test_package1_inactive i_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( m_test_object1_inactive ) ) i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_full_package2_inactive = NEW ltd_cts_hta_full_package( i_package = m_test_package2_inactive i_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( m_test_object2_inactive ) ) i_abap_status = cl_cts_hta_component=>co_inactive_version ).
    m_test_full_package3_inactive = NEW ltd_cts_hta_full_package( i_package = m_test_package3_inactive i_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( m_test_object3_inactive ) ) i_abap_status = cl_cts_hta_component=>co_inactive_version ).
  ENDMETHOD.

  METHOD add_package.
    DATA: lt_packages   TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_packages = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 1 ] exp = m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_package_twice.
    DATA: lt_packages   TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_packages = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 1 ] exp = m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_packages.
    DATA: lt_packages   TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package3 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_packages = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 3 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 1 ] exp = m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 2 ] exp = m_test_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 3 ] exp = m_test_package3 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_object.
    DATA: lt_objects    TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_objects = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_objects ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects[ 1 ] exp = m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_object_twice.
    DATA: lt_objects    TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_objects = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_objects ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects[ 1 ] exp = m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_objects.
    DATA: lt_objects    TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object3 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_objects = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_objects ) exp = 3 ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects[ 1 ] exp = m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects[ 2 ] exp = m_test_object2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects[ 3 ] exp = m_test_object3 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_packages_and_objects_inact.
    DATA: lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package3_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_object2_inactive ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_package2_inactive ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package3_inactive ).
  ENDMETHOD.

  METHOD add_packages_active_and_inact.
    DATA: lv_add_result TYPE abap_bool,
          lt_packages   TYPE if_cts_hta_types=>ty_cts_hta_components.

    "1. Test - add active first and then inactive (same and different package)
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_packages = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 1 ] exp = m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "2. Test - add inactive first and then active
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_packages = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_packages[ 1 ] exp = m_test_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_objects_active_and_inact.
    DATA: lv_add_result TYPE abap_bool,
          lt_objects    TYPE if_cts_hta_types=>ty_cts_hta_components.

    "1. Test - add active first and then inactive (same and different object)
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_objects = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_objects ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects[ 1 ] exp = m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).

    "2. Test - add inactive first and then active
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_objects = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_objects ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects[ 1 ] exp = m_test_object1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_pack_obj_fullpack_act_inac.
    DATA: lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "1. Test - add active package and inactive full package
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "2. Test - add inactive package and active full package
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "3. Test - add active package and inactive object
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "4. Test - add inactive package and active object
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "5. Test - add active full package and inactive object
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).

    "6. Test - add inactive full package and active object
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).

    "7. Test - add active full package and inactive package
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).

    "8. Test - add inactive full package and active package
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).

    "9. Test - add active object and inactive package
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "10. Test - add inactive object and active package
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "11. Test - add active object and inactive full package
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

    "10. Test - add inactive object and active full package
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_object1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_not_bound.
    DATA: lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD remove_not_bound.
    DATA: lv_remove_result TYPE abap_bool.

    "prepare some data in list to check that nothing was removed...
    m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package2 ).

    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).
  ENDMETHOD.

  METHOD remove_package.
    DATA: lv_remove_result TYPE abap_bool.

    "prepare some data in list to check that nothing was removed...
    m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package3 ).

    "1. Test: remove not existing package
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_package3 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).

    "2. Test: remove inactive package
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).

    "3. Test: remove correct package
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).

    "4. Test: remove removed package again
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).
  ENDMETHOD.

  METHOD remove_object.
    DATA: lv_remove_result TYPE abap_bool.

    "prepare some data in list to check that nothing was removed...
    m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package3 ).

    "1. Test: remove not existing object
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_object3 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).

    "2. Test: remove inactive object
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_object1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).

    "3. Test: remove correct object
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).

    "4. Test: remove removed object again
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).
  ENDMETHOD.

  METHOD add_list.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.
    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_package1 ).
    lr_list->add_component( m_test_package2 ).
    lr_list->add_component( m_test_object1 ).
    lr_list->add_component( m_test_full_package3 ).

    "execute business method
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( lr_list ).

    "Validate results
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_package2 ).
    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object1 ).
    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package3 ).
  ENDMETHOD.

  METHOD remove_list.
    DATA: lr_list          TYPE REF TO if_cts_hta_component_list,
          lv_remove_result TYPE abap_bool,
          lt_components    TYPE if_cts_hta_types=>ty_cts_hta_components.

    "prepare
    m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package3 ).
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_package2 ).
    lr_list->add_component( m_test_object1 ).

    "execute business method
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( lr_list ).

    "Validate results
    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_true ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).
    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object2 ).
    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package3 ).
  ENDMETHOD.

  METHOD add_list_false_packages.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_package1 ).
    lr_list->add_component( m_test_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_list_false_objects.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_object1 ).
    lr_list->add_component( m_test_object2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object2 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_object1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD remove_list_false_objects.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_object1 ).
    lr_list->add_component( m_test_object2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object3 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~remove_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object3 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD remove_list_false_packages.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_package1 ).
    lr_list->add_component( m_test_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package3 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~remove_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package3 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_list_active_and_inact.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

* 1. Test add active to inactive
    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_object1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object2_inactive ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object2_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

* 2. Test add inactive to active
    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_object1_inactive ).
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object2 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object2 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

  ENDMETHOD.

  METHOD remove_list_active_and_inact.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

* 1. Test add active and try to remove inactive
    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_package1_inactive ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~remove_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).

* 2. Test add inactive and try to remove active
    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_package1 ).
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package1_inactive ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~remove_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_full_package.
    DATA: lt_components TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_full_packages.
    DATA: lt_components TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package3 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 3 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_full_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 3 ] exp = m_test_full_package3 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_full_package_twice.
    DATA: lt_components TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_full_packages_act_and_inac.
    DATA: lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "1. Test - add active first and then inactive (same and different package)
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package2_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).

    "2. Test - add inactive first and then active
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1_inactive ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( m_test_full_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_list_false_full_packages.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_full_package1 ).
    lr_list->add_component( m_test_full_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package2 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~add_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 2 ] exp = m_test_full_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD remove_full_package.
    DATA: lv_remove_result TYPE abap_bool.

    "prepare some data in list to check that nothing was removed...
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package3 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_package1 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_object1 ).

    "1. Test: remove not existing full package
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_full_package1 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).

    "2. Test: remove inactive full package
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_package1_inactive ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).

    "3. Test: remove correct full package
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_full_package2 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).

    "4. Test: remove removed full package again
    lv_remove_result = m_cut->if_cts_hta_component_list~remove_component( m_test_full_package2 ).

    cl_abap_unit_assert=>assert_equals( act = lv_remove_result exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
  ENDMETHOD.

  METHOD remove_list_false_full_packags.
    DATA: lr_list       TYPE REF TO if_cts_hta_component_list,
          lv_add_result TYPE abap_bool,
          lt_components TYPE if_cts_hta_types=>ty_cts_hta_components.

    "prepare
    lr_list = cl_cts_hta_component_list=>create_instance( ).
    lr_list->add_component( m_test_full_package1 ).
    lr_list->add_component( m_test_full_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package2 ).
    m_cut->if_cts_hta_component_list~add_component( m_test_full_package3 ).

    "Execute
    lv_add_result = m_cut->if_cts_hta_component_list~remove_component( lr_list ).

    "validate
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package3 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
  ENDMETHOD.

  METHOD get_deploy_state_deployed.
    "1. Test: get_deploy_state on empty list returns deployed
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>deployed ).


    "default in test doubles is not_deploy, therefore set expected state
    CAST ltd_cts_hta_package( m_test_package1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_package1 ).
    CAST ltd_cts_hta_package( m_test_package2 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_package2 ).
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_full_package3 ).
    CAST ltd_cts_hta_object( m_test_object1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_object1 ).
    CAST ltd_cts_hta_object( m_test_object2 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_object2 ).

    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_not_deployed.
    "default in test doubles is not_deployed
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    "1. Test: With objects/packages/full package
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).

    "2. Test: Without full package
    m_cut->remove_component( m_test_full_package3 ).
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_partly_deploy.
    "1. Test: 1 package, 1 full package, 1 object deployed and 1 package not
    CAST ltd_cts_hta_package( m_test_package1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_package1 ).
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_full_package3 ).
    CAST ltd_cts_hta_object( m_test_object1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_package2 ). "default in test doubles is not_deployed, therefore setting deploy_state not needed

    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>partly_deployed ).

    "2. Test: 1 package, 1 full package, 1 object deployed and 1 full package not
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    CAST ltd_cts_hta_package( m_test_package1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_package1 ).
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_full_package3 ).
    CAST ltd_cts_hta_object( m_test_object1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_full_package2 ). "default in test doubles is not_deployed, therefore setting deploy_state not needed

    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>partly_deployed ).

    "3. Test: 1 package, 1 full package, 1 object deployed and 1 object not
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    CAST ltd_cts_hta_package( m_test_package1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_package1 ).
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_full_package3 ).
    CAST ltd_cts_hta_object( m_test_object1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ). "default in test doubles is not_deployed, therefore setting deploy_state not needed

    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>partly_deployed ).

    "4. Test: only 1 full package partly_deployed
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->deploy_state = ce_cts_hta_deploy_state=>partly_deployed.
    m_cut->add_component( m_test_full_package3 ).

    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>partly_deployed ).

    "4. Test: 1 full package deployed and 1 full package not deployed
    m_cut = CAST cl_cts_hta_component_list( cl_cts_hta_component_list=>create_instance( ) ).
    CAST ltd_cts_hta_full_package( m_test_full_package1 )->deploy_state = ce_cts_hta_deploy_state=>deployed.
    m_cut->add_component( m_test_full_package1 ).
    m_cut->add_component( m_test_full_package2 ). "default in test doubles is not_deployed, therefore setting deploy_state not needed

    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>partly_deployed ).
  ENDMETHOD.

  METHOD add_full_package_replaces_hoto.
    DATA: lt_components     TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result     TYPE abap_bool,
          lr_test_object1_1 TYPE REF TO if_cts_hta_object.

    "prepare data in list
    lr_test_object1_1 = NEW ltd_cts_hta_object( i_hana_package_name = m_test_object1->object_key-hana_package_name i_hana_object_name = 'Object1_1' i_hana_object_suffix = 'suffix1_1' ).

    lv_add_result = m_cut->add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->add_component( lr_test_object1_1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->add_component( m_test_object2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    "add full package
    lv_add_result = m_cut->add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    "verify
    lt_components = m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).

    lt_components = m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_object2 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_full_package_replaces_hotp.
    DATA: lt_components TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool.

    "prepare data in list
    lv_add_result = m_cut->add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->add_component( m_test_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    "add full package
    lv_add_result = m_cut->add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).

    "verify
    lt_components = m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).

    lt_components = m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_package2 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_object_full_package_exist.
    DATA: lt_components TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool,
          lr_component  TYPE REF TO if_cts_hta_component_list.

    lv_add_result = m_cut->add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->add_component( m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).
    lr_component = CAST if_cts_hta_component_list( lt_components[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( act = lines( lr_component->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lr_component->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD add_package_full_package_exist.
    DATA: lt_components TYPE if_cts_hta_types=>ty_cts_hta_components,
          lv_add_result TYPE abap_bool,
          lr_component  TYPE REF TO if_cts_hta_component_list.

    lv_add_result = m_cut->add_component( m_test_full_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_true ).
    lv_add_result = m_cut->add_component( m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lv_add_result exp = abap_false ).

    lt_components = m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_components ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lt_components[ 1 ] exp = m_test_full_package1 ).
    lr_component = CAST if_cts_hta_component_list( lt_components[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( act = lines( lr_component->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lr_component->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ) ) exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( m_cut->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ) ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_in_sync.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "default in test doubles is in_sync
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_in_sync_empty.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "m_cut is empty by default
    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_pk.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    "default in test doubles is in_sync, therefore set not_in_sync once
    CAST ltd_cts_hta_package( m_test_package1 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.
    CAST ltd_cts_hta_package( m_test_package2 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_ob.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    "default in test doubles is in_sync, therefore set not_in_sync once
    CAST ltd_cts_hta_object( m_test_object1 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.
    CAST ltd_cts_hta_object( m_test_object2 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_fp.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    "default in test doubles is in_sync, therefore set not_in_sync once
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD..

  METHOD get_sync_state_not_in_sync_all.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    "default in test doubles is in_sync, therefore set not_in_sync once
    CAST ltd_cts_hta_package( m_test_package1 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.
    CAST ltd_cts_hta_object( m_test_object1 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->sync_state = ce_cts_hta_sync_state=>not_in_sync.

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_pk.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).

    "default in test doubles is in_sync, therefore set can_not_be_synchronized
    CAST ltd_cts_hta_package( m_test_package1 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_package( m_test_package1 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_wrong_status(
                                        textid                 = cx_cts_hta_wrong_status=>package_requires_deployment
                                        hot_status             = if_cts_hot_db_access=>co_hot_status_inactive
                                        name_of_obj_or_package = m_test_package1->hana_package_name
                                        cts_hta_component      = m_test_package1 ) ) ).
    CAST ltd_cts_hta_package( m_test_package2 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_package( m_test_package2 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_name_conflict(
                                        textid                      = cx_cts_hta_name_conflict=>package_name_conflict
                                        name_of_obj_or_package_conf = m_test_package2->hana_package_name
                                        name_of_obj_or_package_hta  = m_test_package2->hana_package_name
                                        cts_hta_component           = m_test_package2 ) ) ).

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 2 msg = 'Reason for can_not_be_synchronized missing' ).
    DATA(lr_cx_cts_hta_wrong_status) = CAST cx_cts_hta_wrong_status( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->cts_hta_component exp = m_test_package1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>package_requires_deployment ).
    DATA(lr_cx_cts_hta_name_conflict) = CAST cx_cts_hta_name_conflict( lt_reasons[ 2 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->cts_hta_component exp = m_test_package2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>package_name_conflict ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_ob.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    "default in test doubles is in_sync, therefore set can_not_be_synchronized
    CAST ltd_cts_hta_object( m_test_object1 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_object( m_test_object1 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_wrong_status(
                                        textid                 = cx_cts_hta_wrong_status=>object_requires_deployment
                                        hot_status             = if_cts_hot_db_access=>co_hot_status_to_be_deleted
                                        name_of_obj_or_package = m_test_object1->object_key-hana_object_name && '.' && m_test_object1->object_key-hana_object_suffix
                                        cts_hta_component      = m_test_object1 ) ) ).

    CAST ltd_cts_hta_object( m_test_object2 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_object( m_test_object2 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_name_conflict(
                                        textid                      = cx_cts_hta_name_conflict=>object_name_conflict
                                        name_of_obj_or_package_conf = m_test_object2->object_key-hana_object_name
                                        name_of_obj_or_package_hta  = m_test_object2->object_key-hana_object_name
                                        cts_hta_component           = m_test_object2 ) ) ).

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 2 msg = 'Reason for can_not_be_synchronized missing' ).
    DATA(lr_cx_cts_hta_wrong_status) = CAST cx_cts_hta_wrong_status( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->cts_hta_component exp = m_test_object1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>object_requires_deployment ).
    DATA(lr_cx_cts_hta_name_conflict) = CAST cx_cts_hta_name_conflict( lt_reasons[ 2 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->cts_hta_component exp = m_test_object2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>object_name_conflict ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_fp.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_full_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).

    "default in test doubles is in_sync, therefore set can_not_be_synchronized
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_wrong_status(
                                        textid                 = cx_cts_hta_wrong_status=>object_requires_deployment
                                        hot_status             = if_cts_hot_db_access=>co_hot_status_to_be_deleted
                                        name_of_obj_or_package = m_test_object3->object_key-hana_object_name && '.' && m_test_object3->object_key-hana_object_suffix
                                        cts_hta_component      = m_test_object3 ) )
                                  ( NEW cx_cts_hta_name_conflict(
                                        textid                      = cx_cts_hta_name_conflict=>object_name_conflict
                                        name_of_obj_or_package_conf = m_test_package3->hana_package_name
                                        name_of_obj_or_package_hta  = m_test_package3->hana_package_name
                                        cts_hta_component           = m_test_package3 ) ) ).

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 2 msg = 'Reasons for can_not_be_synchronized missing' ).
    LOOP AT lt_reasons INTO DATA(lr_reason).
      CASE lr_reason->if_t100_message~t100key.
        WHEN cx_cts_hta_wrong_status=>object_requires_deployment.
          cl_abap_unit_assert=>assert_equals( act = lr_reason->cts_hta_component exp = m_test_full_package3 ).
        WHEN cx_cts_hta_name_conflict=>object_name_conflict.
          cl_abap_unit_assert=>assert_equals( act = lr_reason->cts_hta_component exp = m_test_full_package3 ).
        WHEN OTHERS.
          cl_abap_unit_assert=>fail( 'Found unexpected reason for can_not_be_synchronized' ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_al.
    DATA: lr_sync_state TYPE REF TO ce_cts_hta_sync_state,
          lt_reasons    TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " prepare component list
    m_cut->add_component( m_test_package1 ).
    m_cut->add_component( m_test_package2 ).
    m_cut->add_component( m_test_full_package3 ).
    m_cut->add_component( m_test_object1 ).
    m_cut->add_component( m_test_object2 ).

    "default in test doubles is in_sync, therefore set can_not_be_synchronized
    CAST ltd_cts_hta_package( m_test_package1 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_package( m_test_package1 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_wrong_status(
                                        textid                 = cx_cts_hta_wrong_status=>package_requires_deployment
                                        hot_status             = if_cts_hot_db_access=>co_hot_status_inactive
                                        name_of_obj_or_package = m_test_package1->hana_package_name
                                        cts_hta_component      = m_test_package1 ) ) ).

    CAST ltd_cts_hta_object( m_test_object2 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_object( m_test_object2 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_wrong_status(
                                        textid                 = cx_cts_hta_wrong_status=>object_requires_deployment
                                        hot_status             = if_cts_hot_db_access=>co_hot_status_to_be_deleted
                                        name_of_obj_or_package = m_test_object2->object_key-hana_object_name && '.' && m_test_object2->object_key-hana_object_suffix
                                        cts_hta_component      = m_test_object2 ) ) ).

    CAST ltd_cts_hta_full_package( m_test_full_package3 )->sync_state = ce_cts_hta_sync_state=>can_not_be_synchronized.
    CAST ltd_cts_hta_full_package( m_test_full_package3 )->reasons_not_syncable = VALUE #(
                                  ( NEW cx_cts_hta_name_conflict(
                                        textid                      = cx_cts_hta_name_conflict=>object_name_conflict
                                        name_of_obj_or_package_conf = m_test_package3->hana_package_name
                                        name_of_obj_or_package_hta  = m_test_package3->hana_package_name
                                        cts_hta_component           = m_test_package3 ) ) ).

    lr_sync_state = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 3 msg = 'Reasons for can_not_be_synchronized missing' ).
    LOOP AT lt_reasons INTO DATA(lr_reason).
      CASE lr_reason->if_t100_message~t100key.
        WHEN cx_cts_hta_wrong_status=>package_requires_deployment.
          cl_abap_unit_assert=>assert_equals( act = lr_reason->cts_hta_component exp = m_test_package1 ).
        WHEN cx_cts_hta_wrong_status=>object_requires_deployment.
          cl_abap_unit_assert=>assert_equals( act = lr_reason->cts_hta_component exp = m_test_object2 ).
        WHEN cx_cts_hta_name_conflict=>object_name_conflict.
          cl_abap_unit_assert=>assert_equals( act = lr_reason->cts_hta_component exp = m_test_full_package3 ).
        WHEN OTHERS.
          cl_abap_unit_assert=>fail( 'Found unexpected reason for can_not_be_synchronized' ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.