*"* use this source file for your ABAP unit test classes
CLASS ltd_cl_cts_hta_component DEFINITION FINAL FOR TESTING
  INHERITING FROM cl_cts_hta_component
  DURATION SHORT
  RISK LEVEL HARMLESS
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA:
      "! Expected value of parameter i_suppress_dialog
      m_exp_suppress_dialog TYPE abap_bool,
      "! Expected value of parameter i_trkorr
      m_exp_trkorr          TYPE trkorr,
      "! Expected value of parameter i_devclass
      m_exp_devclass        TYPE devclass.

    METHODS:
      constructor,
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~get_deploy_state REDEFINITION,
      if_cts_hta_component~get_sync_state REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      rs_corr_check REDEFINITION,
      rs_corr_insert REDEFINITION,
      execute_sync REDEFINITION,
      hta_pre_sync_check REDEFINITION.
    METHODS: read_hana_data REDEFINITION,
      read_hta_data REDEFINITION.

  PRIVATE SECTION.
    DATA:
      "! Counter to verify that methods are called in correct order:<br/>
      "! 1. rs_corr_check<br/>
      "! 2. rs_corr_insert<br/>
      "! 3. execute_sync<br/>
      m_sync_call_counter         TYPE i.
ENDCLASS.

CLASS ltd_cl_cts_hta_component IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    m_sync_call_counter = 0.
  ENDMETHOD.

  METHOD if_cts_hta_component~deploy.
    execute_deploy(
      EXPORTING
        i_force        = i_force
      IMPORTING
        e_deploy_status   = e_overall_deploy_status
        e_deploy_messages = e_deploy_messages
    ).
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

  METHOD hta_pre_sync_check.
    m_sync_call_counter = m_sync_call_counter + 1.
    cl_abap_unit_assert=>assert_equals( act = m_sync_call_counter exp = 3 msg = 'hta_pre_sync_check is expected to be the third call to abstract method in cl_cts_hta_component->synchronize' ).
  ENDMETHOD.

  METHOD rs_corr_check.
    m_sync_call_counter = m_sync_call_counter + 1.
    cl_abap_unit_assert=>assert_equals( act = m_sync_call_counter exp = 4 msg = 'rs_corr_check is expected to be the fourth call to abstract method in cl_cts_hta_component->synchronize' ).

    cl_abap_unit_assert=>assert_equals( act = i_suppress_dialog exp = m_exp_suppress_dialog ).
  ENDMETHOD.

  METHOD rs_corr_insert.
    m_sync_call_counter = m_sync_call_counter + 1.
    cl_abap_unit_assert=>assert_equals( act = m_sync_call_counter exp = 5 msg = 'rs_corr_insert is expected to be the fifth call to abstract method in cl_cts_hta_component->synchronize' ).

    cl_abap_unit_assert=>assert_equals( act = i_trkorr exp = m_exp_trkorr ).
    cl_abap_unit_assert=>assert_equals( act = i_devclass exp = m_exp_devclass ).
    cl_abap_unit_assert=>assert_equals( act = i_suppress_dialog exp = m_exp_suppress_dialog ).
  ENDMETHOD.

  METHOD execute_sync.
    m_sync_call_counter = m_sync_call_counter + 1.
    cl_abap_unit_assert=>assert_equals( act = m_sync_call_counter exp = 6 msg = 'execute_sync is expected to be the sixth call to abstract method in cl_cts_hta_component->synchronize' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~set_deploy_mode.
    cl_abap_unit_assert=>fail( 'Call to set_deploy_mode not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~set_translation_relevance.
    cl_abap_unit_assert=>fail( 'Call to set_translation_relevance not expected in testdouble' ).
  ENDMETHOD.

  METHOD read_hana_data.
    m_sync_call_counter = m_sync_call_counter + 1.
    cl_abap_unit_assert=>assert_equals( act = m_sync_call_counter exp = 2 msg = 'read_hana_data is is expected to be the second call to abstract method in cl_cts_hta_component->synchronize' ).
  ENDMETHOD.

  METHOD read_hta_data.
    m_sync_call_counter = m_sync_call_counter + 1.
    cl_abap_unit_assert=>assert_equals( act = m_sync_call_counter exp = 1 msg = 'read_hta_data is expected to be the first call to abstract method in cl_cts_hta_component->synchronize' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltd_deployer_spy DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES lif_hana_deployer.
    DATA:
      m_expected_packages    TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
      m_expected_objects     TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list,
      m_expected_abap_status TYPE cts_hot_abap_status VALUE cl_cts_hta_component=>co_active_version,
      m_severity_to_return   TYPE errortyp VALUE 'I'.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_deployer_spy IMPLEMENTATION.

  METHOD lif_hana_deployer~execute_deployment.
    cl_abap_unit_assert=>assert_equals( act = lines( i_packages ) exp = lines( m_expected_packages ) msg = 'Different number of packages expected' ).
    cl_abap_unit_assert=>assert_equals( act = lines( i_objects ) exp = lines( m_expected_objects ) msg = 'Different number of objects expected' ).
    cl_abap_unit_assert=>assert_equals( act = i_abap_status exp = m_expected_abap_status  msg = 'Wrong ABAP_STATUS was passed.' ).

    e_deploy_messages = VALUE if_cts_hta_types=>ty_deploy_messages(
        ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
    ).
    LOOP AT i_packages INTO DATA(lr_package).
      APPEND LINES OF VALUE if_cts_hta_types=>ty_deploy_messages(
        ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_package->abap_hana_package_id var2 = lr_package->hana_package_id var3 = '' var4 = '' langu = 'E' )
      ) TO e_deploy_messages.
    ENDLOOP.

    LOOP AT i_objects INTO DATA(lr_object).
      APPEND LINES OF VALUE if_cts_hta_types=>ty_deploy_messages(
        ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_object->transport_object_name var2 = lr_object->hana_package_id var3 = lr_object->hana_object_name var4 = lr_object->hana_object_suffix langu = 'E' )
      ) TO e_deploy_messages.
    ENDLOOP.

    e_max_severity = m_severity_to_return.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_db_access_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES:
      lif_db_access.

    DATA:
      m_expected_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
      m_expected_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.
ENDCLASS.

CLASS ltd_db_access_spy IMPLEMENTATION.
  METHOD lif_db_access~prepare_force_deploy_of_obj.
    IF m_expected_objects IS INITIAL.
      cl_abap_unit_assert=>fail( 'call to prepare_force_deploy_of_obj not expected' ).
    ENDIF.
    cl_abap_unit_assert=>assert_table_contains( line = i_object table = m_expected_objects msg = |Object was not expected: { i_object->hana_object_name }.{ i_object->hana_object_suffix } ({ i_object->hana_package_id })| ).
  ENDMETHOD.

  METHOD lif_db_access~prepare_force_deploy_of_pkg.
    IF m_expected_packages IS INITIAL.
      cl_abap_unit_assert=>fail( 'call to prepare_force_deploy_of_pkg not expected' ).
    ENDIF.
    cl_abap_unit_assert=>assert_table_contains( line = i_package table = m_expected_packages msg = |Package was not expected: { i_package->hana_package_id }| ).
  ENDMETHOD.
ENDCLASS.

CLASS ltd_external_calls_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hot_ext_call_internal PARTIALLY IMPLEMENTED.

    DATA:
      "! Specifies whether call to check_transport_tools_for_sync is expected or not, default it is not expected
      m_call_check_tools_expected    TYPE abap_bool,
      "! Specifies whether call to rs_corr_check is expected or not, default it is not expected
      m_call_rs_corr_check_expected  TYPE abap_bool,
      "! Specifies whether call to rs_corr_insert is expected or not, default it is not expected
      m_call_rs_corr_insert_expected TYPE abap_bool,
      "! Specifies whether call to determine_masterlang_for_tadir is expected or not, default it is not expected
      m_call_determine_mastrlang_exp TYPE abap_bool,
      "! Expected value for suppress dialog for all methods
      m_exp_value_suppress_dialog    TYPE abap_bool.
ENDCLASS.

CLASS ltd_external_calls_spy IMPLEMENTATION.
  METHOD if_cts_hot_ext_call_internal~check_transport_tools_for_sync.
    IF m_call_check_tools_expected = abap_false.
      cl_abap_unit_assert=>fail( 'call to check_transport_tools_for_sync not expected' ).
    ENDIF.
    m_call_check_tools_expected = abap_false. "call only expected once per test
  ENDMETHOD.

  METHOD if_cts_hot_ext_call_internal~rs_corr_check.
    IF m_call_rs_corr_check_expected = abap_false.
      cl_abap_unit_assert=>fail( 'call to rs_corr_check not expected' ).
    ENDIF.
    m_call_rs_corr_check_expected = abap_false. "call only expected once per test
  ENDMETHOD.

  METHOD if_cts_hot_ext_call_internal~rs_corr_insert.
    IF m_call_rs_corr_insert_expected = abap_false.
      cl_abap_unit_assert=>fail( 'call to rs_corr_insert not expected' ).
    ENDIF.
    m_call_rs_corr_insert_expected = abap_false. "call only expected once per test
  ENDMETHOD.

  METHOD if_cts_hot_ext_call_internal~determine_masterlang_for_tadir.
    IF m_call_determine_mastrlang_exp = abap_false.
      cl_abap_unit_assert=>fail( 'call to determine_masterlang_for_tadir not expected' ).
    ENDIF.
    m_call_determine_mastrlang_exp = abap_false. "call only expected once per test
  ENDMETHOD.

ENDCLASS.

CLASS ltd_hot_hana_connector_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hot_hana_conn_internal.

    DATA:
      m_expected_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
      m_expected_objects  TYPE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list.
ENDCLASS.

CLASS ltd_hot_hana_connector_spy IMPLEMENTATION.
  METHOD if_cts_hot_hana_conn_internal~read_objects_from_hana_to_hot.
    IF m_expected_objects IS INITIAL.
      cl_abap_unit_assert=>fail( 'call to read_objects_from_hana_to_hot not expected' ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = lines( i_objects ) exp = lines( m_expected_objects ) msg = 'Different number of objects expected' ).

    LOOP AT m_expected_objects INTO DATA(lr_object).
      cl_abap_unit_assert=>assert_table_contains( line = lr_object table = m_expected_objects msg = |Object was not expected: { lr_object->hana_object_name }.{ lr_object->hana_object_suffix } ({ lr_object->hana_package_id })| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_cts_hot_hana_conn_internal~sync_packages_from_hana_to_hot.
    IF m_expected_packages IS INITIAL.
      cl_abap_unit_assert=>fail( 'call to sync_packages_from_hana_to_hot not expected' ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = lines( i_hana_packages_list ) exp = lines( m_expected_packages ) msg = 'Different number of objects expected' ).

    LOOP AT i_hana_packages_list INTO DATA(lr_package).
      cl_abap_unit_assert=>assert_table_contains( line = lr_package table = m_expected_packages msg = |Package was not expected: { lr_package->hana_package_id }| ).
    ENDLOOP.
  ENDMETHOD.
  METHOD if_cts_hot_hana_conn_internal~read_package_data_from_hana.
    cl_abap_unit_assert=>fail( 'call to read_package_data_from_hana not expected' ).
  ENDMETHOD.
  METHOD if_cts_hot_hana_conn_internal~get_objects_of_pkg_from_hana.
    cl_abap_unit_assert=>fail( 'call to get_objects_of_pkg_from_hana not expected' ).
  ENDMETHOD.

  METHOD if_cts_hot_hana_conn_internal~find_active_objects_in_hana.
    cl_abap_unit_assert=>fail( 'call to find_active_objects_in_hana not expected' ).
  ENDMETHOD.

  METHOD if_cts_hot_hana_conn_internal~list_hana_packages.
    cl_abap_unit_assert=>fail( 'call to list_hana_packages not expected' ).
  ENDMETHOD.

  METHOD if_cts_hot_hana_conn_internal~read_object_data_from_hana.
    cl_abap_unit_assert=>fail( 'call to read_object_data_from_hana not expected' ).
  ENDMETHOD.

ENDCLASS.


"! Test class testing with mock data
CLASS ltcl_cl_cts_hta_component DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      "! Tests deployment without anything to deploy.
      deploy_without_data FOR TESTING RAISING cx_static_check,
      "! Tests deployment of a single package (if_cts_hta_package)
      deploy_one_package FOR TESTING RAISING cx_static_check,
      "! Tests deployment of a single object (if_cts_hta_object)
      deploy_one_object FOR TESTING RAISING cx_static_check,
      "! Tests deployment of 2 packages (if_cts_hta_package) and 2 objects (if_cts_hta_object)
      deploy_packages_and_objects FOR TESTING RAISING cx_static_check,
      "! Tests the severity mapping in deployment
      deploy_severity_mapping FOR TESTING RAISING cx_static_check,
      "! Tests the abap_Status mapping in deployment
      deploy_abap_status_mapping FOR TESTING RAISING cx_static_check,
      "! Tests force deployment for 2 packs and 2 objects
      deploy_force_2_packs_2_objs FOR TESTING RAISING cx_static_check,

      "! Test synchronize whether it calls abstract methods with correct parameters when being called without parameters.
      sync_wo_parameters FOR TESTING RAISING cx_static_check,
      "! Test synchronize whether it calls abstract methods with correct parameters when being called with i_suppress_dialog = abap_true.
      sync_suppress_dialog_true FOR TESTING RAISING cx_static_check,
      "! Test synchronize whether it calls abstract methods with correct parameters when being called with i_suppress_dialog = abap_false.
      sync_suppress_dialog_false FOR TESTING RAISING cx_static_check,
      "! Test synchronize whether it calls abstract methods with correct parameters when being called with i_trkorr only.
      sync_trkorr FOR TESTING RAISING cx_static_check,
      "! Test synchronize whether it calls abstract methods with correct parameters when being called with i_devclass only.
      sync_devclass FOR TESTING RAISING cx_static_check,
      "! Test synchronize whether it calls abstract methods with correct parameters when being called with all parameters set.
      sync_with_all_params FOR TESTING RAISING cx_static_check.

    DATA:
      m_cut               TYPE REF TO cl_cts_hta_component,
      m_deployer_spy      TYPE REF TO ltd_deployer_spy,
      m_db_access_spy     TYPE REF TO ltd_db_access_spy,
      m_external_call_spy TYPE REF TO ltd_external_calls_spy,
      m_hot_hana_conn_spy TYPE REF TO ltd_hot_hana_connector_spy.
ENDCLASS.

CLASS cl_cts_hta_component DEFINITION LOCAL FRIENDS ltcl_cl_cts_hta_component.
CLASS ltcl_cl_cts_hta_component IMPLEMENTATION.
  METHOD setup.
    m_cut = NEW ltd_cl_cts_hta_component( ).

    m_deployer_spy = NEW ltd_deployer_spy( ).
    m_cut->m_hana_deployer = m_deployer_spy.

    m_db_access_spy = NEW ltd_db_access_spy( ).
    m_cut->m_db_access = m_db_access_spy.

    m_external_call_spy = NEW ltd_external_calls_spy( ).
    m_cut->m_external_calls = m_external_call_spy.

    m_hot_hana_conn_spy = NEW ltd_hot_hana_connector_spy( ).
    m_cut->gr_hot_hana_connector = m_hot_hana_conn_spy.
  ENDMETHOD.

  METHOD deploy_without_data.
    m_cut->if_cts_hta_component~deploy(
      IMPORTING
        e_overall_deploy_status = DATA(lv_deploy_status)
        e_deploy_messages       = DATA(lt_deploy_messages)
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_deploy_messages ) exp = 1 ).

    DATA(lt_expected_deploy_messages) = VALUE if_cts_hta_types=>ty_deploy_messages(
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
    ).
    cl_abap_unit_assert=>assert_equals( act = lt_deploy_messages exp = lt_expected_deploy_messages ).
  ENDMETHOD.

  METHOD deploy_one_package.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'TEST.PACK.SUBPACK1' ).

    "set package to spy
    m_deployer_spy->m_expected_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( CAST cl_cts_hta_component( lr_package )->m_hot_package ) ).

    m_cut->execute_deploy(
      EXPORTING
        i_hta_packages = VALUE if_cts_hta_types=>ty_cts_hta_packages( ( lr_package ) )
      IMPORTING
        e_deploy_status = DATA(lv_deploy_status)
        e_deploy_messages = DATA(lt_deploy_messages)
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_deploy_messages ) exp = 2 ).

    DATA(lt_expected_deploy_messages) = VALUE if_cts_hta_types=>ty_deploy_messages(
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_package->if_cts_hta_component~transport_object_name var2 = lr_package->hana_package_name var3 = '' var4 = '' langu = 'E' )
    ).
    cl_abap_unit_assert=>assert_equals( act = lt_deploy_messages exp = lt_expected_deploy_messages ).
  ENDMETHOD.

  METHOD deploy_one_object.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'TEST.PACK.SUBPACK1' ).
    DATA(lr_object) = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package = lr_package
                                                                        i_hana_object_name = 'OBJECT_NAME'
                                                                        i_hana_object_suffix = 'suffix' ).
    "set object to spy
    m_deployer_spy->m_expected_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( CAST cl_cts_hta_object( lr_object )->m_hot_object ) ).

    m_cut->execute_deploy(
      EXPORTING
        i_hta_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( lr_object ) )
      IMPORTING
        e_deploy_status = DATA(lv_deploy_status)
        e_deploy_messages = DATA(lt_deploy_messages)
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_deploy_messages ) exp = 2 ).

    DATA(lt_expected_deploy_messages) = VALUE if_cts_hta_types=>ty_deploy_messages(
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_object->if_cts_hta_component~transport_object_name var2 = lr_object->object_key-hana_package_name var3 =
                                                                                                                                                lr_object->object_key-hana_object_name var4 = lr_object->object_key-hana_object_suffix langu = 'E' )
    ).
    cl_abap_unit_assert=>assert_equals( act = lt_deploy_messages exp = lt_expected_deploy_messages ).
  ENDMETHOD.

  METHOD deploy_packages_and_objects.
    DATA(lr_package1) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'TEST.PACK.SUBPACK1' ).
    DATA(lr_package2) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'TEST.PACK.SUBPACK2' ).
    DATA(lr_object1) = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package = lr_package1
                                                                        i_hana_object_name = 'OBJECT1_NAME'
                                                                        i_hana_object_suffix = 'suffix' ).
    DATA(lr_object2) = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package = lr_package1
                                                                        i_hana_object_name = 'OBJECT2_NAME'
                                                                        i_hana_object_suffix = 'suffix' ).
    "set packages and objects to spy
    m_deployer_spy->m_expected_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( CAST cl_cts_hta_component( lr_package1 )->m_hot_package )
                                                                                                ( CAST cl_cts_hta_component( lr_package2 )->m_hot_package ) ).
    m_deployer_spy->m_expected_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( CAST cl_cts_hta_object( lr_object1 )->m_hot_object )
                                                                                                ( CAST cl_cts_hta_object( lr_object2 )->m_hot_object ) ).

    m_cut->execute_deploy(
      EXPORTING
        i_hta_packages = VALUE if_cts_hta_types=>ty_cts_hta_packages( ( lr_package1 ) ( lr_package2 ) )
        i_hta_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( lr_object1 ) ( lr_object2 ) )
      IMPORTING
        e_deploy_status = DATA(lv_deploy_status)
        e_deploy_messages = DATA(lt_deploy_messages)
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_deploy_messages ) exp = 5 ).

    DATA(lt_expected_deploy_messages) = VALUE if_cts_hta_types=>ty_deploy_messages(
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_package1->if_cts_hta_component~transport_object_name var2 = lr_package1->hana_package_name var3 = '' var4 = '' langu = 'E' )
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_package2->if_cts_hta_component~transport_object_name var2 = lr_package2->hana_package_name var3 = '' var4 = '' langu = 'E' )
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_object1->if_cts_hta_component~transport_object_name var2 = lr_object1->object_key-hana_package_name var3 =
                                                                                                                                               lr_object1->object_key-hana_object_name var4 = lr_object1->object_key-hana_object_suffix langu = 'E' )
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_object2->if_cts_hta_component~transport_object_name var2 = lr_object2->object_key-hana_package_name var3 =
                                                                                                                                               lr_object2->object_key-hana_object_name var4 = lr_object2->object_key-hana_object_suffix langu = 'E' )
    ).
    cl_abap_unit_assert=>assert_equals( act = lt_deploy_messages exp = lt_expected_deploy_messages ).
  ENDMETHOD.

  METHOD deploy_severity_mapping.
    "Input space expect I
    m_deployer_spy->m_severity_to_return = space.
    m_cut->execute_deploy(
      IMPORTING
        e_deploy_status = DATA(lv_deploy_status)
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).

    "Input I expect I
    m_deployer_spy->m_severity_to_return = 'I'.
    m_cut->execute_deploy(
      IMPORTING
        e_deploy_status = lv_deploy_status
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).

    "Input W expect W
    m_deployer_spy->m_severity_to_return = 'W'.
    m_cut->execute_deploy(
      IMPORTING
        e_deploy_status = lv_deploy_status
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'W' ).

    "Input E expect E
    m_deployer_spy->m_severity_to_return = 'E'.
    m_cut->execute_deploy(
      IMPORTING
        e_deploy_status = lv_deploy_status
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'E' ).

    "Input A expect A
    m_deployer_spy->m_severity_to_return = 'A'.
    m_cut->execute_deploy(
      IMPORTING
        e_deploy_status = lv_deploy_status
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'A' ).
  ENDMETHOD.

  METHOD deploy_abap_status_mapping.
    "Expect A for no input
    m_deployer_spy->m_expected_abap_status = cl_cts_hta_component=>co_active_version.
    m_cut->execute_deploy( ).

    "Expect I for input 'I'
    m_cut->m_abap_status = cl_cts_hta_component=>co_inactive_version.
    m_deployer_spy->m_expected_abap_status = cl_cts_hta_component=>co_inactive_version.
    m_cut->execute_deploy( ).
  ENDMETHOD.

  METHOD sync_suppress_dialog_false.
    DATA lv_suppress_dialog TYPE abap_bool VALUE abap_false.

    "configuration of spy for external calls
    m_external_call_spy->m_call_check_tools_expected = abap_true.
    m_external_call_spy->m_exp_value_suppress_dialog = lv_suppress_dialog.

    "set expected value for calls to rs_corr_check and rs_corr_insert
    CAST ltd_cl_cts_hta_component( m_cut )->m_exp_suppress_dialog = lv_suppress_dialog.

    m_cut->if_cts_hta_component~synchronize( i_suppress_dialog = lv_suppress_dialog ).

    "verification happens in ltd_cl_cts_hta_component in redefined protected methods (rs_corr_check, rs_corr_insert and execute_sync) and in spy's
  ENDMETHOD.

  METHOD sync_suppress_dialog_true.
    DATA lv_suppress_dialog TYPE abap_bool VALUE abap_true.

    "configuration of spy for external calls
    m_external_call_spy->m_call_check_tools_expected = abap_true.
    m_external_call_spy->m_exp_value_suppress_dialog = lv_suppress_dialog.

    "set expected value for calls to rs_corr_check and rs_corr_insert
    CAST ltd_cl_cts_hta_component( m_cut )->m_exp_suppress_dialog = lv_suppress_dialog.

    m_cut->if_cts_hta_component~synchronize( i_suppress_dialog = lv_suppress_dialog ).

    "verification happens in ltd_cl_cts_hta_component in redefined protected methods (rs_corr_check, rs_corr_insert and execute_sync) and in spy's
  ENDMETHOD.

  METHOD sync_wo_parameters.
    m_external_call_spy->m_call_check_tools_expected = abap_true.

    m_cut->if_cts_hta_component~synchronize( ).

    "verification happens in ltd_cl_cts_hta_component in redefined protected methods (rs_corr_check, rs_corr_insert and execute_sync)
  ENDMETHOD.

  METHOD sync_devclass.
    DATA lv_devclass TYPE devclass VALUE 'DEV_CLASS_TEST'.

    "configuration of spy for external calls
    m_external_call_spy->m_call_check_tools_expected = abap_true.

    "set expected value for calls to rs_corr_check and rs_corr_insert
    CAST ltd_cl_cts_hta_component( m_cut )->m_exp_devclass = lv_devclass.

    m_cut->if_cts_hta_component~synchronize( i_devclass = lv_devclass ).

    "verification happens in ltd_cl_cts_hta_component in redefined protected methods (rs_corr_check, rs_corr_insert and execute_sync) and in spy's
  ENDMETHOD.

  METHOD sync_trkorr.
    DATA lv_trkorr TYPE trkorr VALUE 'ABCK123456'.

    "configuration of spy for external calls
    m_external_call_spy->m_call_check_tools_expected = abap_true.

    "set expected value for calls to rs_corr_check and rs_corr_insert
    CAST ltd_cl_cts_hta_component( m_cut )->m_exp_trkorr = lv_trkorr.

    m_cut->if_cts_hta_component~synchronize( i_trkorr = lv_trkorr ).

    "verification happens in ltd_cl_cts_hta_component in redefined protected methods (rs_corr_check, rs_corr_insert and execute_sync) and in spy's
  ENDMETHOD.

  METHOD sync_with_all_params.
    DATA lv_trkorr TYPE trkorr VALUE 'ABCK123456'.
    DATA lv_devclass TYPE devclass VALUE 'DEV_CLASS_TEST'.
    DATA lv_suppress_dialog TYPE abap_bool VALUE abap_true.

    "configuration of spy for external calls
    m_external_call_spy->m_call_check_tools_expected = abap_true.
    m_external_call_spy->m_exp_value_suppress_dialog = lv_suppress_dialog.

    "set expected values for calls to rs_corr_check and rs_corr_insert
    CAST ltd_cl_cts_hta_component( m_cut )->m_exp_trkorr = lv_trkorr.
    CAST ltd_cl_cts_hta_component( m_cut )->m_exp_devclass = lv_devclass.
    CAST ltd_cl_cts_hta_component( m_cut )->m_exp_suppress_dialog = lv_suppress_dialog.

    m_cut->if_cts_hta_component~synchronize( i_trkorr = lv_trkorr i_devclass = lv_devclass i_suppress_dialog = lv_suppress_dialog ).

    "verification happens in ltd_cl_cts_hta_component in redefined protected methods (rs_corr_check, rs_corr_insert and execute_sync) and in spy's
  ENDMETHOD.

  METHOD deploy_force_2_packs_2_objs.
    DATA(lr_package1) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'TEST.PACK.SUBPACK1' ).
    DATA(lr_package2) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'TEST.PACK.SUBPACK2' ).
    DATA(lr_object1) = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package = lr_package1
                                                                        i_hana_object_name = 'OBJECT1_NAME'
                                                                        i_hana_object_suffix = 'suffix' ).
    DATA(lr_object2) = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package = lr_package1
                                                                        i_hana_object_name = 'OBJECT2_NAME'
                                                                        i_hana_object_suffix = 'suffix' ).
    "set packages and objects to spys
    m_deployer_spy->m_expected_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( CAST cl_cts_hta_component( lr_package1 )->m_hot_package )
                                                                                                ( CAST cl_cts_hta_component( lr_package2 )->m_hot_package ) ).
    m_deployer_spy->m_expected_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( CAST cl_cts_hta_object( lr_object1 )->m_hot_object )
                                                                                                ( CAST cl_cts_hta_object( lr_object2 )->m_hot_object ) ).
    m_db_access_spy->m_expected_packages = VALUE cl_cts_hot_package=>ty_cl_cts_hot_package_list( ( CAST cl_cts_hta_component( lr_package1 )->m_hot_package )
                                                                                                ( CAST cl_cts_hta_component( lr_package2 )->m_hot_package ) ).
    m_db_access_spy->m_expected_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list( ( CAST cl_cts_hta_object( lr_object1 )->m_hot_object )
                                                                                                ( CAST cl_cts_hta_object( lr_object2 )->m_hot_object ) ).

    m_cut->execute_deploy(
      EXPORTING
        i_hta_packages = VALUE if_cts_hta_types=>ty_cts_hta_packages( ( lr_package1 ) ( lr_package2 ) )
        i_hta_objects = VALUE if_cts_hta_types=>ty_cts_hta_objects( ( lr_object1 ) ( lr_object2 ) )
        i_force = abap_true
      IMPORTING
        e_deploy_status = DATA(lv_deploy_status)
        e_deploy_messages = DATA(lt_deploy_messages)
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_deploy_messages ) exp = 5 ).

    DATA(lt_expected_deploy_messages) = VALUE if_cts_hta_types=>ty_deploy_messages(
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_package1->if_cts_hta_component~transport_object_name var2 = lr_package1->hana_package_name var3 = '' var4 = '' langu = 'E' )
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_package2->if_cts_hta_component~transport_object_name var2 = lr_package2->hana_package_name var3 = '' var4 = '' langu = 'E' )
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_object1->if_cts_hta_component~transport_object_name var2 = lr_object1->object_key-hana_package_name var3 =
                                                                                                                                               lr_object1->object_key-hana_object_name var4 = lr_object1->object_key-hana_object_suffix langu = 'E' )
          ( ag = 'SCTS_HOT' level = '1' msgnr = '531' newobj = ' ' severity = ' ' var1 = lr_object2->if_cts_hta_component~transport_object_name var2 = lr_object2->object_key-hana_package_name var3 =
                                                                                                                                               lr_object2->object_key-hana_object_name var4 = lr_object2->object_key-hana_object_suffix langu = 'E' )
    ).
    cl_abap_unit_assert=>assert_equals( act = lt_deploy_messages exp = lt_expected_deploy_messages ).
  ENDMETHOD.
ENDCLASS.

"! Test class for integration testing. Really deploys to HANA.
CLASS ltcl_cl_cts_hta_component_int DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      g_is_hana_supported TYPE abap_bool.

    CLASS-METHODS:
      "! to check whether we are on HANA or not (currently we also support non hana, but expect different logs.
      class_setup.

    METHODS:
      execute_deploy_without_data FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_cl_cts_hta_component_int IMPLEMENTATION.
  METHOD class_setup.
    TRY.
        cl_nhi_api=>create_instance( ).
        g_is_hana_supported = abap_true.
      CATCH cx_nhi_not_supported.
        g_is_hana_supported = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD execute_deploy_without_data.
    DATA: lt_expected_deploy_messages TYPE if_cts_hta_types=>ty_deploy_messages.

    DATA(lr_hta_comp) = NEW ltd_cl_cts_hta_component( ).
    lr_hta_comp->if_cts_hta_component~deploy(
      IMPORTING
        e_overall_deploy_status = DATA(lv_deploy_status)
        e_deploy_messages       = DATA(lt_deploy_messages)
    ).

    cl_abap_unit_assert=>assert_equals( act = lv_deploy_status exp = 'I' ).
      cl_abap_unit_assert=>assert_equals( act = lines( lt_deploy_messages ) exp = 6 ).

    "UGly, but get timestamps from message variables...
    DATA(lv_501_var1) = lt_deploy_messages[ msgnr = '501' ]-var1.
    DATA(lv_503_var1) = lt_deploy_messages[ msgnr = '503' ]-var1.

    lt_expected_deploy_messages = VALUE if_cts_hta_types=>ty_deploy_messages(
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = 'X' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
          ( ag = 'SCTS_HOT' level = '1' msgnr = '501' newobj = ' ' severity = ' ' var1 = lv_501_var1 var2 = '' var3 = '' var4 = '' langu = 'E' ) "start deployment at xyz
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = 'X' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
          ( ag = 'SCTS_HOT' level = '1' msgnr = '503' newobj = ' ' severity = ' ' var1 = lv_503_var1 var2 = '' var3 = '' var4 = '' langu = 'E' ) "end deployment at xyz
          ( ag = 'SCTS_HOT' level = '1' msgnr = '507' newobj = ' ' severity = ' ' var1 = '' var2 = '' var3 = '' var4 = '' langu = 'E' ) "empty line
    ).

    cl_abap_unit_assert=>assert_equals( act = lt_deploy_messages exp = lt_expected_deploy_messages ).
  ENDMETHOD.
ENDCLASS.