*"* use this source file for your ABAP unit test classes
CLASS ltd_tadir_access DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES lif_tadir_access.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_tadir_access IMPLEMENTATION.
  METHOD lif_tadir_access~read_hota_objnames_for_devclas.
    "do not change fixed string values, used in ltcl_cts_hta_package_with_db
    LOOP AT i_devclasses INTO DATA(lv_devclass).
      IF lv_devclass = 'HTA_DEVCLASS1'. "simple case, 1 devclass has 1 hota object
        APPEND 'TMP.HTA.PACK1' TO r_result.
      ENDIF.
      IF lv_devclass = 'HTA_DEVCLASS2'. "complex case, 1 devclass with 2 hota_objects
        APPEND 'TMP.HTA.PACK2' TO r_result.
        APPEND 'TMP.HTA.PACK3.SUB' TO r_result.
        APPEND 'TMP.HTA.SUB.PACK4' TO r_result.
      ENDIF.
      IF lv_devclass = 'HTA_DEVCLASS3'. "devclass without HOTA object
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_hot_hana_connector_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hot_hana_conn_internal PARTIALLY IMPLEMENTED.

    DATA:
      m_expected_packages TYPE cl_cts_hot_package=>ty_cl_cts_hot_package_list,
      m_throw_exception   TYPE REF TO cx_hana_object_transport.
ENDCLASS.

CLASS ltd_hot_hana_connector_spy IMPLEMENTATION.
  METHOD if_cts_hot_hana_conn_internal~read_objects_from_hana_to_hot.
    cl_abap_unit_assert=>fail( 'call to read_objects_from_hana_to_hot not expected' ).
  ENDMETHOD.

  METHOD if_cts_hot_hana_conn_internal~sync_packages_from_hana_to_hot.
    IF m_throw_exception IS BOUND.
      RAISE EXCEPTION m_throw_exception.
    ENDIF.
    IF m_expected_packages IS INITIAL.
      cl_abap_unit_assert=>fail( 'call to sync_packages_from_hana_to_hot not expected' ).
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = lines( i_hana_packages_list ) exp = lines( m_expected_packages ) msg = 'Different number of packages expected' ).

    LOOP AT i_hana_packages_list INTO DATA(lr_package).
      cl_abap_unit_assert=>assert_table_contains( line = lr_package table = m_expected_packages msg = |Package was not expected: { lr_package->hana_package_id }| ).
    ENDLOOP.
  ENDMETHOD.
  METHOD if_cts_hot_hana_conn_internal~read_package_data_from_hana.
    cl_abap_unit_assert=>fail( 'call to read_package_data_from_hana not expected' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltd_hot_hana_connector_respond DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hot_hana_conn_internal PARTIALLY IMPLEMENTED.

    DATA:
          gv_respond_with_data TYPE abap_bool VALUE abap_true.
ENDCLASS.

CLASS ltd_hot_hana_connector_respond IMPLEMENTATION.
  METHOD if_cts_hot_hana_conn_internal~list_hana_packages.
    "do not change fixed string values, used in ltcl_cts_hta_package_with_db
    IF i_hana_package_name = 'tmp.HANA.Pack1'. "Test method create_from_hana_key_han_list1
      APPEND 'tmp.HANA.Pack1' TO r_result.
    ELSEIF i_hana_package_name = 'tmp.HANA.Pack*'. "Test method create_from_hana_key_han_list3
      APPEND 'tmp.HANA.Pack1' TO r_result.
      APPEND 'tmp.HANA.Pack2' TO r_result.
      APPEND 'tmp.HANA.Pack3.sub' TO r_result.
    ELSEIF i_hana_package_name = '*Pack1*'. "Test method create_from_hana_key_hta_han2
      APPEND 'tmp.HANA.Pack1' TO r_result.
    ELSEIF i_hana_package_name = 'tmp.HTA.Pack*'. "Test method create_from_hana_key_hta_han3
      APPEND 'tmp.HTA.Pack2' TO r_result.
      APPEND 'tmp.HTA.Pack3.sub' TO r_result.
    ENDIF.
  ENDMETHOD.

  METHOD if_cts_hot_hana_conn_internal~read_package_data_from_hana.
    IF gv_respond_with_data = abap_true
       AND i_hana_package_id = '123tmp456.hta.pack'. "only return data if correct package is requested
      "do not change fixed string values, used in ltcl_cts_hta_package_with_db
      r_package_data-abap_hana_package_id = '123TMP456.HTA.PACK'.
      r_package_data-hana_package_id = '123tmp456.hta.pack'.
      r_package_data-hana_pack_delivery_unit = 'DU_NAME'.
      r_package_data-hana_pack_du_vendor = 'DU_VENDOR'.
      r_package_data-hana_pack_description = 'Some Description'.
      r_package_data-hana_pack_hints_for_transl = 'Hint for transl'.
      r_package_data-hana_pack_is_structural = '0'.
      r_package_data-hana_pack_orig_lang = 'en_US'.
      r_package_data-hana_pack_responsible = 'ich'.
      r_package_data-hana_pack_src_system = 'SRC'.
      r_package_data-hana_pack_src_tenant = ''. "usually empty, therefore also taking space here
      r_package_data-hana_pack_text_collection = 'text collection'.
      r_package_data-hana_pack_text_status = 'text status'.
      r_package_data-hana_pack_text_term_domain = 'terminology domain'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cts_hta_package DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Tests creation of a package with name 'this.is.my.Package'
      create_from_hana_key FOR TESTING RAISING cx_static_check,
      "! Tests creation of package objects with package name containing double quotes, dash and camel case
      create_from_hana_key_complex FOR TESTING RAISING cx_static_check,
      "! Testing creation of packages with leading / ending spaces. They are to be ignored/cut off
      create_from_hana_key_spaces FOR TESTING RAISING cx_static_check,
      "! Testing creation of package object for a package name with length of 40 chars ( no hashing )
      create_from_hana_key_40_chars FOR TESTING RAISING cx_static_check,
      "! Testing creation of package object for a package name with more than 40 chars ( with hashing )
      create_from_hana_key_41_chars FOR TESTING RAISING cx_static_check,
      "! Testing 3 cases for long names (with hashing), 'normal' package name, package name with double quotes and camel case
      create_from_hana_key_long_name FOR TESTING RAISING cx_static_check,
      "! Testing that if HANA package name only differs in case they create same abap_hana_package_ids
      create_from_hana_key_conflict FOR TESTING RAISING cx_static_check,

      "! Testing execute_sync without error
      execute_sync FOR TESTING RAISING cx_static_check,
      "! Testing execute_sync with exception
      execute_sync_with_excpetion FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS cl_cts_hta_package DEFINITION LOCAL FRIENDS ltcl_cts_hta_package.
CLASS ltcl_cts_hta_package IMPLEMENTATION.

  METHOD create_from_hana_key.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( 'this.is.my.Package' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = 'THIS.IS.MY.PACKAGE' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = 'this.is.my.Package' ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_hta_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_hana_key_40_chars.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `test.maximum.length.package.without.hash` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( lr_hta_package->hana_package_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( lr_hta_package->if_cts_hta_component~transport_object_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = `test.maximum.length.package.without.hash` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `TEST.MAXIMUM.LENGTH.PACKAGE.WITHOUT.HASH` act = lr_hta_package->if_cts_hta_component~transport_object_name ).
  ENDMETHOD.

  METHOD create_from_hana_key_41_chars.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `test.41.chars.length.package.with.hashing` ).

    cl_abap_unit_assert=>assert_equals( exp = 41 act = strlen( lr_hta_package->hana_package_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( lr_hta_package->if_cts_hta_component~transport_object_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = `test.41.chars.length.package.with.hashing` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `TEST.41;F14QH8278T3577BCA7H915CBLAU85AN8` act = lr_hta_package->if_cts_hta_component~transport_object_name ).
  ENDMETHOD.

  METHOD create_from_hana_key_complex.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `sap.hana-app.CuAn.ANA_HRF.".settings"` ).

    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.CuAn.ANA_HRF.".settings"` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HANA-APP.CUAN.ANA_HRF.".SETTINGS"` act = lr_hta_package->if_cts_hta_component~transport_object_name ).
  ENDMETHOD.

  METHOD create_from_hana_key_long_name.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package.

    "Test 1: 'normal' package name
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( lr_hta_package->if_cts_hta_component~transport_object_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV` act = lr_hta_package->if_cts_hta_component~transport_object_name ).

    "Test 2: package name with double quotes
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `sap.hana-app.cuan.cpred.hrf.ANA_HRF.".settings"` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( lr_hta_package->if_cts_hta_component~transport_object_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.cuan.cpred.hrf.ANA_HRF.".settings"` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;G1IKOS8LD2BN5RC8BT2MKSU8AN043I5G` act = lr_hta_package->if_cts_hta_component~transport_object_name ).

    "Test 3: package name with camel case
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `sap.hana-app.mo.public.logic.HandleEmailSending` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( lr_hta_package->if_cts_hta_component~transport_object_name ) ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.mo.public.logic.HandleEmailSending` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;SM04LG9F98117NJCPJ3805OL94VUHTTK` act = lr_hta_package->if_cts_hta_component~transport_object_name ).
  ENDMETHOD.

  METHOD create_from_hana_key_spaces.
    DATA: lr_hta_package TYPE REF TO if_cts_hta_package.

    " 1 space in beginning
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = ` hana_pack` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = lr_hta_package->if_cts_hta_component~transport_object_name ).

    " 3 spaces in beginning
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `   hana_pack` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = lr_hta_package->if_cts_hta_component~transport_object_name ).

    " 1 space at the end
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `hana_pack ` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = lr_hta_package->if_cts_hta_component~transport_object_name ).

    " 3 spaces at the end
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `hana_pack   ` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = lr_hta_package->if_cts_hta_component~transport_object_name ).
  ENDMETHOD.

  METHOD create_from_hana_key_conflict.
    DATA: lr_hta_package1 TYPE REF TO if_cts_hta_package,
          lr_hta_package2 TYPE REF TO if_cts_hta_package.

    "Test 1:  package id < 40 chars
    lr_hta_package1 = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'sap.com.package' ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.com.package` act = lr_hta_package1->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.COM.PACKAGE` act = lr_hta_package1->if_cts_hta_component~transport_object_name ).

    lr_hta_package2 = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'Sap.Com.pacKage' ).
    cl_abap_unit_assert=>assert_equals( exp = `Sap.Com.pacKage` act = lr_hta_package2->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.COM.PACKAGE` act = lr_hta_package2->if_cts_hta_component~transport_object_name ).

    cl_abap_unit_assert=>assert_equals( exp = lr_hta_package1->if_cts_hta_component~transport_object_name act = lr_hta_package2->if_cts_hta_component~transport_object_name ).

    "Test 2:  package id > 40 chars
    lr_hta_package1 = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` act = lr_hta_package1->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV` act = lr_hta_package1->if_cts_hta_component~transport_object_name ).

    lr_hta_package2 = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = `sap.HANA-APP.cuan.cPred.demo.insurance.DataFoundation.internal` ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.HANA-APP.cuan.cPred.demo.insurance.DataFoundation.internal` act = lr_hta_package2->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV` act = lr_hta_package2->if_cts_hta_component~transport_object_name ).

    cl_abap_unit_assert=>assert_equals( exp = lr_hta_package1->if_cts_hta_component~transport_object_name act = lr_hta_package2->if_cts_hta_component~transport_object_name ).
  ENDMETHOD.

  METHOD execute_sync.
    DATA: lr_package                TYPE REF TO if_cts_hta_package,
          lr_hot_hana_connector_spy TYPE REF TO ltd_hot_hana_connector_spy.

    lr_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'tmp.hta.pack' ).

    "prepare mock data
    cl_cts_hta_package=>g_db_access = VALUE #( ). "do not expect call to db_access
    cl_cts_hta_package=>g_tadir_access = VALUE #( ). "do not expect call to tadir_access
    CAST cl_cts_hta_package( lr_package )->m_external_calls = VALUE #( ). "do not expect call to external_calls
    lr_hot_hana_connector_spy = NEW ltd_hot_hana_connector_spy( ).
    lr_hot_hana_connector_spy->m_expected_packages = VALUE #( ( CAST cl_cts_hta_package( lr_package )->m_hot_package ) ).
    CAST cl_cts_hta_package( lr_package )->gr_hot_hana_connector = lr_hot_hana_connector_spy.

    "execute business function (verified in spy)
    CAST cl_cts_hta_package( lr_package )->execute_sync( i_force = abap_false ).
  ENDMETHOD.

  METHOD execute_sync_with_excpetion.
    DATA: lr_package                TYPE REF TO if_cts_hta_package,
          lr_hot_hana_connector_spy TYPE REF TO ltd_hot_hana_connector_spy,
          lr_exc_exp                TYPE REF TO cx_hana_object_transport.

    lr_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'tmp.hta.pack' ).

    "prepare mock data
    cl_cts_hta_package=>g_db_access = VALUE #( ). "do not expect call to db_access
    cl_cts_hta_package=>g_tadir_access = VALUE #( ). "do not expect call to tadir_access
    CAST cl_cts_hta_package( lr_package )->m_external_calls = VALUE #( ). "do not expect call to external_calls
    lr_hot_hana_connector_spy = NEW ltd_hot_hana_connector_spy( ).
    CREATE OBJECT lr_exc_exp
      EXPORTING
        textid          = cx_hana_object_transport=>read_package_error
        msgv1           = CONV #( lr_package->hana_package_name )
        hana_error_code = '12345'
        hana_error_msg  = 'Some message'.
    lr_hot_hana_connector_spy->m_throw_exception = lr_exc_exp.
    CAST cl_cts_hta_package( lr_package )->gr_hot_hana_connector = lr_hot_hana_connector_spy.

    "execute business function (verified in spy)
    TRY.
        CAST cl_cts_hta_package( lr_package )->execute_sync( i_force = abap_false ).
      CATCH cx_cts_hta INTO DATA(lr_exc_act).
        cl_abap_unit_assert=>assert_equals( act = lr_exc_act->if_t100_message~t100key exp = lr_exc_exp->if_t100_message~t100key ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc_act->message_variable_1 exp = lr_exc_exp->message_variable_1 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc_act->message_variable_2 exp = lr_exc_exp->message_variable_2 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc_act->message_variable_3 exp = lr_exc_exp->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = lr_exc_act->message_variable_4 exp = lr_exc_exp->message_variable_4 ).
        cl_abap_unit_assert=>assert_equals( act = '12345' exp = lr_exc_exp->message_variable_3 ).
        cl_abap_unit_assert=>assert_equals( act = 'Some message' exp = lr_exc_exp->message_variable_4 ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cts_hta_package_with_db DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS:
      co_hta_package_id_1    TYPE cts_hot_package_id VALUE 'TMP.HTA.PACK1',
      co_hta_package_name_1  TYPE cts_hot_hana_package_id VALUE 'tmp.HTA.Pack1',
      co_hta_package_id_2    TYPE cts_hot_package_id VALUE 'TMP.HTA.PACK2',
      co_hta_package_name_2  TYPE cts_hot_hana_package_id VALUE 'tmp.HTA.Pack2',
      co_hta_package_id_3    TYPE cts_hot_package_id VALUE 'TMP.HTA.PACK3.SUB',
      co_hta_package_name_3  TYPE cts_hot_hana_package_id VALUE 'tmp.HTA.Pack3.sub',
      co_hta_package_id_4    TYPE cts_hot_package_id VALUE 'TMP.HTA.SUB.PACK4',
      co_hta_package_name_4  TYPE cts_hot_hana_package_id VALUE 'tmp.HTA.sub.Pack4',
      co_hana_package_id_1   TYPE cts_hot_package_id VALUE 'TMP.HANA.PACK1',
      co_hana_package_name_1 TYPE cts_hot_hana_package_id VALUE 'tmp.HANA.Pack1',
      co_hana_package_id_2   TYPE cts_hot_package_id VALUE 'TMP.HANA.PACK2',
      co_hana_package_name_2 TYPE cts_hot_hana_package_id VALUE 'tmp.HANA.Pack2',
      co_hana_package_id_3   TYPE cts_hot_package_id VALUE 'TMP.HANA.PACK3.SUB',
      co_hana_package_name_3 TYPE cts_hot_hana_package_id VALUE 'tmp.HANA.Pack3.sub',

      co_devclass_1          TYPE devclass VALUE 'HTA_DEVCLASS1',
      co_devclass_2          TYPE devclass VALUE 'HTA_DEVCLASS2',
      co_devclass_3          TYPE devclass VALUE 'HTA_DEVCLASS3'. "devclass without HOTA object

    DATA:
       ms_cts_hot_package_for_sync TYPE cts_hot_package.

    METHODS:
      "! prepare data in DB
      setup,
      "! delete data in DB
      teardown,

      "! Creates a package instance for passed devclass for 1 existing package (HOT_STATUS does not matter and ABAP_STATUS='A')
      create_from_devclass FOR TESTING RAISING cx_static_check,
      "! Creates a list of package instances for passed devclasses for several existing packages (HOT_STATUS does not matter and ABAP_STATUS='A')
      create_from_devclasses FOR TESTING RAISING cx_static_check,
      "! Creates a list of package instances for passed devclasses for several existing packages that can be deployed (HOT_STATUS='I' or 'D' and ABAP_STATUS='A')
      create_from_devclases_dep_only FOR TESTING RAISING cx_static_check,
      "! Same test as create_from_devclasses but this time not using CL_CTS_HTA_PACKAGE=&gt;create... but using IF_CTS_HTA_API_FACTORY-&gt;create_cts_hta_list_devclasses<br/>
      "! Implemented in this class because test setup in CL_CTS_HTA_API_FACTORY is too complicated
      create_from_devclasses_via_api FOR TESTING RAISING cx_static_check,

      "! Tests creation of packages list returning 1 entry in the list, input is 'tmp.HTA.Pack1'
      create_from_hana_key_hta_list1 FOR TESTING RAISING cx_static_check,
      "! Tests creation of packages list returning 3 entries in the list, input is 'tmp.HTA.Pack*' and 3 subpackages (tmp.HTA.Pack1 and tmp.HTA.Pack2 and tmp.HTA.Pack3.sub) exist
      create_from_hana_key_hta_list3 FOR TESTING RAISING cx_static_check,
      "! Tests creation of packages list returning 1 entry in the list, input is 'tmp.HANA.Pack1'
      create_from_hana_key_han_list1 FOR TESTING RAISING cx_static_check,
      "! Tests creation of packages list returning 3 entries in the list, input is 'tmp.HANA.Pack*' and 3 subpackages (tmp.HANA.Pack1 and tmp.HANA.Pack2 and tmp.HANA.Pack3.sub) exist
      create_from_hana_key_han_list3 FOR TESTING RAISING cx_static_check,
      "! Tests creation of packages list returning 2 entries in the list (1 from HTA and one from HANA), input is '*Pack1*' and 2 subpackages (tmp.HTA.Pack1, tmp.HANA.Pack1) exist.
      create_from_hana_key_hta_han2 FOR TESTING RAISING cx_static_check,
      "! Tests creation of packages list returning 3 entries in the list, input is 'tmp.HTA.Pack*' and 3 subpackages (tmp.HTA.Pack1, tmp.HTA.Pack2, tmp.HTA.Pack3.sub) exist.
      "! But this time the 2 HTA packages (tmp.HTA.Pack2 and tmp.HTA.Pack3) are also existing in HANA but should be returned only once
      create_from_hana_key_hta_han3 FOR TESTING RAISING cx_static_check,

      "! Tests creation of a package instance from transport key (key data exists in HTA)
      create_from_obj_name FOR TESTING RAISING cx_static_check,
      "! Tests creation of a package instance from transport key (key data exists in HTA) using abap_staus='I' (SNote/CWB case)
      create_from_obj_name_inactive FOR TESTING RAISING cx_static_check,
      "! Tests creation of a package instance from transport key that does not exist in HTA
      create_from_obj_name_not_exist FOR TESTING RAISING cx_static_check,

      "! Tests the setting of prework done flag to prework done for a package with prework (deploymode P), entry in cts_hot_prework not existing before
      set_prework_done_pkg_w_prework FOR TESTING RAISING cx_static_check,
      "! Tests the setting of prework done flag to prework done for a package without prework (deploymode A), entry in cts_hot_prework not existing before
      set_prework_done_pkg_wo_prwrk1 FOR TESTING RAISING cx_static_check,
      "! Tests the setting of prework done flag to prework done for a package without prework (deploymode A), entry in cts_hot_prework existing before
      set_prework_done_pkg_wo_prwrk2 FOR TESTING RAISING cx_static_check,
      "! Tests the setting of prework done flag to prework not done for a package with prework (deploymode P), entry in cts_hot_prework not existing before
      set_prework_not_done_pkg_w_p1 FOR TESTING RAISING cx_static_check,
      "! Tests the setting of prework done flag to prework not done for a package with prework (deploymode P), entry in cts_hot_prework existing before
      set_prework_not_done_pkg_w_p2 FOR TESTING RAISING cx_static_check,
      "! Tests the setting of prework done flag to prework not done for a package without prework (deploymode A), entry in cts_hot_prework not existing before
      set_prework_not_done_pkg_wo_p FOR TESTING RAISING cx_static_check,

      "! Testing get_deploy_state in case the package is deployed (HOT_STATUS=A)
      get_deploy_state_deployed_a FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the package is deployed (HOT_STATUS=N)
      get_deploy_state_deployed_n FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the package is not deployed (HOT_STATUS=I)
      get_deploy_state_not_depl_i FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the package is not deployed (HOT_STATUS=D)
      get_deploy_state_not_depl_d FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the package is not deployed (HOT_STATUS=F as not supported status)
      get_deploy_state_not_depl_f FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state for unknown package in HTA
      get_deploy_state_unknown_obj FOR TESTING RAISING cx_static_check,

      "! Testing get_sync_state in case package is in sync with HANA (some attribute is different in HANA and HTA)
      get_sync_state_in_sync FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package does neither exist in HANA nor in HTA
      get_sync_state_in_sync_ne FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (description is different in HANA and HTA)
      get_sync_state_not_in_sync_des FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (hint_for_translation is different in HANA and HTA)
      get_sync_state_not_in_sync_hft FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (is structural is different in HANA and HTA)
      get_sync_state_not_in_sync_is FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (original_language is different in HANA and HTA)
      get_sync_state_not_in_sync_ol FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (responsible is different in HANA and HTA)
      get_sync_state_not_in_sync_res FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (src_system is different in HANA and HTA)
      get_sync_state_not_in_sync_ssy FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (src_tenant is different in HANA and HTA)
      get_sync_state_not_in_sync_ste FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (text_collection is different in HANA and HTA)
      get_sync_state_not_in_sync_tc FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (text_status is different in HANA and HTA)
      get_sync_state_not_in_sync_ts FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (text_term_domain is different in HANA and HTA)
      get_sync_state_not_in_sync_ttd FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (package exist in HANA but not in HTA)<br/>nea=not existing in abap
      get_sync_state_not_in_sync_nea FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package is not in sync with HANA (package does not exist in HANA but in HTA)<br/>neh=not existing in hana
      get_sync_state_not_in_sync_neh FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package can not be synchronized because of wrong status in HTA ('I')
      get_sync_state_not_syncable_i FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package can not be synchronized because of wrong status in HTA ('D')
      get_sync_state_not_syncable_d FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case package can not be synchronized because of name conflict between HANA name and HTA name
      get_sync_state_not_syncable_nc FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS cl_cts_hta_package DEFINITION LOCAL FRIENDS ltcl_cts_hta_package_with_db.
CLASS ltcl_cts_hta_package_with_db IMPLEMENTATION.
  METHOD setup.
    cl_cts_hta_package=>g_tadir_access = NEW ltd_tadir_access( ).
    cl_cts_hta_package=>gr_hot_hana_connector = NEW ltd_hot_hana_connector_respond( ).

    DATA ls_cts_hot_package TYPE cts_hot_package.

    "prepare data in DB - DO NOT CHANGE here because used in all tests with these values
    ls_cts_hot_package-abap_hana_package_id = co_hta_package_id_1.
    ls_cts_hot_package-abap_status          = cl_cts_hta_component=>co_active_version.
    ls_cts_hot_package-hot_status           = if_cts_hot_db_access=>co_hot_status_inactive.
    ls_cts_hot_package-hana_package_id      = co_hta_package_name_1.
    ls_cts_hot_package-hot_activation_mode  = if_cts_hot_db_access=>co_hot_deploy_mode_always.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    ls_cts_hot_package-abap_hana_package_id = co_hta_package_id_2.
    ls_cts_hot_package-abap_status          = cl_cts_hta_component=>co_active_version.
    ls_cts_hot_package-hot_status           = if_cts_hot_db_access=>co_hot_status_to_be_deleted.
    ls_cts_hot_package-hana_package_id      = co_hta_package_name_2.
    ls_cts_hot_package-hot_activation_mode  = if_cts_hot_db_access=>co_hot_deploy_mode_prework.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    ls_cts_hot_package-abap_hana_package_id = co_hta_package_id_3.
    ls_cts_hot_package-abap_status          = cl_cts_hta_component=>co_active_version.
    ls_cts_hot_package-hot_status           = if_cts_hot_db_access=>co_hot_status_active.
    ls_cts_hot_package-hana_package_id      = co_hta_package_name_3.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    ls_cts_hot_package-abap_hana_package_id = co_hta_package_id_4.
    ls_cts_hot_package-abap_status          = cl_cts_hta_component=>co_active_version.
    ls_cts_hot_package-hot_status           = if_cts_hot_db_access=>co_hot_status_new.
    ls_cts_hot_package-hana_package_id      = co_hta_package_name_4.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    "prepare data in DB for get_sync_state tests
    ms_cts_hot_package_for_sync-abap_hana_package_id = '123TMP456.HTA.PACK'.
    ms_cts_hot_package_for_sync-hana_package_id = '123tmp456.hta.pack'.
    ms_cts_hot_package_for_sync-hana_pack_delivery_unit = 'DU_NAME'.
    ms_cts_hot_package_for_sync-hana_pack_du_vendor = 'DU_VENDOR'.
    ms_cts_hot_package_for_sync-hana_pack_description = 'Some Description'.
    ms_cts_hot_package_for_sync-hana_pack_hints_for_transl = 'Hint for transl'.
    ms_cts_hot_package_for_sync-hana_pack_is_structural = '0'.
    ms_cts_hot_package_for_sync-hana_pack_orig_lang = 'en_US'.
    ms_cts_hot_package_for_sync-hana_pack_responsible = 'ich'.
    ms_cts_hot_package_for_sync-hana_pack_src_system = 'SRC'.
    ms_cts_hot_package_for_sync-hana_pack_src_tenant = ''. "usually empty, therefore also taking space here
    ms_cts_hot_package_for_sync-hana_pack_text_collection = 'text collection'.
    ms_cts_hot_package_for_sync-hana_pack_text_status = 'text status'.
    ms_cts_hot_package_for_sync-hana_pack_text_term_domain = 'terminology domain'.
    ms_cts_hot_package_for_sync-abap_status = 'A'.
    ms_cts_hot_package_for_sync-hot_status = if_cts_hot_db_access=>co_hot_status_new.
    MODIFY cts_hot_package FROM ms_cts_hot_package_for_sync.
  ENDMETHOD.

  METHOD teardown.
    "delete data in DB
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = co_hta_package_id_1.
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = co_hta_package_id_2.
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = co_hta_package_id_3.
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = co_hta_package_id_4.
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = ms_cts_hot_package_for_sync-abap_hana_package_id.

    DELETE FROM cts_hot_prework WHERE abap_hana_package_id = co_hta_package_id_1.
    DELETE FROM cts_hot_prework WHERE abap_hana_package_id = co_hta_package_id_2.
    DELETE FROM cts_hot_prework WHERE abap_hana_package_id = co_hta_package_id_3.
    DELETE FROM cts_hot_prework WHERE abap_hana_package_id = co_hta_package_id_4.
  ENDMETHOD.

  METHOD create_from_devclass.
    DATA: ls_cts_hot_package TYPE cts_hot_package,
          lt_hta_packages    TYPE if_cts_hta_types=>ty_cts_hta_packages,
          lr_hta_package     TYPE REF TO if_cts_hta_package.

    lt_hta_packages = cl_cts_hta_package=>create_instances_from_devclass( i_devclasses = VALUE if_cts_hta_types=>ty_devclasses( ( co_devclass_1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_hta_packages ) exp = 1 ).

    lr_hta_package = lt_hta_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_1 ).
  ENDMETHOD.

  METHOD create_from_devclasses.
    DATA: ls_cts_hot_package TYPE cts_hot_package,
          lt_hta_packages    TYPE if_cts_hta_types=>ty_cts_hta_packages,
          lr_hta_package     TYPE REF TO if_cts_hta_package.

    lt_hta_packages = cl_cts_hta_package=>create_instances_from_devclass( i_devclasses = VALUE #( ( co_devclass_1 )
                                                                                                  ( co_devclass_2 )
                                                                                                  ( co_devclass_3 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_hta_packages ) exp = 4 ).

    lr_hta_package = lt_hta_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_1 ).

    lr_hta_package = lt_hta_packages[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_2 ).

    lr_hta_package = lt_hta_packages[ 3 ].
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_3 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_3 ).

    lr_hta_package = lt_hta_packages[ 4 ].
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_4 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_4 ).
  ENDMETHOD.

  METHOD create_from_devclasses_via_api.
    DATA: ls_cts_hot_package    TYPE cts_hot_package,
          lr_hta_package        TYPE REF TO if_cts_hta_package,
          lt_hta_components     TYPE if_cts_hta_types=>ty_cts_hta_components,
          lt_hta_full_packages  TYPE if_cts_hta_types=>ty_cts_hta_components,
          lr_hta_component_list TYPE REF TO if_cts_hta_component_list,
          lr_hta_full_package   TYPE REF TO if_cts_hta_full_package.

    "testing cl_cts_hta_api_factory because there the setup would be too complicated
    lr_hta_component_list = cl_cts_hta_api_factory=>create_instance( )->create_list_by_abap_packages( VALUE #( ( co_devclass_1 )
                                                                                                               ( co_devclass_2 )
                                                                                                               ( co_devclass_3 ) ) ).
    lt_hta_components = lr_hta_component_list->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_hta_components ) exp = 0 ).
    lt_hta_full_packages = lr_hta_component_list->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_hta_full_packages ) exp = 4 ).

    lt_hta_components = CAST if_cts_hta_full_package( lt_hta_full_packages[ 1 ] )->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    lr_hta_package = CAST if_cts_hta_package( lt_hta_components[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_1 ).

    lt_hta_components = CAST if_cts_hta_full_package( lt_hta_full_packages[ 2 ] )->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    lr_hta_package = CAST if_cts_hta_package( lt_hta_components[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_2 ).

    lt_hta_components = CAST if_cts_hta_full_package( lt_hta_full_packages[ 3 ] )->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    lr_hta_package = CAST if_cts_hta_package( lt_hta_components[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_3 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_3 ).

    lt_hta_components = CAST if_cts_hta_full_package( lt_hta_full_packages[ 4 ] )->if_cts_hta_component_list~get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    lr_hta_package = CAST if_cts_hta_package( lt_hta_components[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_4 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_4 ).
  ENDMETHOD.

  METHOD create_from_devclases_dep_only.
    DATA: ls_cts_hot_package TYPE cts_hot_package,
          lt_hta_packages    TYPE if_cts_hta_types=>ty_cts_hta_packages,
          lr_hta_package     TYPE REF TO if_cts_hta_package.

    lt_hta_packages = cl_cts_hta_package=>create_instances_from_devclass( i_devclasses = VALUE #( ( co_devclass_1 )
                                                                                                  ( co_devclass_2 )
                                                                                                  ( co_devclass_3 ) )
                                                                          i_deployable_only = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_hta_packages ) exp = 2 ).

    lr_hta_package = lt_hta_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_1 ).

    lr_hta_package = lt_hta_packages[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->hana_package_name exp = co_hta_package_name_2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~transport_object_name exp = co_hta_package_id_2 ).
  ENDMETHOD.

  METHOD create_from_hana_key_hta_list1.
    DATA: lt_packages TYPE if_cts_hta_types=>ty_cts_hta_packages.

    lt_packages = cl_cts_hta_package=>create_instances_from_hana_key( co_hta_package_name_1 ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 1 ).
    DATA(lr_package) = lt_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_hana_key_hta_list3.
    DATA: lt_packages TYPE if_cts_hta_types=>ty_cts_hta_packages.

    lt_packages = cl_cts_hta_package=>create_instances_from_hana_key( 'tmp.HTA.Pack*' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 3 ).
    DATA(lr_package) = lt_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).

    lr_package = lt_packages[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_2 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).

    lr_package = lt_packages[ 3 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_3 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_3 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_hana_key_han_list1.
    DATA: lt_packages TYPE if_cts_hta_types=>ty_cts_hta_packages.

    lt_packages = cl_cts_hta_package=>create_instances_from_hana_key( i_hana_package_name = co_hana_package_name_1
                                                                      i_search_in_hta_only = abap_false ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 1 ).
    DATA(lr_package) = lt_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hana_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hana_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_hana_key_han_list3.
    DATA: lt_packages TYPE if_cts_hta_types=>ty_cts_hta_packages.

    lt_packages = cl_cts_hta_package=>create_instances_from_hana_key( i_hana_package_name = 'tmp.HANA.Pack*'
                                                                      i_search_in_hta_only = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 3 ).
    DATA(lr_package) = lt_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hana_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hana_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).

    lr_package = lt_packages[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hana_package_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hana_package_name_2 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).

    lr_package = lt_packages[ 3 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hana_package_id_3 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hana_package_name_3 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_hana_key_hta_han2.
    DATA: lt_packages TYPE if_cts_hta_types=>ty_cts_hta_packages.

    lt_packages = cl_cts_hta_package=>create_instances_from_hana_key( i_hana_package_name = '*Pack1*'
                                                                      i_search_in_hta_only = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 2 ).
    DATA(lr_package) = lt_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).

    lr_package = lt_packages[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hana_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hana_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_hana_key_hta_han3.
    DATA: lt_packages TYPE if_cts_hta_types=>ty_cts_hta_packages.

    lt_packages = cl_cts_hta_package=>create_instances_from_hana_key( i_hana_package_name = 'tmp.HTA.Pack*'
                                                                      i_search_in_hta_only = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_packages ) exp = 3 ).
    DATA(lr_package) = lt_packages[ 1 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_1 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).

    lr_package = lt_packages[ 2 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_2 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).

    lr_package = lt_packages[ 3 ].
    cl_abap_unit_assert=>assert_equals( act = lr_package->component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->transport_object_name exp = co_hta_package_id_3 ).
    cl_abap_unit_assert=>assert_equals( act = lr_package->hana_package_name exp = co_hta_package_name_3 ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_obj_name.
    DATA: lr_hta_package     TYPE REF TO if_cts_hta_package,
          ls_cts_hot_package TYPE cts_hot_package.

    "test create instance
    lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = CONV #( co_hta_package_id_1 ) ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_package->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_package ).
    cl_abap_unit_assert=>assert_equals( exp = co_hta_package_name_1 act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = co_hta_package_id_1 act = lr_hta_package->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_hta_package )->m_abap_status exp = cl_cts_hta_component=>co_active_version ).
  ENDMETHOD.

  METHOD create_from_obj_name_not_exist.
    DATA: lr_hta_package    TYPE REF TO if_cts_hta_package.

    "test create instance and expect exception for active version
    TRY.
        lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.NOT.EXIST.PACK' ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_cts_hta_not_found INTO DATA(lr_cx).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->if_t100_message~t100key exp = cx_cts_hta_not_found=>package_not_found_in_hta ).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_1 exp = 'TMP.HTA.NOT.EXIST.PACK' ).
    ENDTRY.

    "test create instance and expect exception for inactive version
    TRY.
        lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.NOT.EXIST.PACK' i_abap_status = cl_cts_hta_component=>co_inactive_version ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_cts_hta_not_found INTO lr_cx.
        cl_abap_unit_assert=>assert_equals( act = lr_cx->if_t100_message~t100key exp = cx_cts_hta_not_found=>package_not_found_in_hta ).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_1 exp = 'TMP.HTA.NOT.EXIST.PACK' ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_from_obj_name_inactive.
    DATA: lr_hta_package     TYPE REF TO if_cts_hta_package,
          ls_cts_hot_package TYPE cts_hot_package.

    "prepare data in DB
    UPDATE cts_hot_package SET abap_status = cl_cts_hta_component=>co_inactive_version WHERE abap_hana_package_id = co_hta_package_id_2.

    "test create instance
    lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = CONV #( co_hta_package_id_2 )
                                                                        i_abap_status = cl_cts_hta_component=>co_inactive_version ).

    cl_abap_unit_assert=>assert_equals( exp = co_hta_package_name_2 act = lr_hta_package->hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = co_hta_package_id_2 act = lr_hta_package->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = CAST cl_cts_hta_package( lr_hta_package )->m_abap_status exp = cl_cts_hta_component=>co_inactive_version ).
  ENDMETHOD.

  METHOD get_deploy_state_deployed_a.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = co_hta_package_name_3 ).

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = lr_package->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_deployed_n.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = co_hta_package_name_4 ).

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = lr_package->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_not_depl_i.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = co_hta_package_name_1 ).

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = lr_package->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_not_depl_d.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = co_hta_package_name_2 ).

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = lr_package->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_not_depl_f.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = co_hta_package_name_1 ).

    "prepare data in DB (different to me->setup)
    UPDATE cts_hot_package SET hot_status = 'F' WHERE abap_hana_package_id = co_hta_package_id_1.

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = lr_package->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_unknown_obj.
    DATA: lr_package TYPE REF TO if_cts_hta_package,
          lr_cx      TYPE REF TO cx_cts_hta_not_found.

    lr_package = cl_cts_hta_package=>create_instance_from_hana_key( i_hana_package_name = 'tmp.hta.not.exist.pack' ).

    "execute business function and verify
    TRY.
        lr_package->get_deploy_state( ).
        cl_abap_unit_assert=>fail( 'Expected exception cx_cts_hta_not_found was not thrown.' ).
      CATCH cx_cts_hta_not_found INTO lr_cx.
        IF lr_cx->if_t100_message~t100key = cx_cts_hta_not_found=>package_not_found_in_hta.
          "expected exception got
          cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_1 exp = 'TMP.HTA.NOT.EXIST.PACK' ).
        ELSE.
          cl_abap_unit_assert=>fail( 'Unexpected exception caught.' ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD set_prework_done_pkg_wo_prwrk1.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( co_hta_package_name_1 ).

    "execute
    lr_package->set_prework( ce_cts_hta_prework=>prework_done ).

    "verify
    SELECT SINGLE * FROM cts_hot_prework INTO @DATA(ls_cts_hot_prework) WHERE abap_hana_package_id = @co_hta_package_id_1.
    DATA(lv_sysubrc) = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = lv_sysubrc exp = 4 ).
    cl_abap_unit_assert=>assert_initial( ls_cts_hot_prework ).
  ENDMETHOD.

  METHOD set_prework_done_pkg_wo_prwrk2.
    DATA ls_cts_hot_prework TYPE cts_hot_prework.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( co_hta_package_name_1 ).

    "prepare
    UPDATE cts_hot_package SET hot_activation_mode = if_cts_hot_db_access=>co_hot_deploy_mode_prework WHERE abap_hana_package_id = co_hta_package_id_1.
    ls_cts_hot_prework-abap_hana_package_id = co_hta_package_id_1.
    ls_cts_hot_prework-prework_done = ce_cts_hta_prework=>prework_done->value.
    MODIFY cts_hot_prework FROM ls_cts_hot_prework.

    "execute
    lr_package->set_prework( ce_cts_hta_prework=>prework_done ).

    "verify
    SELECT SINGLE * FROM cts_hot_prework INTO ls_cts_hot_prework WHERE abap_hana_package_id = co_hta_package_id_1.
    DATA(lv_sysubrc) = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = lv_sysubrc exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-abap_hana_package_id exp = co_hta_package_id_1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-prework_done exp = ce_cts_hta_prework=>prework_done->value ).
  ENDMETHOD.

  METHOD set_prework_done_pkg_w_prework.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( co_hta_package_name_2 ).

    "execute
    lr_package->set_prework( ).

    "verify
    SELECT SINGLE * FROM cts_hot_prework INTO @DATA(ls_cts_hot_prework) WHERE abap_hana_package_id = @co_hta_package_id_2.
    DATA(lv_sysubrc) = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = lv_sysubrc exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-abap_hana_package_id exp = co_hta_package_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-prework_done exp = ce_cts_hta_prework=>prework_done->value ).
  ENDMETHOD.

  METHOD set_prework_not_done_pkg_wo_p.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( co_hta_package_name_1 ).

    "execute
    lr_package->set_prework( ce_cts_hta_prework=>prework_not_done ).

    "verify
    SELECT SINGLE * FROM cts_hot_prework INTO @DATA(ls_cts_hot_prework) WHERE abap_hana_package_id = @co_hta_package_id_1.
    DATA(lv_sysubrc) = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = lv_sysubrc exp = 4 ).
    cl_abap_unit_assert=>assert_initial( ls_cts_hot_prework ).
  ENDMETHOD.

  METHOD set_prework_not_done_pkg_w_p1.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( co_hta_package_name_2 ).

    "execute
    lr_package->set_prework( ce_cts_hta_prework=>prework_not_done ).

    "verify
    SELECT SINGLE * FROM cts_hot_prework INTO @DATA(ls_cts_hot_prework) WHERE abap_hana_package_id = @co_hta_package_id_2.
    DATA(lv_sysubrc) = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = lv_sysubrc exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-abap_hana_package_id exp = co_hta_package_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-prework_done exp = ce_cts_hta_prework=>prework_not_done->value ).
  ENDMETHOD.

  METHOD set_prework_not_done_pkg_w_p2.
    DATA ls_cts_hot_prework TYPE cts_hot_prework.
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( co_hta_package_name_2 ).

    "prepare
    ls_cts_hot_prework-abap_hana_package_id = co_hta_package_id_1.
    ls_cts_hot_prework-prework_done = ce_cts_hta_prework=>prework_done->value.
    MODIFY cts_hot_prework FROM ls_cts_hot_prework.

    "execute
    lr_package->set_prework( ce_cts_hta_prework=>prework_not_done ).

    "verify
    SELECT SINGLE * FROM cts_hot_prework INTO ls_cts_hot_prework WHERE abap_hana_package_id = co_hta_package_id_2.
    DATA(lv_sysubrc) = sy-subrc.
    cl_abap_unit_assert=>assert_equals( act = lv_sysubrc exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-abap_hana_package_id exp = co_hta_package_id_2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_cts_hot_prework-prework_done exp = ce_cts_hta_prework=>prework_not_done->value ).
  ENDMETHOD.

  METHOD get_sync_state_in_sync.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    " 1. Test: all same in HANA and HTA
    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).


    " 2. Test: different DU name is not a difference from HTA perspective, because we do not write DU name to HANA during imports
    "prepare DB
    UPDATE cts_hot_package SET hana_pack_delivery_unit = 'ABC' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    lr_sync_state = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).

    "reset DB
    UPDATE cts_hot_package SET hana_pack_delivery_unit = me->ms_cts_hot_package_for_sync-hana_pack_delivery_unit WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    " 3. Test: different DU vendor is not a difference from HTA perspective, because we do not write DU name to HANA during imports
    "prepare DB
    UPDATE cts_hot_package SET hana_pack_du_vendor = 'new_vendor' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    lr_sync_state = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).

*    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
*    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
*    lr_cx_cts_hta_wrong_status ?= CAST cx_cts_hta_wrong_status( lt_reasons[ 1 ] ). "cast fails if wrong type
*    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->cts_hta_component exp = m_cut ).
*    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>object_requires_deployment ).
  ENDMETHOD.

  METHOD get_sync_state_in_sync_ne.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( 'unknown.package' ).

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_des.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_description = 'some new description' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_hft.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_hints_for_transl = 'some new hint' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_is.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_is_structural = '1' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_ol.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_orig_lang = 'ax_AX' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_res.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_responsible = 'new responsible' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_ssy.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_src_system = 'new src system' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_ste.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_src_tenant = 'new src tenant' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_tc.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_text_collection = 'new text collection' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_ts.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_text_status = 'new text status' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_ttd.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_pack_text_term_domain = 'new text terminology domain' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_nea.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_neh.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare mock data
    CAST ltd_hot_hana_connector_respond( cl_cts_hta_package=>gr_hot_hana_connector )->gv_respond_with_data = abap_false.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_nc.
    DATA: lt_reasons                  TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hana_package_id = 'different.packid' WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    data(lr_cx_cts_hta_name_conflict) = CAST cx_cts_hta_name_conflict( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->cts_hta_component exp = lr_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>package_name_conflict ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_i.
    DATA: lt_reasons                 TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    data(lr_cx_cts_hta_wrong_status) = CAST cx_cts_hta_wrong_status( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->cts_hta_component exp = lr_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>package_requires_deployment ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_d.
    DATA: lt_reasons                 TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = cl_cts_hta_package=>create_instance_from_hana_key( ms_cts_hot_package_for_sync-hana_package_id ).

    "prepare DB
    UPDATE cts_hot_package SET hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted WHERE abap_hana_package_id = me->ms_cts_hot_package_for_sync-abap_hana_package_id.

    "execute business function
    DATA(lr_sync_state) = lr_package->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    data(lr_cx_cts_hta_wrong_status) = CAST cx_cts_hta_wrong_status( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->cts_hta_component exp = lr_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>package_requires_deployment ).
  ENDMETHOD.

ENDCLASS.