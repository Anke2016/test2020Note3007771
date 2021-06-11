*"* use this source file for your ABAP unit test classes
CLASS ltd_tadir_access DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES lif_tadir_access.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_tadir_access IMPLEMENTATION.
  METHOD lif_tadir_access~read_hota_objnames_for_devclas.
    LOOP AT i_devclasses INTO DATA(lv_devclass).
      IF lv_devclass = 'HTA_DEVCLASS1'. "simple case, 1 devclass has 1 hota object
        APPEND 'TMP.HTA.DEVCLASS1.PACK1' TO r_result.
      ENDIF.
      IF lv_devclass = 'HTA_DEVCLASS2'. "complex case, 1 devclass with 3 hota_objects
        APPEND 'TMP.HTA.DEVCLASS2.PACK1' TO r_result.
        APPEND 'TMP.HTA.DEVCLASS2.PACK2' TO r_result.
        APPEND 'TMP.HTA.DEVCLASS2.PACK3' TO r_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS ltd_cts_hta_package DEFINITION FINAL FOR TESTING
INHERITING FROM cl_cts_hta_component
CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      if_cts_hta_package.

    DATA:
      hana_name    TYPE cts_hot_hana_package_id READ-ONLY.

    METHODS:
      if_cts_hta_component~deploy REDEFINITION,
      if_cts_hta_component~get_deploy_state REDEFINITION,
      if_cts_hta_component~get_sync_state REDEFINITION,
      if_cts_hta_component~set_prework REDEFINITION,
      if_cts_hta_component~set_deploy_mode REDEFINITION,
      if_cts_hta_component~set_translation_relevance REDEFINITION,

      constructor
        IMPORTING
          i_hana_name TYPE cts_hot_hana_package_id
        RAISING
          cx_hana_object_transport.
  PROTECTED SECTION.
    METHODS:
      rs_corr_check REDEFINITION,
      rs_corr_insert REDEFINITION,
      execute_sync REDEFINITION.
    METHODS: hta_pre_sync_check REDEFINITION.
    METHODS: read_hana_data REDEFINITION,
      read_hta_data REDEFINITION.
ENDCLASS.

CLASS ltd_cts_hta_package IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_package.
    me->hana_name = i_hana_name.
    me->m_hot_package = cl_cts_hot_package=>create_instance( i_hana_name ).
  ENDMETHOD.

  METHOD if_cts_hta_component~set_prework.
    cl_abap_unit_assert=>fail( 'Call to set_prework not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~deploy.
    cl_abap_unit_assert=>fail( 'Call to deploy not expected in testdouble' ).
  ENDMETHOD.

  METHOD if_cts_hta_component~get_deploy_state.
    cl_abap_unit_assert=>fail( 'Call to get_deploy_state not expected in testdouble' ).
  ENDMETHOD.

  METHOD execute_sync.
    cl_abap_unit_assert=>fail( 'Call to execute_sync not expected in testdouble' ).
  ENDMETHOD.

  METHOD rs_corr_check.
    cl_abap_unit_assert=>fail( 'Call to rs_corr_check not expected in testdouble' ).
  ENDMETHOD.

  METHOD rs_corr_insert.
    cl_abap_unit_assert=>fail( 'Call to rs_corr_check not expected in testdouble' ).
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

  METHOD if_cts_hta_component~get_sync_state.
    cl_abap_unit_assert=>fail( 'Call to get_sync_state not expected in testdouble' ).
  ENDMETHOD.

  METHOD read_hana_data.
    cl_abap_unit_assert=>fail( 'Call to read_hana_data not expected in testdouble' ).
  ENDMETHOD.

  METHOD read_hta_data.
    cl_abap_unit_assert=>fail( 'Call to read_hta_data not expected in testdouble' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cts_hta_object DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Testing creation of simple object with create_instance_from_hana_key
      create_from_hana_key FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with create_instance_from_hana_key
      "! but using spaces in front of object name, suffix and package name.
      create_from_hana_key_spaces FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with object name and suffix length 70
      "! (max supportedl length without hashing the name)
      create_from_hana_key_70_chars FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with object name and suffix length = 71
      "! First length where hasing should take place
      create_from_hana_key_71_chars FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with object name and suffix length more than 71
      "! so that hashing needs to be used. Also suffix is longer than 20 chars but maximum of
      "! 20 chars should be used, rest to cut off
      create_from_hana_key_long_name FOR TESTING RAISING cx_static_check,
      "! Testing that 2 objects with only difference in object name case are conflicting
      create_from_hana_key_conflict FOR TESTING RAISING cx_static_check,
      "! Testing the creation of an object that has a package name that needs hashing and
      "! that has an object name that needs hashing
      create_from_hana_key_long2  FOR TESTING RAISING cx_static_check,

      "! Tests creation of an object instance from transport key (key data exists in HTA)
      create_from_obj_name FOR TESTING RAISING cx_static_check,
      "! Tests creation of an object instance from transport key (key data exists in HTA) using abap_staus='I' (SNote/CWB case). Package also exists with abap_status = 'I'
      create_from_obj_name_inactive FOR TESTING RAISING cx_static_check,
      "! Tests creation of an object instance from transport key (key data exists in HTA) using abap_staus='I' (SNote/CWB case). Package exists with abap_status = 'A'
      create_from_obj_name_inactive2 FOR TESTING RAISING cx_static_check,
      "! Tests creation of an object instance from transport key but object does not exist in HTA (package exists)
      create_from_obj_name_not_exist FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS cl_cts_hta_object DEFINITION LOCAL FRIENDS ltcl_cts_hta_object.
CLASS ltcl_cts_hta_object IMPLEMENTATION.

  METHOD create_from_hana_key.
    DATA: lr_hta_package   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl TYPE REF TO cl_cts_hta_object.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( 'this.is.my.Package' ).

    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = 'Object_Name'
                                i_hana_object_suffix = 'Suffix' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp = 'THIS.IS.MY.PACKAGE                      OBJECT_NAME' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'this.is.my.Package' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'Object_Name' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'Suffix' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'this.is.my.Package' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'Object_Name' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'Suffix' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'THIS.IS.MY.PACKAGE' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'OBJECT_NAME' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).

  ENDMETHOD.

  METHOD create_from_hana_key_70_chars.
    DATA: lr_hta_package   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl TYPE REF TO cl_cts_hta_object.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( 'hana_pack' ).

    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_70_chars_INCLUDING_SUFFIX'
                                i_hana_object_suffix = 'some_suff' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_object ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp =
                                                                              'HANA_PACK                               THIS_IS_SOME_LONG_OBJECT_NAME_WITH_70_CHARS_INCLUDING_SUFFIX' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SOME_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_70_chars_INCLUDING_SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'some_suff' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_70_chars_INCLUDING_SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'some_suff' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'HANA_PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'THIS_IS_SOME_LONG_OBJECT_NAME_WITH_70_CHARS_INCLUDING_SUFFIX' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SOME_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
  ENDMETHOD.

  METHOD create_from_hana_key_71_chars.
    DATA: lr_hta_package   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl TYPE REF TO cl_cts_hta_object.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( 'hana.pack' ).

    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_71_chars_INCLUDING_SUFFIXX'
                                i_hana_object_suffix = 'some_suff' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp =
                                                               `HANA.PACK                               ` &&
                                                                                      'THIS_IS_SOME_LONG_OBJECT_NA' && cl_cts_hot_package=>co_hash_seperator && `IA3DCFITIBM1PAVO3ACOO6ELRR3GIA1L` && cl_cts_hot_package=>co_hash_seperator && 'SOME_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'some_suff' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'some_suff' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'HANA.PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp =
                                                                                      'THIS_IS_SOME_LONG_OBJECT_NA' && cl_cts_hot_package=>co_hash_seperator && `IA3DCFITIBM1PAVO3ACOO6ELRR3GIA1L` && cl_cts_hot_package=>co_hash_seperator && 'SOME_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
  ENDMETHOD.

  METHOD create_from_hana_key_conflict.
    DATA: lr_hta_package_1   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if_1 TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl_1 TYPE REF TO cl_cts_hta_object,
          lr_hta_package_2   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if_2 TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl_2 TYPE REF TO cl_cts_hta_object.

    "First package and object
    lr_hta_package_1 = cl_cts_hta_package=>create_instance_from_hana_key( 'hana.pack' ).

    lr_hta_object_if_1 = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package_1
                                i_hana_object_name = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX'
                                i_hana_object_suffix = 'some_longer_suffix_this_time' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_1->if_cts_hta_component~transport_object_name exp =
                                                               `HANA.PACK                               ` &&
                                                                                      `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_1->object_key-hana_package_name exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_1->object_key-hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_1->object_key-hana_object_suffix exp = 'some_longer_suffix_this_time' ).

    lr_hta_object_cl_1 = CAST cl_cts_hta_object( lr_hta_object_if_1 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_1->m_hot_object->hana_package_id exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_1->m_hot_object->hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_1->m_hot_object->hana_object_suffix exp = 'some_longer_suffix_this_time' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_1->m_hot_object->abap_hana_package_id exp = 'HANA.PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_1->m_hot_object->abap_hana_object_name_suffix exp =
                                                                                      `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_1->m_hot_object->transport_object_name exp = lr_hta_object_if_1->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( lr_hta_object_cl_1->m_hot_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_1->m_hta_package exp = lr_hta_package_1 ).

    "Second package and object
    lr_hta_package_2 = cl_cts_hta_package=>create_instance_from_hana_key( 'hana.pack' ).

    lr_hta_object_if_2 = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package_2
                                i_hana_object_name = 'this_IS_SOME_long_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX'
                                i_hana_object_suffix = 'some_Longer_SUFFIX_this_TiMe' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_2->if_cts_hta_component~transport_object_name exp =
                                                               `HANA.PACK                               ` &&
                                                                                      `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_2->object_key-hana_package_name exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_2->object_key-hana_object_name exp = 'this_IS_SOME_long_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if_2->object_key-hana_object_suffix exp = 'some_Longer_SUFFIX_this_TiMe' ).

    lr_hta_object_cl_2 = CAST cl_cts_hta_object( lr_hta_object_if_2 ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_2->m_hot_object->hana_package_id exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_2->m_hot_object->hana_object_name exp = 'this_IS_SOME_long_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_2->m_hot_object->hana_object_suffix exp = 'some_Longer_SUFFIX_this_TiMe' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_2->m_hot_object->abap_hana_package_id exp = 'HANA.PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_2->m_hot_object->abap_hana_object_name_suffix exp =
                                                                                      `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_2->m_hot_object->transport_object_name exp = lr_hta_object_if_2->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( lr_hta_object_cl_2->m_hot_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl_2->m_hta_package exp = lr_hta_package_2 ).

    " verify all objects have same abap_hana_object_name_suffix and same transport_object_name
    cl_abap_unit_assert=>assert_equals( exp = lr_hta_object_cl_1->m_hot_object->abap_hana_object_name_suffix act = lr_hta_object_cl_2->m_hot_object->abap_hana_object_name_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = lr_hta_object_cl_1->if_cts_hta_component~transport_object_name act = lr_hta_object_cl_2->if_cts_hta_component~transport_object_name ).
  ENDMETHOD.

  METHOD create_from_hana_key_long2.
    DATA: lr_hta_package   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl TYPE REF TO cl_cts_hta_object.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( 'sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal' ).

    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX'
                                i_hana_object_suffix = 'some_longer_suffix_this_time' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp =
                                                               `SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV` &&
                                                                                      `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'some_longer_suffix_this_time' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'some_longer_suffix_this_time' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp =
                                                                                      `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( lr_hta_object_cl->m_hot_object->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
  ENDMETHOD.

  METHOD create_from_hana_key_long_name.
    DATA: lr_hta_package   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl TYPE REF TO cl_cts_hta_object.

    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( 'hana.pack' ).

    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX'
                                i_hana_object_suffix = 'some_longer_suffix_this_time' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp =
                                                               `HANA.PACK                               ` &&
                                                                                        `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'some_longer_suffix_this_time' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'hana.pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'some_longer_suffix_this_time' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'HANA.PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp =
                                                                                      `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
  ENDMETHOD.

  METHOD create_from_hana_key_spaces.
    DATA: lr_hta_package   TYPE REF TO if_cts_hta_package,
          lr_hta_object_if TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl TYPE REF TO cl_cts_hta_object.

    "Test 1: create instance with 1 space in beginning
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( ` hana_pack` ).
    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = ' hana_obj_nam'
                                i_hana_object_suffix = ' hana_obj_suff' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp = 'HANA_PACK                               HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'hana_obj_nam' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'hana_obj_suff' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'hana_obj_nam' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'hana_obj_suff' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'HANA_PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).

    "Test 2: create instance with 1 space at the end
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( `hana_pack ` ).
    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = 'hana_obj_nam '
                                i_hana_object_suffix = 'hana_obj_suff ' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp = 'HANA_PACK                               HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'hana_obj_nam' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'hana_obj_suff' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'hana_obj_nam' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'hana_obj_suff' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'HANA_PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).

    "Test 3: create instance with different number of spaces in beginning and end
    lr_hta_package = cl_cts_hta_package=>create_instance_from_hana_key( `   hana_pack  ` ).
    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_hana_key(
                                i_hta_package = lr_hta_package
                                i_hana_object_name = '   hana_obj_nam  '
                                i_hana_object_suffix = '      hana_obj_suff   ' ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp = 'HANA_PACK                               HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = 'hana_obj_nam' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = 'hana_obj_suff' ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'hana_pack' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'hana_obj_nam' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'hana_obj_suff' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'HANA_PACK' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
  ENDMETHOD.

  METHOD create_from_obj_name.
    DATA: lr_hta_object_if   TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl   TYPE REF TO cl_cts_hta_object,
          lr_hta_package     TYPE REF TO if_cts_hta_package,
          ls_cts_hot_package TYPE cts_hot_package,
          ls_cts_hot_object  TYPE cts_hot_object.

    "1. prepare data in DB
    "1.1 in cts_hot_object
    ls_cts_hot_object-abap_hana_package_id = 'TMP.HTA.PACK1'.
    ls_cts_hot_object-abap_hana_object_name_suffix = 'OBJECT1.SUFFIX'.
    ls_cts_hot_object-abap_status          = cl_cts_hta_object=>co_active_version.
    ls_cts_hot_object-hana_package_id      = 'tmp.HTA.Pack1'.
    ls_cts_hot_object-hana_object_name      = 'Object1'.
    ls_cts_hot_object-hana_object_suffix      = 'suffix'.
    MODIFY cts_hot_object FROM ls_cts_hot_object.

    "1.2 in cts_hot_package
    ls_cts_hot_package-abap_hana_package_id = 'TMP.HTA.PACK1'.
    ls_cts_hot_package-abap_status          = cl_cts_hta_object=>co_active_version.
    ls_cts_hot_package-hana_package_id      = 'tmp.HTA.Pack1'.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    "2. test create instance
    lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.PACK1                           OBJECT1.SUFFIX' ).
    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.PACK1                           OBJECT1.SUFFIX'
                                                                         i_hta_package = lr_hta_package ).

    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_package_name exp = `tmp.HTA.Pack1` ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_name exp = `Object1` ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->object_key-hana_object_suffix exp = `suffix` ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~transport_object_name exp = `TMP.HTA.PACK1                           OBJECT1.SUFFIX` ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_if->if_cts_hta_component~component_type exp = ce_cts_hta_component_type=>ct_if_cts_hta_object ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_abap_status exp = cl_cts_hta_object=>co_active_version ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'tmp.HTA.Pack1' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'Object1' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'suffix' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'TMP.HTA.PACK1' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'OBJECT1' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package->hana_package_name exp = 'tmp.HTA.Pack1' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package->if_cts_hta_component~transport_object_name exp = 'TMP.HTA.PACK1' ).
  ENDMETHOD.

  METHOD create_from_obj_name_inactive.
    DATA: lr_hta_object_if   TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl   TYPE REF TO cl_cts_hta_object,
          lr_hta_package     TYPE REF TO if_cts_hta_package,
          ls_cts_hot_package TYPE cts_hot_package,
          ls_cts_hot_object  TYPE cts_hot_object.

    "1. prepare data in DB
    "1.1 in cts_hot_object
    ls_cts_hot_object-abap_hana_package_id = 'TMP.HTA.PACK1.INACTIVE'.
    ls_cts_hot_object-abap_hana_object_name_suffix = 'OBJECT1_INACTIVE.SUFFIX'.
    ls_cts_hot_object-abap_status          = cl_cts_hta_object=>co_inactive_version.
    ls_cts_hot_object-hana_package_id      = 'tmp.HTA.Pack1.inactive'.
    ls_cts_hot_object-hana_object_name      = 'Object1_inactive'.
    ls_cts_hot_object-hana_object_suffix      = 'suffix'.
    MODIFY cts_hot_object FROM ls_cts_hot_object.

    "1.2 in cts_hot_package
    ls_cts_hot_package-abap_hana_package_id = 'TMP.HTA.PACK1.INACTIVE'.
    ls_cts_hot_package-abap_status          = cl_cts_hta_object=>co_inactive_version.
    ls_cts_hot_package-hana_package_id      = 'tmp.HTA.Pack1.inactive'.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    "2. test create instance
    lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.PACK1.INACTIVE                  OBJECT1_INACTIVE.SUFFIX'
                                                                        i_abap_status = cl_cts_hta_object=>co_inactive_version ).
    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.PACK1.INACTIVE                  OBJECT1_INACTIVE.SUFFIX'
                                                                         i_hta_package = lr_hta_package
                                                                         i_abap_status = cl_cts_hta_object=>co_inactive_version ).

    cl_abap_unit_assert=>assert_equals( exp = `tmp.HTA.Pack1.inactive` act = lr_hta_object_if->object_key-hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `Object1_inactive` act = lr_hta_object_if->object_key-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = `suffix` act = lr_hta_object_if->object_key-hana_object_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = `TMP.HTA.PACK1.INACTIVE                  OBJECT1_INACTIVE.SUFFIX` act = lr_hta_object_if->if_cts_hta_component~transport_object_name ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_abap_status exp = cl_cts_hta_object=>co_inactive_version ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'tmp.HTA.Pack1.inactive' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'Object1_inactive' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'suffix' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'TMP.HTA.PACK1.INACTIVE' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'OBJECT1_INACTIVE' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package->hana_package_name exp = 'tmp.HTA.Pack1.inactive' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package->if_cts_hta_component~transport_object_name exp = 'TMP.HTA.PACK1.INACTIVE' ).

  ENDMETHOD.

  METHOD create_from_obj_name_inactive2.
    DATA: lr_hta_object_if   TYPE REF TO if_cts_hta_object,
          lr_hta_object_cl   TYPE REF TO cl_cts_hta_object,
          lr_hta_package     TYPE REF TO if_cts_hta_package,
          ls_cts_hot_package TYPE cts_hot_package,
          ls_cts_hot_object  TYPE cts_hot_object.

    "1. prepare data in DB
    "1.1 in cts_hot_object
    ls_cts_hot_object-abap_hana_package_id = 'TMP.HTA.PACK1.ACTIVE'.
    ls_cts_hot_object-abap_hana_object_name_suffix = 'OBJECT1_INACTIVE.SUFFIX'.
    ls_cts_hot_object-abap_status          = cl_cts_hta_object=>co_inactive_version.
    ls_cts_hot_object-hana_package_id      = 'tmp.HTA.Pack1.active'.
    ls_cts_hot_object-hana_object_name      = 'Object1_inactive'.
    ls_cts_hot_object-hana_object_suffix      = 'suffix'.
    MODIFY cts_hot_object FROM ls_cts_hot_object.

    "1.2 in cts_hot_package
    ls_cts_hot_package-abap_hana_package_id = 'TMP.HTA.PACK1.ACTIVE'.
    ls_cts_hot_package-abap_status          = cl_cts_hta_object=>co_active_version.
    ls_cts_hot_package-hana_package_id      = 'tmp.HTA.Pack1.active'.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    "2. test create instance
    lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.PACK1.ACTIVE                    OBJECT1_INACTIVE.SUFFIX'
                                                                        i_abap_status = cl_cts_hta_object=>co_active_version ).
    lr_hta_object_if = cl_cts_hta_object=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.PACK1.ACTIVE                    OBJECT1_INACTIVE.SUFFIX'
                                                                         i_hta_package = lr_hta_package
                                                                         i_abap_status = cl_cts_hta_object=>co_inactive_version ).

    cl_abap_unit_assert=>assert_equals( exp = `tmp.HTA.Pack1.active` act = lr_hta_object_if->object_key-hana_package_name ).
    cl_abap_unit_assert=>assert_equals( exp = `Object1_inactive` act = lr_hta_object_if->object_key-hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = `suffix` act = lr_hta_object_if->object_key-hana_object_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = `TMP.HTA.PACK1.ACTIVE                    OBJECT1_INACTIVE.SUFFIX` act = lr_hta_object_if->if_cts_hta_component~transport_object_name ).

    lr_hta_object_cl = CAST cl_cts_hta_object( lr_hta_object_if ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_abap_status exp = cl_cts_hta_object=>co_inactive_version ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_package_id exp = 'tmp.HTA.Pack1.active' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_name exp = 'Object1_inactive' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->hana_object_suffix exp = 'suffix' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_package_id exp = 'TMP.HTA.PACK1.ACTIVE' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->abap_hana_object_name_suffix exp = 'OBJECT1_INACTIVE' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hot_object->transport_object_name exp = lr_hta_object_if->if_cts_hta_component~transport_object_name ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package exp = lr_hta_package ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package->hana_package_name exp = 'tmp.HTA.Pack1.active' ).
    cl_abap_unit_assert=>assert_equals( act = lr_hta_object_cl->m_hta_package->if_cts_hta_component~transport_object_name exp = 'TMP.HTA.PACK1.ACTIVE' ).
  ENDMETHOD.

  METHOD create_from_obj_name_not_exist.
    DATA: lr_hta_object      TYPE REF TO if_cts_hta_object,
          lr_hta_package     TYPE REF TO if_cts_hta_package,
          ls_cts_hot_package TYPE cts_hot_package.

    "prepare package in DB
    ls_cts_hot_package-abap_hana_package_id = 'TMP.HTA.EXISTING.PACK'.
    ls_cts_hot_package-abap_status          = cl_cts_hta_object=>co_active_version.
    ls_cts_hot_package-hana_package_id      = 'tmp.HTA.existing.Pack'.
    MODIFY cts_hot_package FROM ls_cts_hot_package.

    "test create instance and expect exception for active version
    TRY.
        lr_hta_package = cl_cts_hta_package=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.EXISTING.PACK                   UNKNOWN_OBJECT'
                                                                            i_abap_status = cl_cts_hta_object=>co_active_version ).

        lr_hta_object = cl_cts_hta_object=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.EXISTING.PACK                   UNKNOWN_OBJECT'
                                                                          i_hta_package = lr_hta_package ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_cts_hta_not_found INTO DATA(lr_cx).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->if_t100_message~t100key exp = cx_cts_hta_not_found=>object_not_found_in_hta ).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_1 exp = 'TMP.HTA.EXISTING.PACK' ).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_2 exp = 'UNKNOWN_OBJECT' ).
    ENDTRY.

    "test create instance and expect exception for inactive version
    TRY.
        lr_hta_object = cl_cts_hta_object=>create_instance_from_obj_name( i_transport_object_name = 'TMP.HTA.EXISTING.PACK                   UNKNOWN_OBJECT'
                                                                          i_hta_package = lr_hta_package
                                                                          i_abap_status = cl_cts_hta_object=>co_inactive_version ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_cts_hta_not_found INTO lr_cx.
        cl_abap_unit_assert=>assert_equals( act = lr_cx->if_t100_message~t100key exp = cx_cts_hta_not_found=>object_not_found_in_hta ).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_1 exp = 'TMP.HTA.EXISTING.PACK' ).
        cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_2 exp = 'UNKNOWN_OBJECT' ).
    ENDTRY.
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
  METHOD if_cts_hot_hana_conn_internal~read_object_data_from_hana.
    IF gv_respond_with_data = abap_true AND i_cts_hot_object->abap_hana_package_id = 'TMP.HTA.PACK1' AND i_cts_hot_object->abap_hana_object_name_suffix = 'TEST_OBJECT.SUFFIX'.
      r_object_data-abap_hana_package_id = i_cts_hot_object->abap_hana_package_id.
      r_object_data-abap_hana_object_name_suffix = i_cts_hot_object->abap_hana_object_name_suffix.
      r_object_data-hana_package_id = i_cts_hot_object->hana_package_id.
      r_object_data-hana_object_name = i_cts_hot_object->hana_object_name.
      r_object_data-hana_object_suffix = i_cts_hot_object->hana_object_suffix.
      r_object_data-hana_object_version = 111.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_cts_hta_object_with_db DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS:
      "do not change constants, also used in ltd_hot_hana_connector_respond.
      co_hta_package_id_1         TYPE cts_hot_package_id VALUE 'TMP.HTA.PACK1',
      co_hta_package_name_1       TYPE cts_hot_hana_package_id VALUE 'tmp.HTA.Pack1', "if changed, also change in method get_sync_state_not_syncable_nc
      co_hta_object_name_1        TYPE cts_hot_hana_object_name VALUE 'TEST_OBJECT', "if changed, also change in method get_sync_state_not_syncable_nc
      co_hta_object_suffix_1      TYPE cts_hot_hana_object_suffix VALUE 'suffix', "if changed, also change in method get_sync_state_not_syncable_nc
      co_hta_object_name_suffix_1 TYPE cts_hot_object_name_suffix VALUE 'TEST_OBJECT.SUFFIX'.

    DATA:
      m_cut TYPE REF TO if_cts_hta_object.

    METHODS:
      "! prepare data in DB
      setup RAISING cx_static_check,
      "! delete data in DB
      teardown,

      "! Testing get_deploy_state in case the object is deployed (HOT_STATUS=A)
      get_deploy_state_deployed_a FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the object is deployed (HOT_STATUS=N)
      get_deploy_state_deployed_n FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the object is not deployed (HOT_STATUS=I)
      get_deploy_state_not_depl_i FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the object is not deployed (HOT_STATUS=D)
      get_deploy_state_not_depl_d FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state in case the object is not deployed (HOT_STATUS=F as not supported status)
      get_deploy_state_not_depl_f FOR TESTING RAISING cx_static_check,
      "! Testing get_deploy_state for unknown object in HTA
      get_deploy_state_unknown_obj FOR TESTING RAISING cx_static_check,

      "! Testing get_sync_state in case object is in sync with HANA (version is same in HANA and HTA na dhot_status is N)
      get_sync_state_in_sync_n FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object is in sync with HANA (version is same in HANA and HTA na dhot_status is A)
      get_sync_state_in_sync_a FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object does neither exist in HANA nor in HTA
      get_sync_state_in_sync_ne FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object is not in sync with HANA (version is different in HANA and HTA)
      get_sync_state_not_in_sync FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object is not in sync with HANA (object exist in HANA but not in HTA)<br/>nea=not existing in abap
      get_sync_state_not_in_sync_nea FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object is not in sync with HANA (object does not exist in HANA but in HTA)<br/>neh=not existing in hana
      get_sync_state_not_in_sync_neh FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object can not be synchronized because of wrong status in HTA ( 'I' )
      get_sync_state_not_syncable_i FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object can not be synchronized because of wrong status in HTA ( 'D')
      get_sync_state_not_syncable_d FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object can not be synchronized because of name conflict between HANA package name and HTA package name
      get_sync_state_not_syncabl_ncp FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object can not be synchronized because of name conflict between HANA object name and HTA object name
      get_sync_state_not_syncabl_nco FOR TESTING RAISING cx_static_check,
      "! Testing get_sync_state in case object can not be synchronized because of name conflict between HANA object suffix and HTA object suffix
      get_sync_state_not_syncabl_ncs FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS cl_cts_hta_object DEFINITION LOCAL FRIENDS ltcl_cts_hta_object_with_db.
CLASS ltcl_cts_hta_object_with_db IMPLEMENTATION.
  METHOD setup.
    DATA(lr_package) = NEW ltd_cts_hta_package( co_hta_package_name_1 ).
    m_cut = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package = lr_package
                                                      i_hana_object_name = co_hta_object_name_1
                                                      i_hana_object_suffix = co_hta_object_suffix_1 ).

    cl_cts_hta_object=>gr_hot_hana_connector = NEW ltd_hot_hana_connector_respond( ).

    "prepare data in DB - DO NOT CHANGE here because used in all tests with these values
    DATA ls_cts_hot_object TYPE cts_hot_object.
    ls_cts_hot_object-abap_hana_package_id = co_hta_package_id_1.
    ls_cts_hot_object-abap_hana_object_name_suffix = co_hta_object_name_suffix_1.
    ls_cts_hot_object-abap_status = cl_cts_hta_component=>co_active_version.
    ls_cts_hot_object-hot_status = if_cts_hot_db_access=>co_hot_status_new.
    ls_cts_hot_object-hana_package_id = co_hta_package_name_1.
    ls_cts_hot_object-hana_object_name = co_hta_object_name_1.
    ls_cts_hot_object-hana_object_suffix = co_hta_object_suffix_1.
    ls_cts_hot_object-hana_object_version = 111.

    MODIFY cts_hot_object FROM ls_cts_hot_object.
  ENDMETHOD.

  METHOD teardown.
    "delete data in DB
    DELETE FROM cts_hot_object WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.
  ENDMETHOD.

  METHOD get_deploy_state_deployed_a.
    "prepare data in DB (different to me->setup)
    UPDATE cts_hot_object SET hot_status = if_cts_hot_db_access=>co_hot_status_active WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_deployed_n.
    "execute business function and verify (hot_stats = N already set by me->setup( ))
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_not_depl_i.
    "prepare data in DB (different to me->setup)
    UPDATE cts_hot_object SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_not_depl_d.
    "prepare data in DB (different to me->setup)
    UPDATE cts_hot_object SET hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_not_depl_f.
    "prepare data in DB (different to me->setup)
    UPDATE cts_hot_object SET hot_status = 'F' WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function and verify
    cl_abap_unit_assert=>assert_equals( act = m_cut->get_deploy_state( ) exp = ce_cts_hta_deploy_state=>not_deployed ).
  ENDMETHOD.

  METHOD get_deploy_state_unknown_obj.
    "prepare data in DB (different to me->setup)
    DELETE FROM cts_hot_object WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function and verify
    TRY.
        m_cut->get_deploy_state( ).
        cl_abap_unit_assert=>fail( 'Expected exception cx_cts_hta_not_found was not thrown.' ).
      CATCH cx_cts_hta_not_found INTO DATA(lr_cx).
        IF lr_cx->if_t100_message~t100key = cx_cts_hta_not_found=>object_not_found_in_hta.
          "expected exception got
          cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_1 exp = co_hta_package_id_1 ).
          cl_abap_unit_assert=>assert_equals( act = lr_cx->message_variable_2 exp = co_hta_object_name_suffix_1 ).
        ELSE.
          cl_abap_unit_assert=>fail( 'Unexpected exception caught.' ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD get_sync_state_in_sync_n.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_in_sync_a.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "prepare DB
    UPDATE cts_hot_object SET hot_status = if_cts_hot_db_access=>co_hot_status_active WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_in_sync_ne.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    DATA(lr_package) = NEW ltd_cts_hta_package( co_hta_package_name_1 ).
    DATA(lr_object) = cl_cts_hta_object=>create_instance_from_hana_key( i_hta_package = lr_package
                                                      i_hana_object_name = 'UNKNOWN'
                                                      i_hana_object_suffix = co_hta_object_suffix_1 ).

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    UPDATE cts_hot_object SET hana_object_version = 123 WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_nea.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "prepare data in DB
    DELETE FROM cts_hot_object WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_in_sync_neh.
    DATA: lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    CAST ltd_hot_hana_connector_respond( cl_cts_hta_object=>gr_hot_hana_connector )->gv_respond_with_data = abap_false.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>not_in_sync ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 0 ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncabl_ncp.
    DATA: lt_reasons                  TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "prepare DB
    UPDATE cts_hot_object SET hana_package_id = 'tmp.HTA.PAck1' WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    DATA(lr_cx_cts_hta_name_conflict) = CAST cx_cts_hta_name_conflict( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->cts_hta_component exp = m_cut ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>package_name_conflict ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncabl_nco.
    DATA: lt_reasons                  TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "prepare DB
    UPDATE cts_hot_object SET hana_object_name = 'tEST_OBJECT' WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    data(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    data(lr_cx_cts_hta_name_conflict) = CAST cx_cts_hta_name_conflict( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->cts_hta_component exp = m_cut ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>object_name_conflict ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncabl_ncs.
    DATA: lt_reasons                  TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "prepare DB
    UPDATE cts_hot_object SET hana_object_suffix = 'Suffix' WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    data(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    data(lr_cx_cts_hta_name_conflict) = CAST cx_cts_hta_name_conflict( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->cts_hta_component exp = m_cut ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_name_conflict->if_t100_message~t100key exp = cx_cts_hta_name_conflict=>object_name_conflict ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_i.
    DATA: lt_reasons                 TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "Test 1: HOT status is inactive
    "prepare DB
    UPDATE cts_hot_object SET hot_status = if_cts_hot_db_access=>co_hot_status_inactive WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    DATA(lr_cx_cts_hta_wrong_status) = CAST cx_cts_hta_wrong_status( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->cts_hta_component exp = m_cut ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>object_requires_deployment ).
  ENDMETHOD.

  METHOD get_sync_state_not_syncable_d.
    DATA: lt_reasons                 TYPE if_cts_hta_types=>ty_cx_cts_htas.

    "Test 1: HOT status is inactive
    "prepare DB
    UPDATE cts_hot_object SET hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted WHERE abap_hana_package_id = co_hta_package_id_1 AND abap_hana_object_name_suffix = co_hta_object_name_suffix_1.

    "execute business function
    DATA(lr_sync_state) = m_cut->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).

    "verify results
    cl_abap_unit_assert=>assert_equals( act = lr_sync_state exp = ce_cts_hta_sync_state=>can_not_be_synchronized ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_reasons ) exp = 1 msg = 'Reason for can_not_be_synchronized missing' ).
    DATA(lr_cx_cts_hta_wrong_status) = CAST cx_cts_hta_wrong_status( lt_reasons[ 1 ] ). "cast fails if wrong type
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->cts_hta_component exp = m_cut ).
    cl_abap_unit_assert=>assert_equals( act = lr_cx_cts_hta_wrong_status->if_t100_message~t100key exp = cx_cts_hta_wrong_status=>object_requires_deployment ).
  ENDMETHOD.

ENDCLASS.