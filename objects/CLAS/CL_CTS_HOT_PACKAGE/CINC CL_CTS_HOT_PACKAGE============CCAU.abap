*"* use this source file for your ABAP unit test classes
CLASS ltcl_cts_hot_package DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Tests creation of a package with name 'hana_pack'
      test_create_cts_hot_package FOR TESTING RAISING cx_static_check,
      "! Tests creaton of package objects with package name containing double quotes, dash and camel case
      test_create_instance FOR TESTING RAISING cx_static_check,
      "! Testing creation of packages with leading / ending spaces. They are to be ignored/cut off
      test_create_instance_spaces FOR TESTING RAISING cx_static_check,
      "! Testing creation of package object for a package name with length of 40 chars ( no hashing )
      test_create_instance_40_chars FOR TESTING RAISING cx_static_check,
      "! Testing creation of package object for a package name with more than 40 chars ( with hashing )
      test_create_instance_41_chars FOR TESTING RAISING cx_static_check,
      "! Testing 3 cases for long names (with hashing), 'normal' package name, package name with double quotes and camel case
      test_create_instance_long_name FOR TESTING RAISING cx_static_check,
      "! Testing that if hana package name only differes in case they create same abap_abap_package_ids
      test_create_camelcase_conflict FOR TESTING RAISING cx_static_check,

      "! Tests creation of a package instance with name 'hana_pack'
      test_cts_hot_package_w_objname FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_cts_hot_package IMPLEMENTATION.
  METHOD test_cts_hot_package_w_objname.  "Stephan/Daniel
    DATA: ls_cts_hot_package TYPE cts_hot_package.

    ls_cts_hot_package-abap_hana_package_id = 'D019419_1'.
    ls_cts_hot_package-abap_status          = 'I'.
    ls_cts_hot_package-hot_status           = 'I'.
    ls_cts_hot_package-hana_package_id      = 'd019419_1'.
    ls_cts_hot_package-hana_pack_orig_lang  = 'de_DE'.

    INSERT cts_hot_package FROM ls_cts_hot_package.
    IF sy-subrc = 4.
      MODIFY cts_hot_package FROM ls_cts_hot_package.
    ENDIF.

    DATA(obj) = cl_cts_hot_package=>create_instance_from_objname( iv_objname = `D019419_1` iv_abap_status = 'I' ).

    cl_abap_unit_assert=>assert_equals( exp = `d019419_1` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `D019419_1` act = obj->abap_hana_package_id ).
  ENDMETHOD.

  METHOD test_create_cts_hot_package.
    DATA(obj) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `hana_pack` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = obj->abap_hana_package_id ).
  ENDMETHOD.

  METHOD test_create_instance.
    data(obj) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `sap.hana-app.CuAn.ANA_HRF.".settings"` ).

    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.CuAn.ANA_HRF.".settings"` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HANA-APP.CUAN.ANA_HRF.".SETTINGS"` act = obj->abap_hana_package_id ).
  ENDMETHOD.

  METHOD test_create_instance_40_chars.
    DATA(obj) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `test.maximum.length.package.without.hash` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( obj->hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( obj->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = `test.maximum.length.package.without.hash` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `TEST.MAXIMUM.LENGTH.PACKAGE.WITHOUT.HASH` act = obj->abap_hana_package_id ).
  ENDMETHOD.

  METHOD test_create_instance_41_chars.
    DATA(obj) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `test.41.chars.length.package.with.hashing` ).

    cl_abap_unit_assert=>assert_equals( exp = 41 act = strlen( obj->hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( obj->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = `test.41.chars.length.package.with.hashing` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `TEST.41;F14QH8278T3577BCA7H915CBLAU85AN8` act = obj->abap_hana_package_id ).
  ENDMETHOD.

  METHOD test_create_instance_long_name.
    "Test 1: 'normal' package name
    DATA(obj) = cl_cts_hot_package=>create_instance( iv_hana_package_id = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( obj->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV` act = obj->abap_hana_package_id ).

    "Test 2: package name with double quotes
    obj = cl_cts_hot_package=>create_instance( iv_hana_package_id = `sap.hana-app.cuan.cpred.hrf.ANA_HRF.".settings"` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( obj->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.cuan.cpred.hrf.ANA_HRF.".settings"` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;G1IKOS8LD2BN5RC8BT2MKSU8AN043I5G` act = obj->abap_hana_package_id ).

    "Test 3: package name with camel case
    obj = cl_cts_hot_package=>create_instance( iv_hana_package_id = `sap.hana-app.mo.public.logic.HandleEmailSending` ).

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( obj->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.mo.public.logic.HandleEmailSending` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;SM04LG9F98117NJCPJ3805OL94VUHTTK` act = obj->abap_hana_package_id ).

  ENDMETHOD.

  METHOD test_create_camelcase_conflict.
    "Test 1:  package id < 40 chars
    DATA(obj1) = cl_cts_hot_package=>create_instance( iv_hana_package_id = 'sap.com.package' ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.com.package` act = obj1->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.COM.PACKAGE` act = obj1->abap_hana_package_id ).

    DATA(obj2) = cl_cts_hot_package=>create_instance( iv_hana_package_id = 'Sap.Com.pacKage' ).
    cl_abap_unit_assert=>assert_equals( exp = `Sap.Com.pacKage` act = obj2->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.COM.PACKAGE` act = obj2->abap_hana_package_id ).

    cl_abap_unit_assert=>assert_equals( exp = obj1->abap_hana_package_id act = obj2->abap_hana_package_id ).

    "Test 2:  package id > 40 chars
    obj1 = cl_cts_hot_package=>create_instance( iv_hana_package_id = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal` act = obj1->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV` act = obj1->abap_hana_package_id ).

    obj2 = cl_cts_hot_package=>create_instance( iv_hana_package_id = `sap.HANA-APP.cuan.cPred.demo.insurance.DataFoundation.internal` ).
    cl_abap_unit_assert=>assert_equals( exp = `sap.HANA-APP.cuan.cPred.demo.insurance.DataFoundation.internal` act = obj2->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV` act = obj2->abap_hana_package_id ).

    cl_abap_unit_assert=>assert_equals( exp = obj1->abap_hana_package_id act = obj2->abap_hana_package_id ).
  ENDMETHOD.

  METHOD test_create_instance_spaces.
    " 1 space in beginning
    DATA(obj) = cl_cts_hot_package=>create_instance( iv_hana_package_id = ` hana_pack` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = obj->abap_hana_package_id ).

    " 3 spaces in beginning
    obj = cl_cts_hot_package=>create_instance( iv_hana_package_id = `   hana_pack` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = obj->abap_hana_package_id ).

    " 1 space at the end
    obj = cl_cts_hot_package=>create_instance( iv_hana_package_id = `hana_pack ` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = obj->abap_hana_package_id ).

    " 3 spaces at the end
    obj = cl_cts_hot_package=>create_instance( iv_hana_package_id = `hana_pack   ` ).

    cl_abap_unit_assert=>assert_equals( exp = `hana_pack` act = obj->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = `HANA_PACK` act = obj->abap_hana_package_id ).

  ENDMETHOD.

ENDCLASS.