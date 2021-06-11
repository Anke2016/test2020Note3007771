*"* use this source file for your ABAP unit test classes
CLASS ltcl_cts_hot_object DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      "! Testing creation of simple object with create_instance and create_instance2
      test_create_cts_hot_object FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with create_instance and create_instance2
      "! but using spaces in front of object name, suffix and package name.
      test_create_instance_spaces FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with object name and suffix length 70
      "! (max supportedl length without hashing the name)
      test_create_instance_70_chars FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with object name and suffix length = 71
      "! First length where hasing should take place
      test_create_instance_71_chars FOR TESTING RAISING cx_static_check,
      "! Testing creation of simple object with object name and suffix length more than 71
      "! so that hashing needs to be used. Also suffix is longer than 20 chars but maximum of
      "! 20 chars should be used, rest to cut off
      test_create_instance_long_name FOR TESTING RAISING cx_static_check,
      "! Testing that 2 objects with only difference in object name case are conflicting
      test_create_camelcase_conflict FOR TESTING RAISING cx_static_check,
      "! Testing the creation of an object that has a package name that needs hashing and
      "! that has an object name that needs hashing
      test_long_object_long_package  FOR TESTING RAISING cx_static_check,


      "! Helper method for testing create_instance and create_instance2 together including verification
      "! In are the inpout parameter and exp are the expected results in verification
      "! Exporting eo_object created with create_instance and eo_object2 created with create_instance2 for
      "! further tests
      create_instance_and_verify
        IMPORTING
          iv_in_pack                TYPE string
          iv_in_obj_name            TYPE string
          iv_in_obj_suffix          TYPE string
          iv_exp_pack               TYPE string
          iv_exp_obj_name           TYPE string
          iv_exp_obj_suffix         TYPE string
          iv_exp_abap_hana_pack     TYPE string
          iv_exp_abap_hana_obj      TYPE string
          iv_exp_transport_obj_name TYPE string
        EXPORTING
          eo_object                 TYPE REF TO cl_cts_hot_object_v1
          eo_object2                TYPE REF TO cl_cts_hot_object_v1
        RAISING
          cx_hana_object_transport.

ENDCLASS.

CLASS cl_cts_hot_object_v1 DEFINITION LOCAL FRIENDS ltcl_cts_hot_object.
CLASS ltcl_cts_hot_object IMPLEMENTATION.

  METHOD test_create_cts_hot_object.
    create_instance_and_verify( iv_in_pack  = `hana_pack` iv_in_obj_name  = 'hana_obj_nam' iv_in_obj_suffix  = 'hana_obj_suff'
                                iv_exp_pack = 'hana_pack' iv_exp_obj_name = 'hana_obj_nam' iv_exp_obj_suffix = 'hana_obj_suff'
                                iv_exp_abap_hana_pack = 'HANA_PACK'
                                iv_exp_abap_hana_obj = 'HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF'
                                iv_exp_transport_obj_name = 'HANA_PACK                               HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).
  ENDMETHOD.

  METHOD test_create_instance_spaces.
    "Test 1: create instance with 1 space in beginning
    create_instance_and_verify( iv_in_pack  = ` hana_pack` iv_in_obj_name  = ' hana_obj_nam' iv_in_obj_suffix  = ' hana_obj_suff'
                                iv_exp_pack = 'hana_pack'  iv_exp_obj_name = 'hana_obj_nam'  iv_exp_obj_suffix = 'hana_obj_suff'
                                iv_exp_abap_hana_pack = 'HANA_PACK'
                                iv_exp_abap_hana_obj = `HANA_OBJ_NAM` && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF'
                                iv_exp_transport_obj_name = 'HANA_PACK                               HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).

    "Test 2: create instance with 1 space at the end
    create_instance_and_verify( iv_in_pack  = `hana_pack ` iv_in_obj_name  = 'hana_obj_nam ' iv_in_obj_suffix  = 'hana_obj_suff '
                                iv_exp_pack = 'hana_pack'  iv_exp_obj_name = 'hana_obj_nam'  iv_exp_obj_suffix = 'hana_obj_suff'
                                iv_exp_abap_hana_pack = 'HANA_PACK'
                                iv_exp_abap_hana_obj = `HANA_OBJ_NAM` && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF'
                                iv_exp_transport_obj_name = 'HANA_PACK                               HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).

    "Test 3: create instance with different number of spaces in beginning and end
    create_instance_and_verify( iv_in_pack  = `   hana_pack  ` iv_in_obj_name  = '   hana_obj_nam  ' iv_in_obj_suffix  = '      hana_obj_suff   '
                                iv_exp_pack = 'hana_pack'      iv_exp_obj_name = 'hana_obj_nam'      iv_exp_obj_suffix = 'hana_obj_suff'
                                iv_exp_abap_hana_pack = 'HANA_PACK'
                                iv_exp_abap_hana_obj = `HANA_OBJ_NAM` && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF'
                                iv_exp_transport_obj_name = 'HANA_PACK                               HANA_OBJ_NAM' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'HANA_OBJ_SUFF' ).

  ENDMETHOD.

  METHOD test_create_instance_70_chars.
    create_instance_and_verify( EXPORTING
                                  iv_in_pack  = `hana_pack` iv_in_obj_name  = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_70_chars_INCLUDING_SUFFIX' iv_in_obj_suffix  = 'some_suff'
                                  iv_exp_pack = 'hana_pack' iv_exp_obj_name = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_70_chars_INCLUDING_SUFFIX' iv_exp_obj_suffix = 'some_suff'
                                  iv_exp_abap_hana_pack = 'HANA_PACK'
                                  iv_exp_abap_hana_obj = `THIS_IS_SOME_LONG_OBJECT_NAME_WITH_70_CHARS_INCLUDING_SUFFIX` && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SOME_SUFF'
                                  iv_exp_transport_obj_name = 'HANA_PACK                               THIS_IS_SOME_LONG_OBJECT_NAME_WITH_70_CHARS_INCLUDING_SUFFIX' && cl_cts_hot_object_v1=>co_object_name_suffix_delimitr && 'SOME_SUFF'
                                IMPORTING
                                  eo_object = DATA(eo_object)
                                  eo_object2 = DATA(eo_object2) ) .

    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object2->abap_hana_object_name_suffix ) ).
  ENDMETHOD.

  METHOD test_create_instance_71_chars.
    create_instance_and_verify( EXPORTING
                                  iv_in_pack  = `hana_pack` iv_in_obj_name  = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_71_chars_INCLUDING_SUFFIXX' iv_in_obj_suffix  = 'some_suff'
                                  iv_exp_pack = 'hana_pack' iv_exp_obj_name = 'THIS_IS_SOME_LONG_OBJECT_NAME_with_71_chars_INCLUDING_SUFFIXX' iv_exp_obj_suffix = 'some_suff'
                                  iv_exp_abap_hana_pack = 'HANA_PACK'
                                  iv_exp_abap_hana_obj = `THIS_IS_SOME_LONG_OBJECT_NA` && cl_cts_hot_package=>co_hash_seperator && `IA3DCFITIBM1PAVO3ACOO6ELRR3GIA1L` && cl_cts_hot_package=>co_hash_seperator && 'SOME_SUFF'
                                  iv_exp_transport_obj_name = `HANA_PACK                               ` &&
                                                                                          'THIS_IS_SOME_LONG_OBJECT_NA' && cl_cts_hot_package=>co_hash_seperator && `IA3DCFITIBM1PAVO3ACOO6ELRR3GIA1L` && cl_cts_hot_package=>co_hash_seperator && 'SOME_SUFF'
                                IMPORTING
                                  eo_object = DATA(eo_object)
                                  eo_object2 = DATA(eo_object2) ) .

    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object2->abap_hana_object_name_suffix ) ).
  ENDMETHOD.

  METHOD test_create_instance_long_name.
    create_instance_and_verify( EXPORTING
                                  iv_in_pack  = `hana_pack` iv_in_obj_name  = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' iv_in_obj_suffix  = 'some_longer_suffix_this_time'
                                  iv_exp_pack = 'hana_pack' iv_exp_obj_name = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' iv_exp_obj_suffix = 'some_longer_suffix_this_time'
                                  iv_exp_abap_hana_pack = 'HANA_PACK'
                                  iv_exp_abap_hana_obj = `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                  iv_exp_transport_obj_name = `HANA_PACK                               ` &&
                                                                                          `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                IMPORTING
                                  eo_object = DATA(eo_object)
                                  eo_object2 = DATA(eo_object2) ) .

    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object2->abap_hana_object_name_suffix ) ).
  ENDMETHOD.

  METHOD test_create_camelcase_conflict.
    create_instance_and_verify( EXPORTING
                                  iv_in_pack  = `hana_pack` iv_in_obj_name  = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' iv_in_obj_suffix  = 'some_longer_suffix_this_time'
                                  iv_exp_pack = 'hana_pack' iv_exp_obj_name = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' iv_exp_obj_suffix = 'some_longer_suffix_this_time'
                                  iv_exp_abap_hana_pack = 'HANA_PACK'
                                  iv_exp_abap_hana_obj = `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                  iv_exp_transport_obj_name = `HANA_PACK                               ` &&
                                                                                          'THIS_IS_SOME_LON' && cl_cts_hot_package=>co_hash_seperator && 'K6QUTDQL0B4U8EDM8KEDQ01F79DE700T' && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                IMPORTING
                                  eo_object = DATA(eo1_object)
                                  eo_object2 = DATA(eo1_object2) ) .

    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo1_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo1_object2->abap_hana_object_name_suffix ) ).

    create_instance_and_verify( EXPORTING
                                  iv_in_pack  = `hana_pack` iv_in_obj_name  = 'this_IS_SOME_long_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' iv_in_obj_suffix  = 'some_Longer_SUFFIX_this_TiMe'
                                  iv_exp_pack = 'hana_pack' iv_exp_obj_name = 'this_IS_SOME_long_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX' iv_exp_obj_suffix = 'some_Longer_SUFFIX_this_TiMe'
                                  iv_exp_abap_hana_pack = 'HANA_PACK'
                                  iv_exp_abap_hana_obj = `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                  iv_exp_transport_obj_name = `HANA_PACK                               ` &&
                                                                                          `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                IMPORTING
                                  eo_object = DATA(eo2_object)
                                  eo_object2 = DATA(eo2_object2) ) .

    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo2_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo2_object2->abap_hana_object_name_suffix ) ).

    " verify all objects have same abap_hana_object_name_suffix
    cl_abap_unit_assert=>assert_equals( exp = eo1_object->abap_hana_object_name_suffix act = eo1_object2->abap_hana_object_name_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = eo1_object->abap_hana_object_name_suffix act = eo2_object->abap_hana_object_name_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = eo1_object->abap_hana_object_name_suffix act = eo2_object2->abap_hana_object_name_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = eo1_object2->abap_hana_object_name_suffix act = eo2_object->abap_hana_object_name_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = eo1_object2->abap_hana_object_name_suffix act = eo2_object2->abap_hana_object_name_suffix ).
  ENDMETHOD.

  METHOD test_long_object_long_package.
    create_instance_and_verify( EXPORTING
                                  iv_in_pack = `sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal`
                                  iv_in_obj_name = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX'
                                  iv_in_obj_suffix = 'some_longer_suffix_this_time'
                                  iv_exp_pack = 'sap.hana-app.cuan.cpred.demo.insurance.datafoundation.internal'
                                  iv_exp_obj_name = 'THIS_IS_SOME_LONG_OBJECT-NAME_with-much_more_THAN_71_chars_INCLUDING_SUFFIXX'
                                  iv_exp_obj_suffix = 'some_longer_suffix_this_time'
                                  iv_exp_abap_hana_pack = 'SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV'
                                  iv_exp_abap_hana_obj = `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                  iv_exp_transport_obj_name = 'SAP.HAN;GLPA8FLUOCR3U9482RQHSVBG4J70JANV' &&
                                                                                          `THIS_IS_SOME_LON` && cl_cts_hot_package=>co_hash_seperator && `K6QUTDQL0B4U8EDM8KEDQ01F79DE700T` && cl_cts_hot_package=>co_hash_seperator && 'GER_SUFFIX_THIS_TIME'
                                IMPORTING
                                  eo_object = DATA(eo_object)
                                  eo_object2 = DATA(eo_object2) ) .

    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( eo_object->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = 40 act = strlen( eo_object2->abap_hana_package_id ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object->abap_hana_object_name_suffix ) ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = strlen( eo_object2->abap_hana_object_name_suffix ) ).
  ENDMETHOD.

  METHOD create_instance_and_verify.
    "use create_instance
    eo_object = cl_cts_hot_object_v1=>create_instance(
                   iv_hana_package_id    = iv_in_pack
                   iv_hana_object_name   = iv_in_obj_name
                   iv_hana_object_suffix = iv_in_obj_suffix
               ).

    cl_abap_unit_assert=>assert_equals( exp = iv_exp_pack act = eo_object->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_obj_name act = eo_object->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_obj_suffix act = eo_object->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = iv_exp_abap_hana_pack act = eo_object->abap_hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_abap_hana_obj act = eo_object->abap_hana_object_name_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_transport_obj_name act = eo_object->transport_object_name ).

    "use create_instance2
    DATA(pack) = cl_cts_hot_package=>create_instance( iv_hana_package_id = iv_in_pack ).
    eo_object2 = cl_cts_hot_object_v1=>create_instance2(
                   io_cts_hot_package       = pack
                   iv_hana_object_name   = iv_in_obj_name
                   iv_hana_object_suffix = iv_in_obj_suffix
               ).

    cl_abap_unit_assert=>assert_equals( exp = iv_exp_pack act = eo_object2->hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_obj_name act = eo_object2->hana_object_name ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_obj_suffix act = eo_object2->hana_object_suffix ).

    cl_abap_unit_assert=>assert_equals( exp = iv_exp_abap_hana_pack act = eo_object2->abap_hana_package_id ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_abap_hana_obj act = eo_object2->abap_hana_object_name_suffix ).
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_transport_obj_name act = eo_object2->transport_object_name ).
  ENDMETHOD.

ENDCLASS.