CLASS ltd_lcl_helper DEFINITION FINAL FOR TESTING INHERITING FROM lcl_helper.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_hana_repo_data,
             package_id    TYPE string,
             object_name   TYPE string,
             object_suffix TYPE string,
             version       TYPE i,
             cdata         TYPE string,
             bdata         TYPE xstring,
           END OF ty_s_hana_repo_data,
           ty_t_hana_repo_data TYPE STANDARD TABLE OF ty_s_hana_repo_data.

    METHODS:
      read_hana_object_data REDEFINITION,
      read_hana_object_version REDEFINITION.

    DATA:
      "! Test data to be used in read_hana_object_data and read_hana_object_version
      mt_hana_repo_data          TYPE ty_t_hana_repo_data,
      "! Exception to be raised in read_hana_object_version, independent if data exists in mt_hana_repo_data or not
      mr_exc_read_object_version TYPE REF TO cx_hana_object_transport,
      "! Exception to be raised in read_hana_object_data, independent if data exists in mt_hana_repo_data or not
      mr_exc_read_object_data    TYPE REF TO cx_hana_object_transport.
ENDCLASS.

CLASS ltd_lcl_helper IMPLEMENTATION.
  METHOD read_hana_object_data.
    DATA ls_hana_repo_data TYPE ty_s_hana_repo_data.

    IF mr_exc_read_object_data IS BOUND.
      RAISE EXCEPTION mr_exc_read_object_data.
    ENDIF.

    ls_hana_repo_data = VALUE #( mt_hana_repo_data[ package_id = iv_package_id object_name = iv_object_name object_suffix = iv_object_suffix ] OPTIONAL ).

    IF mt_hana_repo_data IS NOT INITIAL AND ls_hana_repo_data IS INITIAL.
      cl_abap_unit_assert=>fail( |Call to read_hane_object_data not expected for object { iv_package_id }.{ iv_object_name }.{ iv_object_suffix }| ).
    ENDIF.
    ev_cdata = ls_hana_repo_data-cdata.
    ev_bdata = ls_hana_repo_data-bdata.
  ENDMETHOD.

  METHOD read_hana_object_version.
    DATA ls_hana_repo_data TYPE ty_s_hana_repo_data.

    IF mr_exc_read_object_version IS BOUND.
      RAISE EXCEPTION mr_exc_read_object_version.
    ENDIF.

    ls_hana_repo_data = VALUE #( mt_hana_repo_data[ package_id = iv_package_id object_name = iv_object_name object_suffix = iv_object_suffix ] OPTIONAL ).

    IF mt_hana_repo_data IS NOT INITIAL AND ls_hana_repo_data IS INITIAL.
      cl_abap_unit_assert=>fail( |Call to read_hana_object_version not expected for object { iv_package_id }.{ iv_object_name }.{ iv_object_suffix }| ).
    ENDIF.

    rv_version = ls_hana_repo_data-version.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_cts_hot_upg DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
      ty_t_sprot_u TYPE if_cts_hot_logger=>ty_t_sprot_u.

    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown RAISING cx_static_check.

    CLASS-DATA:
      gr_osql_env TYPE REF TO if_osql_test_environment.

    METHODS:
      setup RAISING cx_static_check,
      teardown RAISING cx_static_check,
      "! Ensure that get_instance is initializing logger and helper
      get_instance FOR TESTING RAISING cx_static_check,
      "! Test without any hana repository package and without any hana repository object in HTA
      check_consistency_no_data_hta FOR TESTING RAISING cx_static_check,
      "! Test with packages with prework required flag and prework done
      check_consistency_pkgs_w_pw_t FOR TESTING RAISING cx_static_check,
      "! Test with 1 package with prework required flag and no entry in prework done table
      check_consistency_pkgs_w_pw_f1 FOR TESTING RAISING cx_static_check,
      "! Test with 1 package with prework required flag and an entry in prework done table with space
      check_consistency_pkgs_w_pw_f2 FOR TESTING RAISING cx_static_check,
      "! Test with packages without prework required flag
      check_consistency_pkgs_wo_pw FOR TESTING RAISING cx_static_check,
      "! Testing a complex scenario for prework required settings:
      "! <ul><li>2 active packages (ABAP_STATUS=A) without prework required flag, having hot_activation_mode=A and space</li>
      "! <li>1 inactive package (ABAP_STATUS=I) without prework flag, hot_activation_mode=A</li>
      "! <li>1 inactive package (ABAP_STATUS=I) with prework required and prework not done</li>
      "! <li>3 active packages with prework required flag and 3 different values
      "! in cts_hot_prework (not existing, 'X', space)</li></ul>
      check_consistency_pkgs_pw_cplx FOR TESTING RAISING cx_static_check,
      "! Testing packages that are deployed (ABAP_STATUS=A and HOT_STATUS=A or N) and 1 package with ABAP_STATUS=I and HOT_STATUS=I which should be ignored
      check_consistency_pkgs_dep FOR TESTING RAISING cx_static_check,
      "! Testing packages that are not deployed (ABAP_STAUTS=A and HOT_STATUS=I/D/E/Z)
      check_consistency_pkgs_not_dep FOR TESTING RAISING cx_static_check,
      "! Testing objects that are deployed (ABAP_STATUS=A and HOT_STATUS=A or N) and 1 object with ABAP_STATUS=I and HOT_STATUS=I which should be ignored
      check_consistency_objs_dep FOR TESTING RAISING cx_static_check,
      "! Testing objects that are not deployed (ABAP_STAUTS=A and HOT_STATUS=I/D/E/Z)
      check_consistency_objs_not_dep FOR TESTING RAISING cx_static_check,
      "! Testing objects that are active in HTA (HOT_STATUS=N and A), have different version in HANA Repo but have same cdata/bdata compared with HANA Repo --> consider as consistent
      check_consistency_objs_diff_v1 FOR TESTING RAISING cx_static_check,
      "! Testing objects that are active in HTA (HOT_STATUS=N and A), have different version in HANA Repo but have different cdata/bdata compared with HANA Repo --> report as inconsistent
      check_consistency_objs_diff_v2 FOR TESTING RAISING cx_static_check,
      "! Testing object that is active in HTA (HOT_STATUS=N and A), but does not exist in HANA Repo
      check_consistency_objs_diff_v3 FOR TESTING RAISING cx_static_check,
      "! Testing objects that are active in HTA (HOT_STATUS=N and A) and have same version in HANA Repo
      check_consistency_objs_same_v FOR TESTING RAISING cx_static_check,
      "! Testing that packages/objects are not logged several times if several issues are found:<br/>
      "! <ul><li>1 package that has missing prework and is not deployed</li>
      "! <li>1 object that is not deployed because it is part of the package with missing prework</li>
      "! <li>1 object that is on a package without prework but not deployed and has different version in HANA</li></ul>
      check_consistency_several_issu FOR TESTING RAISING cx_static_check,
      "! Testing exception handling. Exception is thrown when reading object_version from HANA
      check_consistency_hana_exc_1 FOR TESTING RAISING cx_static_check,
      "! Testing exception handling. Exception is thrown when reading object_data from HANA
      check_consistency_hana_exc_2 FOR TESTING RAISING cx_static_check,

      "! Insert test data from mt_hot_packages, mt_hot_objects and mt_hot_prework to test double or to DB, implementation is different, depending on basis release
      insert_test_data,

      "! Verify logs for method check_consistency. By default 401 and 402 are always expected.
      "! @parameter it_exp_logs_missing_prework | Expected log lines for missing prework.<br/>
      "!                                          If not initial, 404 is inserted before these entries.<br/>
      "!                                          If initial, 403 is used.
      "! @parameter it_exp_logs_not_deployed_pkgs | Expected log lines for not deployed packages.<br/>
      "!                                            If not initial, 406 is inserted before these entries.<br/>
      "!                                            If initial, 405 is used.
      "! @parameter it_exp_logs_not_deployed_objs | Expected log lines for not deployed objects.<br/>
      "!                                            If not initial, 408 is inserted before these entries.<br/>
      "!                                            If initial, 407 is used.
      "! @parameter it_exp_logs_different_obj_vers | Expected log lines for objects with different version in HTA and HANA Repo.<br/>
      "!                                             If not initial, 411 is inserted before these entries.<br/>
      "!                                             If initial, 410 is used.
      "! @parameter it_Exp_logs_exc_check_obj_vers | Expected log lines for exception during read_hana_object_version or read_hana_object_data
      "! @parameter iv_result | rv_result of call to undeploy_newly_delivered_nhdus for message 402
      verify_logs_check_consistency
        IMPORTING
          it_exp_logs_missing_prework    TYPE ty_t_sprot_u OPTIONAL
          it_exp_logs_not_deployed_pkgs  TYPE ty_t_sprot_u OPTIONAL
          it_exp_logs_not_deployed_objs  TYPE ty_t_sprot_u OPTIONAL
          it_exp_logs_different_obj_vers TYPE ty_t_sprot_u OPTIONAL
          it_exp_logs_exc_check_obj_vers TYPE ty_t_sprot_u OPTIONAL
          iv_result                      TYPE abap_bool DEFAULT abap_true.

    DATA:
      mr_logger_double TYPE REF TO cl_cts_hot_logger_memory,
      mr_helper_double TYPE REF TO ltd_lcl_helper,
      mr_cut           TYPE REF TO cl_cts_hot_upg,
      mt_hot_packages  TYPE STANDARD TABLE OF cts_hot_package,
      mt_hot_objects   TYPE STANDARD TABLE OF cts_hot_object,
      mt_hot_prework   TYPE STANDARD TABLE OF cts_hot_prework.
ENDCLASS.

CLASS cl_cts_hot_upg DEFINITION LOCAL FRIENDS ltcl_cts_hot_upg.
CLASS ltcl_cts_hot_upg IMPLEMENTATION.
  METHOD class_setup.
    IF sy-dbsys <> 'HDB'.
      cl_abap_unit_assert=>abort( msg = 'Test can only be run on ABAP systems on HDB' ).
    ENDIF.
    gr_osql_env = cl_osql_test_environment=>create( VALUE #( ( 'CTS_HOT_OBJECT' )
                                                             ( 'CTS_HOT_PACKAGE' )
                                                             ( 'CTS_HOT_PREWORK' )
                                                             ( 'TADIR' )
                                                             ( 'TDEVC' )
                                                             ( 'DF14L' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    gr_osql_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    DATA: lt_tadir TYPE STANDARD TABLE OF tadir,
          lt_tdevc TYPE STANDARD TABLE OF tdevc,
          lt_df14l TYPE STANDARD TABLE OF df14l.

    mr_logger_double = CAST cl_cts_hot_logger_memory( cl_cts_hot_logger_memory=>create_instance( ) ).
    mr_helper_double = NEW ltd_lcl_helper( ).
    mr_cut = NEW cl_cts_hot_upg( ir_logger = NEW lcl_logger( mr_logger_double )
                                 ir_helper = mr_helper_double ).

    "fill global tables for AKH retrieval for some of the test objects/packages only
    lt_tadir = VALUE #( ( pgmid = 'R3TR' object = 'HOTA' obj_name = 'COM.AUNIT.PACK' devclass = 'Z_HTA_AUNIT_ABAP_PACK' )
                        ( pgmid = 'R3TR' object = 'HOTA' obj_name = 'COM.AUNIT.PACK.I' devclass = 'Z_HTA_AUNIT_ABAP_PACK' ) ).
    gr_osql_env->insert_test_data( lt_tadir ).

    lt_tdevc = VALUE #( ( component = 'Z_HTA_AUNIT_COMP' devclass = 'Z_HTA_AUNIT_ABAP_PACK' ) ).
    gr_osql_env->insert_test_data( lt_tdevc ).

    lt_df14l = VALUE #( ( ps_posid = 'BC-CTS-HTA-AUNIT' fctr_id = 'Z_HTA_AUNIT_COMP' as4local = 'A' ) ).
    gr_osql_env->insert_test_data( lt_df14l ).
  ENDMETHOD.

  METHOD teardown.
    gr_osql_env->clear_doubles( ).
  ENDMETHOD.

  METHOD get_instance.
    DATA(lr_hot_upg) = cl_cts_hot_upg=>get_instance( ).

    cl_abap_unit_assert=>assert_bound( lr_hot_upg ).
    cl_abap_unit_assert=>assert_bound( lr_hot_upg->mr_logger ).
    cl_abap_unit_assert=>assert_bound( lr_hot_upg->mr_helper ).
  ENDMETHOD.

  METHOD check_consistency_no_data_hta.
    DATA(lv_result) = mr_cut->check_consistency( ).

    cl_abap_unit_assert=>assert_true( lv_result ).

    me->verify_logs_check_consistency( ).
  ENDMETHOD.

  METHOD check_consistency_pkgs_pw_cplx.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.NO.PREWORK.1'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hot_activation_mode = space )
                               ( abap_hana_package_id = 'COM.AUNIT.NO.PREWORK.2'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hot_activation_mode = 'A' )
                               ( abap_hana_package_id = 'COM.AUNIT.NO.PREWORK.3'
                                 abap_status = 'I'
                                 hot_status = 'A'
                                 hot_activation_mode = 'A' )
                               ( abap_hana_package_id = 'COM.AUNIT.PREWORK_REQUIRED.I'
                                 abap_status = 'I'
                                 hot_status = 'A'
                                 hana_package_id = 'com.aunit.prework.required.i'
                                 hot_activation_mode = 'P' ) "Prework for this package is not yet done but AVAP_STATUS=I packages should be ignored in check!
                               ( abap_hana_package_id = 'COM.AUNIT.PREWORK_REQUIRED.X'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hana_package_id = 'com.aunit.prework.required.x'
                                 hot_activation_mode = 'P' )
                               ( abap_hana_package_id = 'COM.AUNIT.PREWORK_REQUIRED.SPACE'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hana_package_id = 'com.aunit.prework.required.space'
                                 hot_activation_mode = 'P' )
                               ( abap_hana_package_id = 'COM.AUN;IRNV3D9KS0HJ2BTH7Q4DLENCLQTLA7F9'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hana_package_id = 'com.aunit.some.very.long.package.longer_than.50chars.pack2'
                                 hot_activation_mode = 'P' ) ).

    mt_hot_prework = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PREWORK_REQUIRED.X'
                                prework_done = 'X' )
                              ( abap_hana_package_id = 'COM.AUNIT.PREWORK_REQUIRED.SPACE'
                                prework_done = space ) ).

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PREWORK_REQUIRED.SPAC'
              var2 = 'E --> com.aunit.prework.require'
              var3 = 'd.space []' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUN;IRNV3D9KS0HJ2BTH7Q4DLEN'
              var2 = 'CLQTLA7F9 --> com.aunit.some.ve'
              var3 = 'ry.long.package.longer_than.50c'
              var4 = 'hars.pack2 []' ) ).
    me->verify_logs_check_consistency( it_exp_logs_missing_prework = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_pkgs_w_pw_f1.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hana_package_id = 'com.aunit.pack'
                                 hot_activation_mode = 'P' ) ).

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PACK --> com.aunit.pa'
              var2 = 'ck [BC-CTS-HTA-AUNIT]' ) ).
    me->verify_logs_check_consistency( it_exp_logs_missing_prework = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_pkgs_w_pw_f2.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hana_package_id = 'com.aunit.pack'
                                 hot_activation_mode = 'P' ) ).

    mt_hot_prework = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK'
                                prework_done = space ) ).

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PACK --> com.aunit.pa'
              var2 = 'ck [BC-CTS-HTA-AUNIT]' ) ).
    me->verify_logs_check_consistency( it_exp_logs_missing_prework = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_pkgs_w_pw_t.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hot_activation_mode = 'P' )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK2'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hot_activation_mode = 'P' ) ).

    mt_hot_prework = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                prework_done = 'X' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK2'
                                prework_done = 'X' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK3'
                                prework_done = 'X' ) ).

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).
    me->verify_logs_check_consistency( ).
  ENDMETHOD.

  METHOD check_consistency_pkgs_wo_pw.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hot_activation_mode = space )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK2'
                                 abap_status = 'A'
                                 hot_status = 'A'
                                 hot_activation_mode = 'A' ) ).

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).
    me->verify_logs_check_consistency( ).
  ENDMETHOD.

  METHOD check_consistency_pkgs_dep.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                 abap_status = 'A'
                                 hot_status = 'A' )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK2'
                                 abap_status = 'A'
                                 hot_status = 'N' )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK3'
                                 abap_status = 'I'
                                 hot_status = 'I' ) ). "Test that ABAP_STATUS=I is ignored in check_consistency

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).
    me->verify_logs_check_consistency( ).
  ENDMETHOD.

  METHOD check_consistency_pkgs_not_dep.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK.I'
                                 abap_status = 'A'
                                 hot_status = 'I'
                                 hana_package_id = 'com.aunit.pack.i' )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK.D'
                                 abap_status = 'A'
                                 hot_status = 'D'
                                 hana_package_id = 'com.aunit.pack.d' )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK.E'
                                 abap_status = 'A'
                                 hot_status = 'E'
                                 hana_package_id = 'com.aunit.pack.e' )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK.Z'
                                 abap_status = 'A'
                                 hot_status = 'Z'
                                 hana_package_id = 'com.aunit.pack.z' ) ).

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PACK.D --> com.aunit.'
              var2 = 'pack.d []' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PACK.E --> com.aunit.'
              var2 = 'pack.e []' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PACK.I --> com.aunit.'
              var2 = 'pack.i [BC-CTS-HTA-AUNIT]' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PACK.Z --> com.aunit.'
              var2 = 'pack.z []' ) ).
    me->verify_logs_check_consistency( it_exp_logs_not_deployed_pkgs = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_objs_dep.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'A' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK2'
                                abap_hana_object_name_suffix = 'VIEW2.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'N' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK3'
                                abap_hana_object_name_suffix = 'VIEW3.CALCULATIONVIEW'
                                abap_status = 'I'
                                hot_status = 'I' ) ). "Test that ABAP_STATUS=I is ignored in check_consistency

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).
    me->verify_logs_check_consistency( ).
  ENDMETHOD.

  METHOD check_consistency_objs_not_dep.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK.I'
                                abap_hana_object_name_suffix = 'VIEW_I.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'I'
                                hana_package_id = 'com.aunit.pack.i'
                                hana_object_name = 'VIEW_I'
                                hana_object_suffix = 'calculationview' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.D'
                                abap_hana_object_name_suffix = 'VIEW_D.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'D'
                                hana_package_id = 'com.aunit.pack.d'
                                hana_object_name = 'VIEW_D'
                                hana_object_suffix = 'calculationview' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.E'
                                abap_hana_object_name_suffix = 'VIEW_E.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'E'
                                hana_package_id = 'com.aunit.pack.e'
                                hana_object_name = 'VIEW_E'
                                hana_object_suffix = 'calculationview' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.Z'
                                abap_hana_object_name_suffix = 'VIEW_Z.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'Z'
                                hana_package_id = 'com.aunit.pack.z'
                                hana_object_name = 'VIEW_Z'
                                hana_object_suffix = 'calculationview' ) ).

    me->insert_test_data( ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK.D                        VIEW_D.CAL'
              var2 = 'CULATIONVIEW --> VIEW_D.calculationview (com.aunit'
              var3 = '.pack.d) []' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK.E                        VIEW_E.CAL'
              var2 = 'CULATIONVIEW --> VIEW_E.calculationview (com.aunit'
              var3 = '.pack.e) []' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK.I                        VIEW_I.CAL'
              var2 = 'CULATIONVIEW --> VIEW_I.calculationview (com.aunit'
              var3 = '.pack.i) [BC-CTS-HTA-AUNIT]' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK.Z                        VIEW_Z.CAL'
              var2 = 'CULATIONVIEW --> VIEW_Z.calculationview (com.aunit'
              var3 = '.pack.z) []' ) ).
    me->verify_logs_check_consistency( it_exp_logs_not_deployed_objs = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_objs_diff_v1.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'A'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'VIEW1'
                                hana_object_suffix = 'calculationview'
                                hana_object_version = 3
                                hana_content_cdata = 'SAME_C_DATA_IN_HTA_AND_HANA_REPO' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'PIC.JPG'
                                abap_status = 'A'
                                hot_status = 'N'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'PIC'
                                hana_object_suffix = 'jpg'
                                hana_object_version = 4
                                hana_content_bdata = cl_abap_codepage=>convert_to( 'SAME_B_DATA_IN_HTA_AND_HANA_REPO' ) ) ).

    me->insert_test_data( ).

    mr_helper_double->mt_hana_repo_data = VALUE #( ( package_id = 'com.aunit.pack1'
                                                     object_name = 'VIEW1'
                                                     object_suffix = 'calculationview'
                                                     version = 1
                                                     cdata = 'SAME_C_DATA_IN_HTA_AND_HANA_REPO' )
                                                   ( package_id = 'com.aunit.pack1'
                                                     object_name = 'PIC'
                                                     object_suffix = 'jpg'
                                                     version = 1
                                                     bdata = cl_abap_codepage=>convert_to( 'SAME_B_DATA_IN_HTA_AND_HANA_REPO' ) ) ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).
    me->verify_logs_check_consistency( ).
  ENDMETHOD.

  METHOD check_consistency_objs_diff_v2.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'A'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'VIEW1'
                                hana_object_suffix = 'calculationview'
                                hana_object_version = 1
                                hana_content_cdata = 'DIFFERENT_C_DATA_IN_HTA_AND_HANA_REPO' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'PIC.JPG'
                                abap_status = 'A'
                                hot_status = 'N'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'PIC'
                                hana_object_suffix = 'jpg'
                                hana_object_version = 2
                                hana_content_bdata = cl_abap_codepage=>convert_to( 'DIFFERENT_B_DATA_IN_HTA_AND_HANA_REPO' ) ) ).

    me->insert_test_data( ).

    mr_helper_double->mt_hana_repo_data = VALUE #( ( package_id = 'com.aunit.pack1'
                                                     object_name = 'VIEW1'
                                                     object_suffix = 'calculationview'
                                                     version = 11
                                                     cdata = 'HANA_REPO_AND_HTA_HAVE_DIFFERENT_C_DATA' )
                                                   ( package_id = 'com.aunit.pack1'
                                                     object_name = 'PIC'
                                                     object_suffix = 'jpg'
                                                     version = 22
                                                     bdata = cl_abap_codepage=>convert_to( 'HANA_REPO_AND_HTA_HAVE_DIFFERENT_B_DATA' ) ) ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK1                         PIC.JPG --'
              var2 = '> PIC.jpg (com.aunit.pack1) []' )
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK1                         VIEW1.CALC'
              var2 = 'ULATIONVIEW --> VIEW1.calculationview (com.aunit.p'
              var3 = 'ack1) []' ) ).
    me->verify_logs_check_consistency( it_exp_logs_different_obj_vers = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_objs_diff_v3.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'A'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'VIEW1'
                                hana_object_suffix = 'calculationview'
                                hana_object_version = 1
                                hana_content_cdata = 'DIFFERENT_C_DATA_IN_HTA_AND_HANA_REPO' ) ).

    me->insert_test_data( ).

    mr_helper_double->mt_hana_repo_data = VALUE #( ( package_id = 'com.aunit.pack1'
                                                     object_name = 'VIEW1'
                                                     object_suffix = 'calculationview'
                                                     version = 0 "0 is used as indicator of not existing object in HANA Repo
                                                     cdata = 'HANA_REPO_AND_HTA_HAVE_DIFFERENT_C_DATA' ) ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK1                         VIEW1.CALC'
              var2 = 'ULATIONVIEW --> VIEW1.calculationview (com.aunit.p'
              var3 = 'ack1) []' ) ).
    me->verify_logs_check_consistency( it_exp_logs_different_obj_vers = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_objs_same_v.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'A'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'VIEW1'
                                hana_object_suffix = 'calculationview'
                                hana_object_version = 11
                                hana_content_cdata = 'SAME_C_DATA_HTA_HANA_REPO' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'PIC.JPG'
                                abap_status = 'A'
                                hot_status = 'N'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'PIC'
                                hana_object_suffix = 'jpg'
                                hana_object_version = 22
                                hana_content_bdata = cl_abap_codepage=>convert_to( 'SAME_B_DATA_HTA_HANA_REPO' ) ) ).

    me->insert_test_data( ).

    mr_helper_double->mt_hana_repo_data = VALUE #( ( package_id = 'com.aunit.pack1'
                                                     object_name = 'VIEW1'
                                                     object_suffix = 'calculationview'
                                                     version = 11
                                                     cdata = 'SAME_C_DATA_HTA_HANA_REPO' )
                                                   ( package_id = 'com.aunit.pack1'
                                                     object_name = 'PIC'
                                                     object_suffix = 'jpg'
                                                     version = 22
                                                     bdata = cl_abap_codepage=>convert_to( 'SAME_B_DATA_HTA_HANA_REPO' ) ) ).
    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).
    me->verify_logs_check_consistency( ).
  ENDMETHOD.

  METHOD check_consistency_several_issu.
    " 1. prepare test data
    mt_hot_packages = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK'
                                 abap_status = 'A'
                                 hot_status = 'I'
                                 hana_package_id = 'com.aunit.pack'
                                 hot_activation_mode = 'P' )
                               ( abap_hana_package_id = 'COM.AUNIT.PACK2'
                                 abap_status = 'A'
                                 hana_package_id = 'com.aunit.pack'
                                 hot_status = 'A' ) ).

    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'I'
                                hana_package_id = 'com.aunit.pack'
                                hana_object_name = 'VIEW1'
                                hana_object_suffix = 'calculationview' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK2'
                                abap_hana_object_name_suffix = 'VIEW2.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'I'
                                hana_package_id = 'com.aunit.pack2'
                                hana_object_name = 'VIEW2'
                                hana_object_suffix = 'calculationview'
                                hana_object_version = 22 ) ).

    me->insert_test_data( ).

    mr_helper_double->mt_hana_repo_data = VALUE #( ( package_id = 'com.aunit.pack2'
                                                     object_name = 'VIEW2'
                                                     object_suffix = 'calculationview'
                                                     version = 33 ) ).
    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs_missing_prew) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '530'
              var1 = 'COM.AUNIT.PACK --> com.aunit.pa'
              var2 = 'ck [BC-CTS-HTA-AUNIT]' ) ).
    DATA(lt_expected_logs_not_dep_objs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'E'
              langu = sy-langu
              ag = 'SCTS_HDI'
              msgnr = '100'
              var1 = 'COM.AUNIT.PACK2                         VIEW2.CALC'
              var2 = 'ULATIONVIEW --> VIEW2.calculationview (com.aunit.p'
              var3 = 'ack2) []' ) ).

    me->verify_logs_check_consistency( it_exp_logs_missing_prework = lt_expected_logs_missing_prew
                                       it_exp_logs_not_deployed_objs = lt_expected_logs_not_dep_objs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_hana_exc_1.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'A'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'VIEW1'
                                hana_object_suffix = 'calculationview'
                                hana_object_version = 11
                                hana_content_cdata = 'SAME_C_DATA_HTA_HANA_REPO' ) ).

    me->insert_test_data( ).

    DATA(lr_expected_exc) = NEW cx_hana_object_transport(
            textid   = cx_hana_object_transport=>cx_nhi_hana_repository_error
            msgv1    = 'Text1'
            msgv2    = 'Text2'
            msgv3    = 'Text3'
            msgv4    = 'Text4' ).
    mr_helper_double->mr_exc_read_object_version = lr_expected_exc.

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'A'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '000'
              var1 = 'Text1'
              var2 = 'Text2'
              var3 = 'Text3'
              var4 = 'Text4' ) ).
    me->verify_logs_check_consistency( it_exp_logs_exc_check_obj_vers = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD check_consistency_hana_exc_2.
    " 1. prepare test data
    mt_hot_objects = VALUE #( ( abap_hana_package_id = 'COM.AUNIT.PACK1'
                                abap_hana_object_name_suffix = 'VIEW1.CALCULATIONVIEW'
                                abap_status = 'A'
                                hot_status = 'A'
                                hana_package_id = 'com.aunit.pack1'
                                hana_object_name = 'VIEW1'
                                hana_object_suffix = 'calculationview'
                                hana_object_version = 11
                                hana_content_cdata = 'SAME_C_DATA_HTA_HANA_REPO' ) ).

    me->insert_test_data( ).

    DATA(lr_expected_exc) = NEW cx_hana_object_transport(
            textid   = cx_hana_object_transport=>cx_nhi_hana_repository_error
            msgv1    = 'Text1' ).
    mr_helper_double->mr_exc_read_object_data = lr_expected_exc.

    " 2. execute business function
    DATA(lv_result) = mr_cut->check_consistency( ).

    " 3. verification
    cl_abap_unit_assert=>assert_false( lv_result ).
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
            ( level = 2
              severity = 'A'
              langu = sy-langu
              ag = 'SCTS_HOT'
              msgnr = '000'
              var1 = 'Text1' ) ).
    me->verify_logs_check_consistency( it_exp_logs_exc_check_obj_vers = lt_expected_logs
                                       iv_result = abap_false ).
  ENDMETHOD.

  METHOD insert_test_data.
    gr_osql_env->insert_test_data( mt_hot_packages ).
    gr_osql_env->insert_test_data( mt_hot_objects ).
    gr_osql_env->insert_test_data( mt_hot_prework ).
  ENDMETHOD.

  METHOD verify_logs_check_consistency.
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
             ( level = 2
               severity = ' '
               langu = sy-langu
               ag = 'SCTS_HOT'
               msgnr = '401' ) ). "Beginn Konsistenzprüfung der HANA-Repository-Pakete und -Objekte

    IF it_exp_logs_missing_prework IS INITIAL.
      APPEND VALUE sprot_u( level = 3
                            severity = ' '
                            langu = sy-langu
                            ag = 'SCTS_HOT'
                            msgnr = '403' ) TO lt_expected_logs. "  Keine HANA-Repository-Pakete mit fehlender Vorarbeit gefunden
    ELSE.
      APPEND VALUE sprot_u( level = 2
                            severity = 'E'
                            langu = sy-langu
                            ag = 'SCTS_HOT'
                            msgnr = '404' ) TO lt_expected_logs. "  Vorarbeit ist nicht erledigt für folgende Pakete:
      APPEND LINES OF it_exp_logs_missing_prework TO lt_expected_logs.
    ENDIF.

    IF it_exp_logs_not_deployed_pkgs IS INITIAL.
      IF it_exp_logs_missing_prework IS INITIAL.
        APPEND VALUE sprot_u( level = 3
                              severity = ' '
                              langu = sy-langu
                              ag = 'SCTS_HOT'
                              msgnr = '405' ) TO lt_expected_logs. "  Alle hana-Repository-Pakete sind deployt
      ELSE.
        APPEND VALUE sprot_u( level = 3
                              severity = ' '
                              langu = sy-langu
                              ag = 'SCTS_HOT'
                              msgnr = '412' ) TO lt_expected_logs. "  Alle Pakete sind deployt (Ausnahme: Pakete mit fehlender Vorarbeit)
      ENDIF.
    ELSE.
      APPEND VALUE sprot_u( level = 2
                            severity = 'E'
                            langu = sy-langu
                            ag = 'SCTS_HOT'
                            msgnr = '406' ) TO lt_expected_logs. "  Folgende HANA-Repository-Pakete sind nicht deployt:
      APPEND LINES OF it_exp_logs_not_deployed_pkgs TO lt_expected_logs.
    ENDIF.

    IF it_exp_logs_not_deployed_objs IS INITIAL.
      IF it_exp_logs_missing_prework IS INITIAL.
        APPEND VALUE sprot_u( level = 3
                              severity = ' '
                              langu = sy-langu
                              ag = 'SCTS_HOT'
                              msgnr = '407' ) TO lt_expected_logs. "  Alle HANA-Repository-Objekte sind deployt
      ELSE.
        APPEND VALUE sprot_u( level = 3
                              severity = ' '
                              langu = sy-langu
                              ag = 'SCTS_HOT'
                              msgnr = '413' ) TO lt_expected_logs. "  Alle Objekte, für deren Pakete die Vorarbeit erledigt ist, sind deployt
      ENDIF.
    ELSE.
      APPEND VALUE sprot_u( level = 2
                            severity = 'E'
                            langu = sy-langu
                            ag = 'SCTS_HOT'
                            msgnr = '408' ) TO lt_expected_logs. "  Folgende HANA-Repository-Objekte sind nicht deployt:
      APPEND LINES OF it_exp_logs_not_deployed_objs TO lt_expected_logs.
    ENDIF.

    IF it_exp_logs_exc_check_obj_vers IS INITIAL.
      IF it_exp_logs_different_obj_vers IS INITIAL.
        IF it_exp_logs_missing_prework IS INITIAL AND it_exp_logs_not_deployed_objs IS INITIAL.
          APPEND VALUE sprot_u( level = 3
                                severity = ' '
                                langu = sy-langu
                                ag = 'SCTS_HOT'
                                msgnr = '410' ) TO lt_expected_logs. "  Alle HANA-Repository-Objekte im HTA haben gleichen Inhalt wie in HANA
        ELSE.
          APPEND VALUE sprot_u( level = 3
                                severity = ' '
                                langu = sy-langu
                                ag = 'SCTS_HOT'
                                msgnr = '414' ) TO lt_expected_logs. "  Alle noch nicht als Fehler protokol. Obj. sind gleich in HTA und HANA
        ENDIF.
      ELSE.
        APPEND VALUE sprot_u( level = 2
                              severity = 'E'
                              langu = sy-langu
                              ag = 'SCTS_HOT'
                              msgnr = '411' ) TO lt_expected_logs. "  Folgende HANA-Repository-Objekte sind unterschiedlich in HTA und HANA:
        APPEND LINES OF it_exp_logs_different_obj_vers TO lt_expected_logs.
      ENDIF.
    ELSE.
      APPEND LINES OF it_exp_logs_exc_check_obj_vers TO lt_expected_logs.
    ENDIF.

    IF iv_result = abap_false.
      APPEND VALUE sprot_u( level = 2
                            severity = 'E'
                            langu = sy-langu
                            ag = 'SCTS_HOT'
                            msgnr = '409' ) TO lt_expected_logs. "  Inkonsistenzen zwischen HTA und HANA gefunden. Siehe Hinweis 2776773
    ENDIF.

    APPEND VALUE sprot_u( level = 2
                          severity = ' '
                          langu = sy-langu
                          ag = 'SCTS_HOT'
                          msgnr = '402'
                          var1 = COND #( WHEN iv_result = abap_true THEN 'Success'(002) ELSE 'Error'(001) )
                        ) TO lt_expected_logs. "Ende Konsistenzprüfung der HANA-Repository-Pakete und -Objekte

    DATA(lt_log_messages) = mr_logger_double->get_log_messages( ).
    cl_abap_unit_assert=>assert_equals( act = lt_log_messages exp = lt_expected_logs msg = 'Actual and expected logs are different' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cts_hot_upg_prepare_redep DEFINITION FINAL FOR TESTING DURATION MEDIUM RISK LEVEL DANGEROUS.
  PRIVATE SECTION.
    TYPES:
      ty_t_cts_hot_package TYPE STANDARD TABLE OF cts_hot_package WITH EMPTY KEY,
      ty_t_cts_hot_object  TYPE STANDARD TABLE OF cts_hot_object WITH EMPTY KEY,
      ty_t_sprot_u         TYPE if_cts_hot_logger=>ty_t_sprot_u.

    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown RAISING cx_static_check.

    CLASS-DATA:
      gr_osql_env TYPE REF TO if_osql_test_environment.

    METHODS:
      setup RAISING cx_static_check,
      teardown RAISING cx_static_check,

      "! Testing prepare_redeployment in case there is no change in HTA and in BCK tables
      no_change_hta_and_bck FOR TESTING RAISING cx_static_check,

      "! Testing prepare_redeployment in case table CTS_HOT_PACKAGE~BCK is missing in the system
      table_hot_package_bck_missing FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment in case table CTS_HOT_OBJECT~BCK is missing in the system
      table_hot_object_bck_missing FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for an object existing in table cts_hot_object~bck but not in CTS_HOT_OBJECT.
      "! Object should be added as to be deleted in cts_hot_object
      add_to_be_deleted_objs FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for packages existing in table cts_hot_package~bck but not in CTS_HOT_PACKAGE.
      "! Only packages with ABAP_STATUS=A should be added as to be deleted in CTS_HOT_PACKAGE, independent of their HOT_STATUS
      add_to_be_deleted_pkgs FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for packages existing in table cts_hot_package~bck but not in CTS_HOT_PACKAGE for packages
      "! that require prework (having hot_activation_mode=P). For these packages also entries in CTS_HOT_PREWORK must be created
      "! by method add_to_be_deleted_pkgs_objs.
      "! Only packages with ABAP_STATUS=A should be added as to be deleted in CTS_HOT_PACKAGE, independent of their HOT_STATUS
      add_to_be_deleted_pkgs_w_pw FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for packages existing in table cts_hot_package~bck but not in CTS_HOT_PACKAGE and
      "! for objects existing in table cts_hot_object~bck but not in CTS_HOT_OBJECT.
      add_to_be_deleted_pkgs_objs FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package not existing in table cts_hot_package~bck and thus ALL packages
      "! in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_1 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_description in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_2 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_responsible in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_3 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_orig_lang in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_4 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_is_structural in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_5 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_text_collection in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_6 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_text_status in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_7 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_text_term_domain in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_pkgs_8 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one package that has different hana_pack_text_hints_for_transl in tables cts_hot_package~bck
      "! and cts_hot_package and thus ALL packages in cts_hot_package should be set to HOT_STATUS=I where ABAP_STATUS=A.<br/>
      "! Remark for all tests set_to_be_deployed_pkgs_x: There is no test for HANA_PACK_DELIVERY_UNIT and HANA_PACK_DU_VENDOR because HTA does not write this info to HANA and
      "! therefore the result would not make any difference
      set_to_be_deployed_pkgs_9 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one object not existing in table cts_hot_object~bck and thus ALL objects
      "! in cts_hot_object should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_objs_1 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one object that has different cdata in tables cts_hot_object~bck and cts_hot_object
      "! and thus ALL objects in cts_hot_object should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_objs_2 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one object that has different bdata in tables cts_hot_object~bck and cts_hot_object
      "! and thus ALL objects in cts_hot_object should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_objs_3 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one object that has different version and different cdata in table cts_hot_object
      "! and in HANA repository and thus ALL objects in cts_hot_object should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_objs_4 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one object that has different version and different bdata in table cts_hot_object
      "! and in HANA repository and thus ALL objects in cts_hot_object should be set to HOT_STATUS=I where ABAP_STATUS=A.
      set_to_be_deployed_objs_5 FOR TESTING RAISING cx_static_check,
      "! Testing prepare_redeployment for one object that has different version but same cdata and same bdata in table cts_hot_object
      "! and in HANA repository and thus NOT any object should be set to HOT_STATUS=I.
      set_to_be_deployed_objs_6 FOR TESTING RAISING cx_static_check,

      "! Verify logs for method prepare_redeployment. By default 415 and 416 are always expected
      verify_logs_prepare_deployment
        IMPORTING
          iv_added_packs_for_deletion TYPE i DEFAULT 0
          iv_added_objs_for_deletion  TYPE i DEFAULT 0
          iv_set_packs_for_deploy     TYPE i DEFAULT 0
          iv_set_objs_for_deploy      TYPE i DEFAULT 0
          it_exp_log_messages         TYPE ty_t_sprot_u OPTIONAL
          iv_result                   TYPE abap_bool DEFAULT abap_true,

      create_bck_tables
        RAISING
          cx_sql_exception,

      drop_bck_tables,
      create_package_in_bck_table
        IMPORTING
          iv_package_id          TYPE string
          iv_abap_status         TYPE cts_hot_package-abap_status DEFAULT 'A'
          iv_hot_status          TYPE cts_hot_package-hot_status DEFAULT 'A'
          iv_hot_activation_mode TYPE cts_hot_package-hot_activation_mode DEFAULT 'A'
        RAISING
          cx_hana_object_transport
          cx_sql_exception,
      "! In order to use if_osql_test_environment on ABAP side we need to get all existing data into if_osql_test_environment
      fill_osql_env_from_bck
        RAISING
          cx_sql_exception,
      verify_packages
        IMPORTING
          it_act_packages TYPE ty_t_cts_hot_package
          it_exp_packages TYPE ty_t_cts_hot_package,
      create_object_in_bck_table
        IMPORTING
          iv_package_id    TYPE string
          iv_object_name   TYPE string
          iv_object_suffix TYPE string
          iv_abap_status   TYPE string DEFAULT 'A'
          iv_hot_status    TYPE string DEFAULT 'A'
        RAISING
          cx_hana_object_transport
          cx_sql_exception,
      verify_objects
        IMPORTING
          it_act_objects TYPE ty_t_cts_hot_object
          it_exp_objects TYPE ty_t_cts_hot_object,
      create_object_in_abap_table
        IMPORTING
          iv_package_id    TYPE string
          iv_object_name   TYPE string
          iv_object_suffix TYPE string
          iv_hot_status    TYPE string
        RAISING
          cx_hana_object_transport,
      create_package_in_abap_table
        IMPORTING
          iv_package_id TYPE string
          iv_hot_status TYPE string
        RAISING
          cx_hana_object_transport,
      "! Ensure all packages were set to hot_status = 'I' apart from 'D' and 'Z' packages
      "! @parameter rv_count | Number of packages with HOT_STATUS=I
      verify_hot_status_i_for_pkgs
        RETURNING
          VALUE(rv_count) TYPE i,
      "! Ensure all objects were set to hot_status = 'I' apart from 'D' and 'Z' objects
      "! @parameter rv_count | Number of objects with HOT_STATUS=I
      verify_hot_status_i_for_objs
        RETURNING
          VALUE(rv_count) TYPE i,
      fill_helper_double.

    DATA:
      mr_logger_double TYPE REF TO cl_cts_hot_logger_memory,
      mr_helper_double TYPE REF TO ltd_lcl_helper,
      mr_cut           TYPE REF TO cl_cts_hot_upg,
      mt_hot_packages  TYPE STANDARD TABLE OF cts_hot_package,
      mt_hot_objects   TYPE STANDARD TABLE OF cts_hot_object,
      mt_hot_prework   TYPE STANDARD TABLE OF cts_hot_prework.
ENDCLASS.

CLASS cl_cts_hot_upg DEFINITION LOCAL FRIENDS ltcl_cts_hot_upg_prepare_redep.
CLASS ltcl_cts_hot_upg_prepare_redep IMPLEMENTATION.
  METHOD class_setup.
    IF sy-dbsys <> 'HDB'.
      cl_abap_unit_assert=>abort( msg = 'Test can only be run on ABAP systems on HDB' ).
    ENDIF.

    "test can only run if some data is already in HTA repository
    SELECT COUNT(*) FROM cts_hot_package WHERE abap_status = 'A' INTO @DATA(lv_cnt).
    IF lv_cnt < 1.
      cl_abap_unit_assert=>abort( msg = 'CTS_HOT_PACKAGE is empty. Test can not be executed.' ).
    ENDIF.
    SELECT COUNT(*) FROM cts_hot_object WHERE abap_status = 'A' INTO @lv_cnt.
    IF lv_cnt < 1.
      cl_abap_unit_assert=>abort( msg = 'CTS_HOT_OBJECT is empty. Test can not be executed.' ).
    ENDIF.

    gr_osql_env = cl_osql_test_environment=>create( VALUE #( ( 'CTS_HOT_OBJECT' )
                                                             ( 'CTS_HOT_PACKAGE' )
                                                             ( 'CTS_HOT_PREWORK' )
                                                             ( 'TADIR' )
                                                             ( 'TDEVC' )
                                                             ( 'DF14L' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    gr_osql_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    DATA: lt_tadir TYPE STANDARD TABLE OF tadir,
          lt_tdevc TYPE STANDARD TABLE OF tdevc,
          lt_df14l TYPE STANDARD TABLE OF df14l.

    me->drop_bck_tables( ). "needed because tables might be there if previous tests failed

    gr_osql_env->clear_doubles( ).

    mr_logger_double = CAST cl_cts_hot_logger_memory( cl_cts_hot_logger_memory=>create_instance( ) ).
    mr_helper_double = NEW ltd_lcl_helper( ).
    mr_cut = NEW cl_cts_hot_upg( ir_logger = NEW lcl_logger( mr_logger_double )
                                 ir_helper = mr_helper_double ).

    me->create_bck_tables( ).
    me->fill_osql_env_from_bck( ).
    me->fill_helper_double( ).
  ENDMETHOD.

  METHOD teardown.
    me->drop_bck_tables( ).
  ENDMETHOD.

  METHOD no_change_hta_and_bck.
* prepare
    "no prepare needed

* execute
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

* verify
    cl_abap_unit_assert=>assert_true( lv_result ).

    "ensure the packages and objects are unchanged in cts_hot_package/cts_hot_object
    SELECT * FROM cts_hot_package INTO TABLE @DATA(lt_packages).
    SELECT * FROM cts_hot_object INTO TABLE @DATA(lt_objects).

    SORT lt_packages BY abap_hana_package_id ASCENDING abap_status ASCENDING.
    SORT lt_objects BY abap_hana_package_id ASCENDING abap_hana_object_name_suffix ASCENDING abap_status ASCENDING.
    cl_abap_unit_assert=>assert_equals( act = lt_packages exp = mt_hot_packages ).
    cl_abap_unit_assert=>assert_equals( act = lt_objects exp = mt_hot_objects ).

    me->verify_logs_prepare_deployment( ).
  ENDMETHOD.

  METHOD table_hot_package_bck_missing.
* prepare
    NEW cl_sql_statement( )->execute_ddl( |DROP TABLE "CTS_HOT_PACKAGE~BCK"| ).

* execute
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

* verify
    cl_abap_unit_assert=>assert_false( lv_result ).

    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
                ( level = 2
                  severity = 'E'
                  langu = sy-langu
                  ag = 'SCTS_HOT'
                  msgnr = '417' ) ). "  Tabelle CTS_HOT_PACKAGE~BCK fehlt
    me->verify_logs_prepare_deployment( it_exp_log_messages = lt_expected_logs
                                        iv_result = lv_result ).
  ENDMETHOD.

  METHOD table_hot_object_bck_missing.
* prepare
    NEW cl_sql_statement( )->execute_ddl( |DROP TABLE "CTS_HOT_OBJECT~BCK"| ).


* execute
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

* verify
    cl_abap_unit_assert=>assert_false( lv_result ).

    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
                ( level = 2
                  severity = 'E'
                  langu = sy-langu
                  ag = 'SCTS_HOT'
                  msgnr = '418' ) ). "  Tabelle CTS_HOT_OBJECT~BCK fehlt
    me->verify_logs_prepare_deployment( it_exp_log_messages = lt_expected_logs
                                        iv_result = lv_result ).
  ENDMETHOD.

  METHOD add_to_be_deleted_objs.
    " 1. prepare test data
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.A' iv_object_name = 'OBJ_A_A' iv_object_suffix = 'suf' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.I' iv_object_name = 'OBJ_A_I' iv_object_suffix = 'suf' iv_hot_status = 'I' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.D' iv_object_name = 'OBJ_A_D' iv_object_suffix = 'suf' iv_hot_status = 'D' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.E' iv_object_name = 'OBJ_A_E' iv_object_suffix = 'suf' iv_hot_status = 'E' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.Z' iv_object_name = 'OBJ_A_Z' iv_object_suffix = 'suf' iv_hot_status = 'Z' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.N' iv_object_name = 'OBJ_A_N' iv_object_suffix = 'suf' iv_hot_status = 'N' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.I.A' iv_object_name = 'OBJ_I_A' iv_object_suffix = 'suf' iv_abap_status = 'I' iv_hot_status = 'A' ). "should be added to cts_hot_object due to ABAP_STATUS=I
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.I.I' iv_object_name = 'OBJ_I_I' iv_object_suffix = 'suf' iv_abap_status = 'I' iv_hot_status = 'I' ). "should be added to cts_hot_object due to ABAP_STATUS=I

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    SELECT * FROM cts_hot_object INTO TABLE @DATA(lt_objects).

    "delete all objects already existed before call to prepare_redeployment to get only the added objects
    LOOP AT mt_hot_objects REFERENCE INTO DATA(lr_obj).
      DELETE lt_objects WHERE abap_hana_package_id = lr_obj->abap_hana_package_id
                          AND abap_hana_object_name_suffix = lr_obj->abap_hana_object_name_suffix
                          AND abap_status = lr_obj->abap_status
                          AND hot_status = lr_obj->hot_status. "also use hot_status to make sure it was not modified...
    ENDLOOP.

    DATA(lt_expected_objects) = VALUE ty_t_cts_hot_object(
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.A' abap_hana_object_name_suffix = 'OBJ_A_A.SUF' abap_status = 'A' hot_status = 'D' )
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.I' abap_hana_object_name_suffix = 'OBJ_A_I.SUF' abap_status = 'A' hot_status = 'D' )
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.D' abap_hana_object_name_suffix = 'OBJ_A_D.SUF' abap_status = 'A' hot_status = 'D' )
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.E' abap_hana_object_name_suffix = 'OBJ_A_E.SUF' abap_status = 'A' hot_status = 'D' )
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.Z' abap_hana_object_name_suffix = 'OBJ_A_Z.SUF' abap_status = 'A' hot_status = 'D' )
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.N' abap_hana_object_name_suffix = 'OBJ_A_N.SUF' abap_status = 'A' hot_status = 'D' ) ).
    me->verify_objects( it_act_objects = lt_objects it_exp_objects = lt_expected_objects ).

    me->verify_logs_prepare_deployment( iv_added_objs_for_deletion = 6 ).
  ENDMETHOD.

  METHOD add_to_be_deleted_pkgs.
    " 1. prepare test data
    me->create_package_in_bck_table( 'com.aunit.pack.A.A' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.I' iv_hot_status = 'I' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.D' iv_hot_status = 'D' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.E' iv_hot_status = 'E' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.Z' iv_hot_status = 'Z' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.N' iv_hot_status = 'N' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.I.I' iv_abap_status = 'I' iv_hot_status = 'I' ). "should be added to cts_hot_package due to ABAP_STATUS=I
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.I.A' iv_abap_status = 'I'  iv_hot_status = 'A' ). "should be added to cts_hot_package due to ABAP_STATUS=I

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    SELECT * FROM cts_hot_package INTO TABLE @DATA(lt_packages).

    "delete all packages already existed before call to prepare_redeployment to get only the added packages
    LOOP AT mt_hot_packages REFERENCE INTO DATA(lr_pack).
      DELETE lt_packages WHERE abap_hana_package_id = lr_pack->abap_hana_package_id
                           AND abap_status = lr_pack->abap_status
                           AND hot_status = lr_pack->hot_status. "also use hot_status to make sure it was not modified...
    ENDLOOP.

    DATA(lt_expected_packs) = VALUE ty_t_cts_hot_package(
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.A' abap_status = 'A' hot_status = 'D' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.I' abap_status = 'A' hot_status = 'D' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.D' abap_status = 'A' hot_status = 'D' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.E' abap_status = 'A' hot_status = 'D' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.Z' abap_status = 'A' hot_status = 'D' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.N' abap_status = 'A' hot_status = 'D' ) ).
    me->verify_packages( it_act_packages = lt_packages it_exp_packages = lt_expected_packs ).

    me->verify_logs_prepare_deployment( iv_added_packs_for_deletion = 6 ).
  ENDMETHOD.

  METHOD add_to_be_deleted_pkgs_objs.
    " 1. prepare test data
    me->create_package_in_bck_table( 'com.aunit.pack.A.A' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.I' iv_hot_status = 'I' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.N' iv_hot_status = 'N' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.A' iv_object_name = 'OBJ_A_A' iv_object_suffix = 'suf' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.I' iv_object_name = 'OBJ_A_I' iv_object_suffix = 'suf' iv_hot_status = 'I' ).
    me->create_object_in_bck_table( iv_package_id = 'com.aunit.pack.A.N' iv_object_name = 'OBJ_A_N' iv_object_suffix = 'suf' iv_hot_status = 'N' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    SELECT * FROM cts_hot_package INTO TABLE @DATA(lt_packages).
    SELECT * FROM cts_hot_object INTO TABLE @DATA(lt_objects).

    "delete all packages already existed before call to prepare_redeployment to get only the added packages
    LOOP AT mt_hot_packages REFERENCE INTO DATA(lr_pack).
      DELETE lt_packages WHERE abap_hana_package_id = lr_pack->abap_hana_package_id
                           AND abap_status = lr_pack->abap_status
                           AND hot_status = lr_pack->hot_status. "also use hot_status to make sure it was not modified...
    ENDLOOP.

    "delete all objects already existed before call to prepare_redeployment to get only the added objects
    LOOP AT mt_hot_objects REFERENCE INTO DATA(lr_obj).
      DELETE lt_objects WHERE abap_hana_package_id = lr_obj->abap_hana_package_id
                          AND abap_hana_object_name_suffix = lr_obj->abap_hana_object_name_suffix
                          AND abap_status = lr_obj->abap_status
                          AND hot_status = lr_obj->hot_status. "also use hot_status to make sure it was not modified...
    ENDLOOP.

    DATA(lt_expected_packs) = VALUE ty_t_cts_hot_package(
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.A' abap_status = 'A' hot_status = 'D' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.I' abap_status = 'A' hot_status = 'D' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.N' abap_status = 'A' hot_status = 'D' ) ).
    me->verify_packages( it_act_packages = lt_packages it_exp_packages = lt_expected_packs ).

    DATA(lt_expected_objects) = VALUE ty_t_cts_hot_object(
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.A' abap_hana_object_name_suffix = 'OBJ_A_A.SUF' abap_status = 'A' hot_status = 'D' )
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.I' abap_hana_object_name_suffix = 'OBJ_A_I.SUF' abap_status = 'A' hot_status = 'D' )
                          ( abap_hana_package_id = 'COM.AUNIT.PACK.A.N' abap_hana_object_name_suffix = 'OBJ_A_N.SUF' abap_status = 'A' hot_status = 'D' ) ).
    me->verify_objects( it_act_objects = lt_objects it_exp_objects = lt_expected_objects ).

    me->verify_logs_prepare_deployment( iv_added_packs_for_deletion = 3 iv_added_objs_for_deletion = 3 ).
  ENDMETHOD.

  METHOD add_to_be_deleted_pkgs_w_pw.
    " 1. prepare test data
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.A' iv_hot_activation_mode = 'P' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.I' iv_hot_status = 'I' iv_hot_activation_mode = 'P' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.D' iv_hot_status = 'D' iv_hot_activation_mode = 'P' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.E' iv_hot_status = 'E' iv_hot_activation_mode = 'P' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.Z' iv_hot_status = 'Z' iv_hot_activation_mode = 'P' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.A.N' iv_hot_status = 'N' iv_hot_activation_mode = 'P' ).
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.I.I' iv_abap_status = 'I' iv_hot_status = 'I' iv_hot_activation_mode = 'P' ). "should not be added to cts_hot_package due to ABAP_STATUS=I
    me->create_package_in_bck_table( iv_package_id = 'com.aunit.pack.I.A' iv_abap_status = 'I'  iv_hot_status = 'A' iv_hot_activation_mode = 'P' ). "should not be added to cts_hot_package due to ABAP_STATUS=I

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    SELECT * FROM cts_hot_package INTO TABLE @DATA(lt_packages). "#EC CI_NOWHERE
    SELECT * FROM cts_hot_prework INTO TABLE @DATA(lt_prework_packages). "#EC CI_NOWHERE

    "delete all packages already existed before call to prepare_redeployment to get only the added packages
    LOOP AT mt_hot_packages REFERENCE INTO DATA(lr_pack).
      DELETE lt_packages WHERE abap_hana_package_id = lr_pack->abap_hana_package_id
                           AND abap_status = lr_pack->abap_status
                           AND hot_status = lr_pack->hot_status. "also use hot_status to make sure it was not modified...
      DELETE lt_prework_packages WHERE abap_hana_package_id = lr_pack->abap_hana_package_id.
    ENDLOOP.

    DATA(lt_expected_packs) = VALUE ty_t_cts_hot_package(
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.A' abap_status = 'A' hot_status = 'D' hot_activation_mode = 'P' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.I' abap_status = 'A' hot_status = 'D' hot_activation_mode = 'P' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.D' abap_status = 'A' hot_status = 'D' hot_activation_mode = 'P' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.E' abap_status = 'A' hot_status = 'D' hot_activation_mode = 'P' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.Z' abap_status = 'A' hot_status = 'D' hot_activation_mode = 'P' )
                              ( abap_hana_package_id = 'COM.AUNIT.PACK.A.N' abap_status = 'A' hot_status = 'D' hot_activation_mode = 'P' ) ).
    me->verify_packages( it_act_packages = lt_packages it_exp_packages = lt_expected_packs ).

    me->verify_logs_prepare_deployment( iv_added_packs_for_deletion = 6 ).

    LOOP AT lt_expected_packs REFERENCE INTO lr_pack.
      cl_abap_unit_assert=>assert_table_contains( table = lt_prework_packages
                                                  line = VALUE cts_hot_prework( abap_hana_package_id = lr_pack->abap_hana_package_id prework_done = 'X' )
                                                  msg = |Prework not set to done in CTS_HOT_PREWORK for package { lr_pack->abap_hana_package_id }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_to_be_deployed_objs_1.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all objects
    "    using delete on bck table because comparison is done on DB level with real cts_hot_object table that we do not want to change from the test
    DATA(ls_obj) = mt_hot_objects[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |DELETE FROM "CTS_HOT_OBJECT~BCK" WHERE ABAP_HANA_PACKAGE_ID = '{ ls_obj-abap_hana_package_id }' | &&
                                                                                |AND ABAP_HANA_OBJECT_NAME_SUFFIX = '{ ls_obj-abap_hana_object_name_suffix }'| ).

    " add objects with hot_status D and Z that must not be changed to I
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.d' iv_object_name = 'OBJ_D' iv_object_suffix = 'suf' iv_hot_status = 'D' ).
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.z' iv_object_name = 'OBJ_Z' iv_object_suffix = 'suf' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_objs( ).

    me->verify_logs_prepare_deployment( iv_set_objs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_objs_2.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all objects
    "    using update of HANA_CONTENT_CDATA on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_object table that we do not want to change from the test
    DATA(ls_obj) = mt_hot_objects[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_OBJECT~BCK" SET HANA_CONTENT_CDATA = 'HTA_AUNIT' | &&
                                                                        | WHERE ABAP_HANA_PACKAGE_ID = '{ ls_obj-abap_hana_package_id }' | &&
                                                                        |   AND ABAP_HANA_OBJECT_NAME_SUFFIX = '{ ls_obj-abap_hana_object_name_suffix }'| ).
    " add objects with hot_status D and Z that must not be changed to I
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.d' iv_object_name = 'OBJ_D' iv_object_suffix = 'suf' iv_hot_status = 'D' ).
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.z' iv_object_name = 'OBJ_Z' iv_object_suffix = 'suf' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_objs( ).

    me->verify_logs_prepare_deployment( iv_set_objs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_objs_3.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all objects
    "    using update of HANA_CONTENT_BDATA on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_object table that we do not want to change from the test
    DATA(ls_obj) = mt_hot_objects[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_OBJECT~BCK" SET HANA_CONTENT_BDATA = 'HTA_AUNIT' | &&
                                                                        | WHERE ABAP_HANA_PACKAGE_ID = '{ ls_obj-abap_hana_package_id }' | &&
                                                                        |   AND ABAP_HANA_OBJECT_NAME_SUFFIX = '{ ls_obj-abap_hana_object_name_suffix }'| ).
    " add objects with hot_status D and Z that must not be changed to I
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.d' iv_object_name = 'OBJ_D' iv_object_suffix = 'suf' iv_hot_status = 'D' ).
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.z' iv_object_name = 'OBJ_Z' iv_object_suffix = 'suf' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_objs( ).

    me->verify_logs_prepare_deployment( iv_set_objs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_objs_4.
    " 1. prepare test data
    " add objects with hot_status D and Z that must not be changed to I
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.d' iv_object_name = 'OBJ_D' iv_object_suffix = 'suf' iv_hot_status = 'D' ).
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.z' iv_object_name = 'OBJ_Z' iv_object_suffix = 'suf' iv_hot_status = 'Z' ).

    " Find 1 object in HTA that seems to be deployed correctly and set version of it in HANA repo to 9999999 and change its cdata
    LOOP AT mt_hot_objects REFERENCE INTO DATA(lr_object) WHERE abap_status = 'A' AND ( hot_status = 'A' OR hot_status = 'N' ).
      MODIFY mr_helper_double->mt_hana_repo_data FROM VALUE #( version = 9999999 cdata = 'HTA_AUNIT' )
                                                 TRANSPORTING version cdata
                                                 WHERE package_id = lr_object->hana_package_id AND
                                                       object_name = lr_object->hana_object_name AND
                                                       object_suffix = lr_object->hana_object_suffix.
      IF sy-subrc = 0. "if object was found and changed, exit the loop
        DATA(lv_repo_data_changed) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_true( act = lv_repo_data_changed msg = 'Test can not run. HTA Repository does not contain any deployed object.' level = if_aunit_constants=>tolerable ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_objs( ).

    me->verify_logs_prepare_deployment( iv_set_objs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_objs_5.
    DATA lv_repo_data_changed TYPE abap_bool VALUE abap_false.

    " 1. prepare test data
    " add objects with hot_status D and Z that must not be changed to I
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.d' iv_object_name = 'OBJ_D' iv_object_suffix = 'suf' iv_hot_status = 'D' ).
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.z' iv_object_name = 'OBJ_Z' iv_object_suffix = 'suf' iv_hot_status = 'Z' ).

    " Find 1 object in HTA that seems to be deployed correctly and set version of it in HANA repo to 9999999 and change its bdata
    LOOP AT mt_hot_objects REFERENCE INTO DATA(lr_object) WHERE abap_status = 'A' AND ( hot_status = 'A' OR hot_status = 'N' ).
      MODIFY mr_helper_double->mt_hana_repo_data FROM VALUE #( version = 9999999 bdata = cl_abap_codepage=>convert_to( 'Some bdata' ) )
                                                 TRANSPORTING version bdata
                                                 WHERE package_id = lr_object->hana_package_id AND
                                                       object_name = lr_object->hana_object_name AND
                                                       object_suffix = lr_object->hana_object_suffix.
      IF sy-subrc = 0. "if object was found and changed, exit the loop
        lv_repo_data_changed = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_repo_data_changed = abap_false.
      cl_abap_unit_assert=>abort( msg = 'Test can not run. HTA Repository does not contain any deployed object.' quit = if_aunit_constants=>method ).
    ENDIF.

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_objs( ).

    me->verify_logs_prepare_deployment( iv_set_objs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_objs_6.
    DATA lv_repo_data_changed TYPE abap_bool VALUE abap_false.

    " 1. prepare test data
    " add objects with hot_status D and Z that must not be changed to I
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.d' iv_object_name = 'OBJ_D' iv_object_suffix = 'suf' iv_hot_status = 'D' ).
    me->create_object_in_abap_table( iv_package_id = 'com.aunit.z' iv_object_name = 'OBJ_Z' iv_object_suffix = 'suf' iv_hot_status = 'Z' ).
    "Tests are different here in different releases, depending on availability of gr_osql_env... due to ADBC AND Open SQL calls in from ABAP

    " increase version of one object in HANA repo by 1 but make sure not to use a to be deleted object
    LOOP AT mr_helper_double->mt_hana_repo_data REFERENCE INTO DATA(lr_repo_data).
      DATA(ls_obj) = VALUE cts_hot_object( mt_hot_objects[ hana_package_id = lr_repo_data->package_id
                                                           hana_object_name = lr_repo_data->object_name
                                                           hana_object_suffix = lr_repo_data->object_suffix
                                                           abap_status = 'A' ] OPTIONAL ).

      IF ls_obj IS NOT INITIAL AND ls_obj-hot_status <> 'D' AND ls_obj-hot_status <> 'Z'.
        lr_repo_data->version = lr_repo_data->version + 1.
        lv_repo_data_changed = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_repo_data_changed = abap_false.
      cl_abap_unit_assert=>abort( msg = 'Test can not run. HTA Repository does not contain any deployed object.' quit = if_aunit_constants=>method ).
    ENDIF.

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    "ensure the objects are unchanged in cts_hot_object
    SELECT * FROM cts_hot_object INTO TABLE @DATA(lt_objects). "#EC CI_NOWHERE

    SORT lt_objects BY abap_hana_package_id ASCENDING abap_hana_object_name_suffix ASCENDING abap_status ASCENDING.
    cl_abap_unit_assert=>assert_equals( act = lt_objects exp = mt_hot_objects ).

    me->verify_logs_prepare_deployment( ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_1.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using delete on bck table because comparison is done on DB level with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |DELETE FROM "CTS_HOT_PACKAGE~BCK" WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_2.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of hana_pack_description on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_DESCRIPTION = 'HTA_AUNIT' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_3.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of HANA_PACK_RESPONSIBLE on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_RESPONSIBLE = 'HTA_AUNIT' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_4.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of HANA_PACK_ORIG_LANG on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_ORIG_LANG = 'HTA_AUNIT' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_5.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of HANA_PACK_IS_STRUCTURAL on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' hana_pack_is_structural = '0' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_IS_STRUCTURAL = '1' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_6.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of HANA_PACK_TEXT_COLLECTION on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_TEXT_COLLECTION = 'HTA_AUNIT' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_7.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of HANA_PACK_TEXT_STATUS on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_TEXT_STATUS = 'HTA_AUNIT' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_8.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of HANA_PACK_TEXT_TERM_DOMAIN on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_TEXT_TERM_DOMAIN = 'HTA_AUNIT' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD set_to_be_deployed_pkgs_9.
    " 1. prepare test data so that there is a diff with ~bck table that leads to hot_status'I' for all packages
    "    using update of HANA_PACK_HINTS_FOR_TRANSL on one entry in bck table because comparison is done on DB level
    "    with real cts_hot_package table that we do not want to change from the test
    DATA(ls_pack) = mt_hot_packages[ abap_status = 'A' ].
    NEW cl_sql_statement( )->execute_update( |UPDATE "CTS_HOT_PACKAGE~BCK" SET HANA_PACK_HINTS_FOR_TRANSL = 'HTA_AUNIT' WHERE ABAP_HANA_PACKAGE_ID = '{ ls_pack-abap_hana_package_id }'| ).
    " add packages with hot_status D and Z that must not be changed to I
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.d' iv_hot_status = 'D' ).
    me->create_package_in_abap_table( iv_package_id = 'com.aunit.pack.z' iv_hot_status = 'Z' ).

    " 2. execute business function
    DATA(lv_result) = mr_cut->prepare_redeployment( ).

    " 3. verification
    cl_abap_unit_assert=>assert_true( lv_result ).

    DATA(lv_count) = me->verify_hot_status_i_for_pkgs( ).

    me->verify_logs_prepare_deployment( iv_set_packs_for_deploy = lv_count ).
  ENDMETHOD.

  METHOD create_bck_tables.
    NEW cl_sql_statement( )->execute_ddl( |CREATE COLUMN TABLE "CTS_HOT_PACKAGE~BCK" AS ( SELECT * FROM "CTS_HOT_PACKAGE" )| ).
    NEW cl_sql_statement( )->execute_ddl( |CREATE COLUMN TABLE "CTS_HOT_OBJECT~BCK" AS ( SELECT * FROM "CTS_HOT_OBJECT" )| ).
  ENDMETHOD.

  METHOD drop_bck_tables.
    DATA(lr_sql) = NEW cl_sql_statement( ).
    TRY.
        lr_sql->execute_ddl( |DROP TABLE "CTS_HOT_PACKAGE~BCK"| ).
      CATCH cx_sql_exception.
        "ignore error if table does not exist
    ENDTRY.

    TRY.
        lr_sql->execute_ddl( |DROP TABLE "CTS_HOT_OBJECT~BCK"| ).
      CATCH cx_sql_exception.
        "ignore error if table does not exist
    ENDTRY.
  ENDMETHOD.

  METHOD verify_logs_prepare_deployment.
    DATA(lt_expected_logs) = VALUE ty_t_sprot_u(
             ( level = 2
               severity = ' '
               langu = sy-langu
               ag = 'SCTS_HOT'
               msgnr = '415' ) ). "Beginn Vorbereitung Redeployment der HANA-Repository-Pakete und -Objekte

    IF it_exp_log_messages IS INITIAL.
      APPEND VALUE sprot_u( level = 3
                            severity = ' '
                            langu = sy-langu
                            ag = 'SCTS_HOT'
                            msgnr = '419' "  &1 Pakete und &2 Objekte im HTA-Repository als zu löschen erstellt
                            var1 = |{ iv_added_packs_for_deletion }|
                            var2 = |{ iv_added_objs_for_deletion }| ) TO lt_expected_logs.

      APPEND VALUE sprot_u( level = 3
                            severity = ' '
                            langu = sy-langu
                            ag = 'SCTS_HOT'
                            msgnr = '420' "  &1 Pakete und &2 Objekte im HTA-Repository als zu deployen gesetzt
                            var1 = |{ iv_set_packs_for_deploy }|
                            var2 = |{ iv_set_objs_for_deploy }| ) TO lt_expected_logs.


    ELSE.
      APPEND LINES OF it_exp_log_messages TO lt_expected_logs.
    ENDIF.

    APPEND VALUE sprot_u( level = 2
                          severity = ' '
                          langu = sy-langu
                          ag = 'SCTS_HOT'
                          msgnr = '416'
                          var1 = COND #( WHEN iv_result = abap_true THEN 'Success'(002) ELSE 'Error'(001) )
                        ) TO lt_expected_logs. "Ende Vorbereitung Redeployment der HANA-Repository-Pakete und -Objekte:&1

    DATA(lt_log_messages) = mr_logger_double->get_log_messages( ).
    cl_abap_unit_assert=>assert_equals( act = lt_log_messages exp = lt_expected_logs msg = 'Actual and expected logs are different' ).
  ENDMETHOD.


  METHOD create_package_in_bck_table.
    DATA(lr_pack) = cl_cts_hot_package=>create_instance( iv_package_id ).

    GET TIME STAMP FIELD DATA(lv_timestamp).

    NEW cl_sql_statement( )->execute_update( |INSERT INTO "CTS_HOT_PACKAGE~BCK" (ABAP_HANA_PACKAGE_ID, ABAP_STATUS, HOT_STATUS, HANA_PACKAGE_ID, HANA_READ_SYSTEM, | &&
                                             |ABAP_SYNC_SYSTEM, ABAP_SYNCED_AT, ABAP_SYNCED_BY, ABAP_DEPLOYED_AT, ABAP_DEPLOYED_BY, HANA_PACK_SRC_SYSTEM, | &&
                                             |HANA_PACK_SRC_TENANT, HANA_PACK_DESCRIPTION, HANA_PACK_RESPONSIBLE, HANA_PACK_ORIG_LANG, HANA_PACK_IS_STRUCTURAL, | &&
                                             |HANA_PACK_DELIVERY_UNIT, HANA_PACK_DU_VENDOR, HANA_PACK_TEXT_COLLECTION, HANA_PACK_TEXT_STATUS, HANA_PACK_TEXT_TERM_DOMAIN, | &&
                                             |HANA_PACK_HINTS_FOR_TRANSL, ABAP_IMPORT_TIMESTAMP, HOT_ACTIVATION_MODE, ABAP_NO_TRANSLATION) | &&
                                             |VALUES ('{ lr_pack->abap_hana_package_id }', '{ iv_abap_status }', '{ iv_hot_status }', '{ lr_pack->hana_package_id }', 'HDB', | &&
                                             |'ABP','{ lv_timestamp }','{ sy-uname }', '0', '', 'HDB', | &&
                                             |'SRC_TENANT', 'PACK_DESCRIPTION', 'PACK_ORIG_LANG', 'PACK_RESPONSIBLE', '0',| &&
                                             |'PACK_DU', 'PACK_DU_VENDOR', 'PACK_TEXT_COLL', 'PACK_TEXT_STATUS', 'PACK_TEXT_TERM_DOMAIN',| &&
                                             |'PACK_HINTS','0','{ iv_hot_activation_mode }','X')| ).
  ENDMETHOD.


  METHOD fill_osql_env_from_bck.
    DATA(lr_result) = NEW cl_sql_statement( )->execute_query( |SELECT * FROM "CTS_HOT_PACKAGE~BCK"| ).
    lr_result->set_param_table( REF #( mt_hot_packages ) ).
    lr_result->next_package( ).
    lr_result->close( ).

    SORT mt_hot_packages BY abap_hana_package_id ASCENDING abap_status ASCENDING.
    gr_osql_env->insert_test_data( mt_hot_packages ).

    lr_result = NEW cl_sql_statement( )->execute_query( |SELECT * FROM "CTS_HOT_OBJECT~BCK"| ).
    lr_result->set_param_table( REF #( mt_hot_objects ) ).
    lr_result->next_package( ).
    lr_result->close( ).

    SORT mt_hot_objects BY abap_hana_package_id ASCENDING abap_hana_object_name_suffix ASCENDING abap_status ASCENDING.
    gr_osql_env->insert_test_data( mt_hot_objects ).
  ENDMETHOD.


  METHOD verify_packages.
    cl_abap_unit_assert=>assert_equals( act = lines( it_act_packages ) exp = lines( it_exp_packages ) ).
    LOOP AT it_exp_packages REFERENCE INTO DATA(lr_exp_pack).
      IF NOT line_exists( it_act_packages[ abap_hana_package_id = lr_exp_pack->abap_hana_package_id abap_status = lr_exp_pack->abap_status hot_status = lr_exp_pack->hot_status ] ).
        cl_abap_unit_assert=>fail( |Expected package { lr_exp_pack->abap_hana_package_id } is missing| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_object_in_bck_table.
    DATA(lr_obj) = cl_cts_hot_object_v1=>create_instance( iv_hana_package_id = iv_package_id
                                                          iv_hana_object_name = iv_object_name
                                                          iv_hana_object_suffix = iv_object_suffix ).

    GET TIME STAMP FIELD DATA(lv_timestamp).

    DATA(lv_bdata) = cl_abap_codepage=>convert_to( 'BDATA' ).
    DATA(lr_stmt) = NEW cl_sql_statement( ).
    lr_stmt->set_param( REF #( lv_bdata ) ).
    lr_stmt->execute_update( |INSERT INTO "CTS_HOT_OBJECT~BCK" (ABAP_HANA_PACKAGE_ID, ABAP_HANA_OBJECT_NAME_SUFFIX, ABAP_STATUS, HOT_STATUS, | &&
                                             |HANA_PACKAGE_ID, HANA_OBJECT_NAME, HANA_OBJECT_SUFFIX, ABAP_SYNC_SYSTEM, HANA_READ_SYSTEM, HANA_SOURCE_OBJECT_VERSION, | &&
                                             |HANA_OBJECT_VERSION, HANA_SOURCE_BUILD_VERSION, HANA_ACTIVATED_AT, HANA_ACTIVATED_BY, ABAP_SYNCED_AT, ABAP_SYNCED_BY, | &&
                                             |ABAP_DEPLOYED_AT, ABAP_DEPLOYED_BY, HANA_CONTENT_BDATA, HANA_CONTENT_CDATA, ABAP_IMPORT_TIMESTAMP, ABAP_OBJECT_REFERENCE) | &&
                                             |VALUES ('{ lr_obj->abap_hana_package_id }', '{ lr_obj->abap_hana_object_name_suffix }', '{ iv_abap_status }', '{ iv_hot_status }', | &&
                                             |'{ lr_obj->hana_package_id }', '{ lr_obj->hana_object_name }', '{ lr_obj->hana_object_suffix }', 'ABP', 'HDB', '11', | &&
                                             |'1', '1.1.1', '20190331090909.0000000', 'ACTIVATED_BY', '{ lv_timestamp }', '{ sy-uname }', | &&
                                             |'0', 'DEPLOYED_BY', ?, 'CDATA', '0', '')| ).
  ENDMETHOD.


  METHOD verify_objects.
    cl_abap_unit_assert=>assert_equals( act = lines( it_act_objects ) exp = lines( it_exp_objects ) ).
    LOOP AT it_exp_objects REFERENCE INTO DATA(lr_exp_obj).
      IF NOT line_exists( it_act_objects[ abap_hana_package_id = lr_exp_obj->abap_hana_package_id abap_hana_object_name_suffix = lr_exp_obj->abap_hana_object_name_suffix abap_status = lr_exp_obj->abap_status hot_status = lr_exp_obj->hot_status ] ).
        cl_abap_unit_assert=>fail( |Expected object { lr_exp_obj->abap_hana_package_id }.{ lr_exp_obj->abap_hana_object_name_suffix } is missing| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_package_in_abap_table.
    DATA(lr_pack) = cl_cts_hot_package=>create_instance( iv_package_id ).

    GET TIME STAMP FIELD DATA(lv_timestamp).
    DATA(ls_pack) = VALUE cts_hot_package( abap_hana_package_id = lr_pack->abap_hana_package_id
                                           abap_status = 'A'
                                           hot_status = iv_hot_status
                                           hana_package_id = lr_pack->hana_package_id
                                           abap_synced_by = 'HTA_REVOKE'
                                           abap_synced_at = lv_timestamp ).
    INSERT cts_hot_package FROM ls_pack.
    APPEND ls_pack TO mt_hot_packages.
  ENDMETHOD.

  METHOD create_object_in_abap_table.
    DATA(lr_obj) = cl_cts_hot_object_v1=>create_instance( iv_hana_package_id = iv_package_id
                                                          iv_hana_object_name = iv_object_name
                                                          iv_hana_object_suffix = iv_object_suffix ).

    GET TIME STAMP FIELD DATA(lv_timestamp).
    DATA(ls_object) = VALUE cts_hot_object( abap_hana_package_id = lr_obj->abap_hana_package_id
                                            abap_hana_object_name_suffix = lr_obj->abap_hana_object_name_suffix
                                            abap_status = 'A'
                                            hot_status = iv_hot_status
                                            hana_package_id = lr_obj->hana_package_id
                                            hana_object_name = lr_obj->hana_object_name
                                            hana_object_suffix = lr_obj->hana_object_suffix
                                            hana_object_version = 1
                                            abap_synced_by = 'HTA_REVOKE'
                                            abap_synced_at = lv_timestamp
                                            hana_content_bdata = cl_abap_codepage=>convert_to( 'BDATA' )
                                            hana_content_cdata = 'CDATA' ).
    INSERT cts_hot_object FROM ls_object.
    APPEND ls_object TO mt_hot_objects.
    SORT mt_hot_objects BY abap_hana_package_id ASCENDING abap_hana_object_name_suffix ASCENDING abap_status ASCENDING.
  ENDMETHOD.

  METHOD verify_hot_status_i_for_objs.
    DATA lv_count TYPE i.

    SELECT COUNT(*) FROM cts_hot_object INTO lv_count WHERE abap_status = 'A'
                                                        AND hot_status NOT IN ( 'D', 'Z', 'I' ).
    cl_abap_unit_assert=>assert_initial( act = lv_count msg = |CTS_HOT_OBJECT contains { lv_count } entries with wrong hot_status| ).

    SELECT COUNT(*) FROM cts_hot_object INTO lv_count WHERE abap_hana_package_id IN ( 'COM.AUNIT.D', 'COM.AUNIT.Z' )
                                                        AND abap_status = 'A'
                                                        AND hot_status IN ( 'D', 'Z' ).
    cl_abap_unit_assert=>assert_equals( act = lv_count exp = 2 ).

    SELECT COUNT(*) FROM cts_hot_object INTO rv_count WHERE abap_status = 'A'
                                                        AND hot_status = 'I'.
  ENDMETHOD.

  METHOD verify_hot_status_i_for_pkgs.
    DATA lv_count TYPE i.

    SELECT COUNT(*) FROM cts_hot_package INTO lv_count WHERE abap_status = 'A'
                                                         AND hot_status NOT IN ( 'D', 'Z', 'I' ).
    cl_abap_unit_assert=>assert_initial( act = lv_count msg = |CTS_HOT_PACKAGE contains { lv_count } entries with wrong hot_status| ).

    SELECT COUNT(*) FROM cts_hot_package INTO lv_count WHERE abap_hana_package_id IN ( 'COM.AUNIT.PACK.D', 'COM.AUNIT.PACK.Z' )
                                                         AND abap_status = 'A'
                                                         AND hot_status IN ( 'D', 'Z' ).
    cl_abap_unit_assert=>assert_equals( act = lv_count exp = 2 ).

    SELECT COUNT(*) FROM cts_hot_package INTO rv_count WHERE abap_status = 'A'
                                                         AND hot_status = 'I'.
  ENDMETHOD.


  METHOD fill_helper_double.
    LOOP AT mt_hot_objects REFERENCE INTO DATA(lr_object) WHERE abap_status = 'A'.
      APPEND VALUE #( package_id = lr_object->hana_package_id
                      object_name = lr_object->hana_object_name
                      object_suffix = lr_object->hana_object_suffix
                      version = lr_object->hana_object_version
                      cdata = lr_object->hana_content_cdata
                      bdata = lr_object->hana_content_bdata
                    ) TO mr_helper_double->mt_hana_repo_data.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_lcl_helper DEFINITION FINAL FOR TESTING DURATION MEDIUM RISK LEVEL DANGEROUS.
  PRIVATE SECTION.
    CONSTANTS:
      co_hana_package   TYPE string VALUE 'tmp.hta.aunit.cts_hot_upg',
      co_attview_name   TYPE string VALUE 'AT_HOT_UPG_AUNIT',
      co_attview_suffix TYPE string VALUE 'attributeview',
      co_jpg_name       TYPE string VALUE 'BINARY_HOT_UPG_AUNIT',
      co_jpg_suffix     TYPE string VALUE 'jpg'.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    CLASS-DATA:
      gr_osql_env TYPE REF TO if_osql_test_environment.

    METHODS:
      "! Check existence of HANA package and objects and if not existing, create them.
      setup RAISING cx_static_check,
      teardown RAISING cx_static_check,
      read_object_version_existing FOR TESTING RAISING cx_static_check,
      read_object_version_not_exstng FOR TESTING RAISING cx_static_check,
      read_object_content_cdata FOR TESTING RAISING cx_static_check,
      read_object_content_bdata FOR TESTING RAISING cx_static_check,
      read_object_content_not_exstng FOR TESTING RAISING cx_static_check,

      "! Create test package in HANA if not existing
      create_package_in_hana
        RAISING cx_static_check,
      "! Create 2 test objects in HANA if not existing. If existing, read the data to CTS_HOT_OBJECT
      create_objects_in_hana
        RAISING cx_static_check,
      create_attview_cdata
        RETURNING
          VALUE(rv_result) TYPE string,
      create_jpg_bdata
        RETURNING
          VALUE(rv_result) TYPE xstring,
      create_jpg_in_cts_hot_object
        RAISING cx_static_check,
      create_view_in_cts_hot_object
        RAISING cx_static_check,
      create_cts_hot_object
        IMPORTING
          ir_object        TYPE REF TO cl_cts_hot_object_v1
        RETURNING
          VALUE(rs_object) TYPE cts_hot_object,
      deploy_hot_objects
        RAISING cx_static_check,
      read_object_data_from_hana
        RAISING cx_static_check.

    DATA:
      mr_cut         TYPE REF TO lcl_helper,
      mt_hot_objects TYPE STANDARD TABLE OF cts_hot_object.
ENDCLASS.

CLASS ltcl_lcl_helper IMPLEMENTATION.

  METHOD class_setup.
    IF sy-dbsys <> 'HDB'.
      cl_abap_unit_assert=>abort( msg = 'Test can only be run on ABAP systems on HDB' ).
    ENDIF.
    gr_osql_env = cl_osql_test_environment=>create( VALUE #( ( 'CTS_HOT_OBJECT' )
                                                             ( 'CTS_HOT_PACKAGE' )
                                                             ( 'TADIR' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    gr_osql_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    me->create_package_in_hana( ).

    me->create_objects_in_hana( ).

    mr_cut = NEW lcl_helper( ).
  ENDMETHOD.

  METHOD teardown.
    gr_osql_env->clear_doubles( ).
  ENDMETHOD.

  METHOD read_object_content_bdata.
    mr_cut->read_hana_object_data(
      EXPORTING
        iv_package_id    = co_hana_package
        iv_object_name   = co_jpg_name
        iv_object_suffix = co_jpg_suffix
      IMPORTING
        ev_bdata = DATA(lv_bdata)
        ev_cdata = DATA(lv_cdata) ).
    cl_abap_unit_assert=>assert_initial( lv_cdata ).
    cl_abap_unit_assert=>assert_equals( act = lv_bdata exp = mt_hot_objects[ hana_object_name = co_jpg_name ]-hana_content_bdata ).

  ENDMETHOD.

  METHOD read_object_content_cdata.
    mr_cut->read_hana_object_data(
      EXPORTING
        iv_package_id    = co_hana_package
        iv_object_name   = co_attview_name
        iv_object_suffix = co_attview_suffix
      IMPORTING
        ev_bdata = DATA(lv_bdata)
        ev_cdata = DATA(lv_cdata) ).
    cl_abap_unit_assert=>assert_initial( lv_bdata ).
    cl_abap_unit_assert=>assert_equals( act = lv_cdata exp = mt_hot_objects[ hana_object_name = co_attview_name ]-hana_content_cdata ).
  ENDMETHOD.

  METHOD read_object_content_not_exstng.
    mr_cut->read_hana_object_data(
      EXPORTING
        iv_package_id    = co_hana_package
        iv_object_name   = 'NOT_EXISTSING_VIEW'
        iv_object_suffix = co_attview_suffix
      IMPORTING
        ev_bdata = DATA(lv_bdata)
        ev_cdata = DATA(lv_cdata) ).
    cl_abap_unit_assert=>assert_initial( lv_bdata ).
    cl_abap_unit_assert=>assert_initial( lv_cdata ).
  ENDMETHOD.

  METHOD read_object_version_existing.
    DATA(lv_version_hana) = mr_cut->read_hana_object_version(
              iv_package_id    = co_hana_package
              iv_object_name   = co_attview_name
              iv_object_suffix = co_attview_suffix ).
    cl_abap_unit_assert=>assert_equals( act = lv_version_hana exp = mt_hot_objects[ hana_object_name = co_attview_name ]-hana_object_version ).

    lv_version_hana = mr_cut->read_hana_object_version(
              iv_package_id    = co_hana_package
              iv_object_name   = co_attview_name
              iv_object_suffix = co_attview_suffix ).
    cl_abap_unit_assert=>assert_equals( act = lv_version_hana exp = mt_hot_objects[ hana_object_name = co_jpg_name ]-hana_object_version ).
  ENDMETHOD.

  METHOD read_object_version_not_exstng.
    DATA(lv_version) = mr_cut->read_hana_object_version(
            iv_package_id    = 'NOT_EXISTING_SOMETHING_PACKAGE'
            iv_object_name   = co_attview_name
            iv_object_suffix = co_attview_suffix ).
    cl_abap_unit_assert=>assert_equals( act = lv_version exp = 0 ).

    lv_version = mr_cut->read_hana_object_version(
            iv_package_id    = co_hana_package
            iv_object_name   = 'NO_EXISTING_OBJECT'
            iv_object_suffix = co_attview_suffix ).
    cl_abap_unit_assert=>assert_equals( act = lv_version exp = 0 ).

    lv_version = mr_cut->read_hana_object_version(
            iv_package_id    = co_hana_package
            iv_object_name   = co_attview_name
            iv_object_suffix = 'NOT_EXISTING_SUFFIX' ).
    cl_abap_unit_assert=>assert_equals( act = lv_version exp = 0 ).
  ENDMETHOD.

  METHOD create_attview_cdata.
    rv_result = '<?xml version="1.0" encoding="UTF-8"?>' &&
                  '<Dimension:dimension xmlns:Dimension="http://www.sap.com/ndb/BiModelDimension.ecore" schemaVersion="1.2" id="' && co_attview_name && '" applyPrivilegeType="ANALYTIC_PRIVILEGE" checkAnalyticPrivileges="true"' &&
                          ' defaultClient="$$client$$" defaultLanguage="$$language$$" visibility="internal" dimensionType="Standard">' &&
                  '  <origin/>' &&
                  '  <descriptions defaultDescription="mein text"/>' &&
                  '  <attributes>' &&
                  '    <attribute id="DELIVERY_UNIT" order="1" attributeHierarchyActive="false" displayAttribute="false">' &&
                  '      <descriptions defaultDescription="DELIVERY_UNIT"/>' &&
                  '      <keyMapping schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS" columnName="DELIVERY_UNIT"/>' &&
                  '    </attribute>' &&
                  '    <attribute id="VERSION" order="3" attributeHierarchyActive="false" displayAttribute="false">' &&
                  '      <descriptions defaultDescription="VERSION"/>' &&
                  '      <keyMapping schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS" columnName="VERSION"/>' &&
                  '    </attribute>' &&
                  '    <attribute id="VENDOR" order="4" attributeHierarchyActive="false" displayAttribute="false">' &&
                  '      <descriptions defaultDescription="VENDOR"/>' &&
                  '      <keyMapping schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS" columnName="VENDOR"/>' &&
                  '    </attribute>' &&
                  '  </attributes>' &&
                  '  <calculatedAttributes/>' &&
                  '  <privateDataFoundation>' &&
                  '    <tableProxies>' &&
                  '      <tableProxy>' &&
                  '        <table schemaName="_SYS_REPO" columnObjectName="DELIVERY_UNITS"/>' &&
                  '      </tableProxy>' &&
                  '    </tableProxies>' &&
                  '    <joins/>' &&
                  '    <layout>' &&
                  '      <shapes>' &&
                  '        <shape modelObjectName="DELIVERY_UNITS" modelObjectNameSpace="_SYS_REPO" modelObjectType="catalog">' &&
                  '          <upperLeftCorner x="70" y="30"/>' &&
                  '        </shape>' &&
                  '        <shape modelObjectName="DataFoundation" modelObjectNameSpace="DataFoundation" modelObjectType="repository">' &&
                  '          <upperLeftCorner x="40" y="85"/>' &&
                  '          <rectangleSize height="0" width="0"/>' &&
                  '        </shape>' &&
                  '      </shapes>' &&
                  '    </layout>' &&
                  '  </privateDataFoundation>' &&
                  '  <hierarchies/>' &&
                  '</Dimension:dimension>'.
  ENDMETHOD.

  METHOD create_view_in_cts_hot_object.
    DATA(lr_object) = cl_cts_hot_object_v1=>create_instance(
                                  iv_hana_package_id = co_hana_package
                                  iv_hana_object_name = co_attview_name
                                  iv_hana_object_suffix = co_attview_suffix ).

    DATA(ls_object) = me->create_cts_hot_object( lr_object ).

    ls_object-hana_content_cdata = me->create_attview_cdata( ).

    MODIFY cts_hot_object FROM ls_object.
  ENDMETHOD.

  METHOD create_jpg_in_cts_hot_object.
    DATA(lr_object) = cl_cts_hot_object_v1=>create_instance(
                                  iv_hana_package_id = co_hana_package
                                  iv_hana_object_name = co_jpg_name
                                  iv_hana_object_suffix = co_jpg_suffix ).

    DATA(ls_object) = me->create_cts_hot_object( lr_object ).

    ls_object-hana_content_bdata = me->create_jpg_bdata( ).

    MODIFY cts_hot_object FROM ls_object.
  ENDMETHOD.


  METHOD create_package_in_hana.
    DATA(lr_hana_connector) = cl_cts_hot_hana_connector=>create_instance( ).
    lr_hana_connector->sync_packages_from_hana_to_hot( VALUE #( ( cl_cts_hot_package=>create_instance( co_hana_package ) ) ) ).

    SELECT COUNT(*) FROM cts_hot_package WHERE hana_package_id = co_hana_package.
    IF sy-dbcnt = 1.
      UPDATE cts_hot_package SET abap_no_translation = 'X' WHERE hana_package_id = co_hana_package. "needed to prevent tadir read for master lang
      RETURN.
    ENDIF.

    DATA(lr_nhi_api) = cl_nhi_api=>create_instance( ).

    DATA(lr_package_api) = lr_nhi_api->get_package( ).
    DATA(lr_exists_resp) = lr_package_api->exists( lr_package_api->create_exists_package_req( tenant = '' package = co_hana_package ) ).

    IF lr_exists_resp->error_code IS INITIAL AND lr_exists_resp->exists = abap_true.
      "package exists, nothing to do
    ELSE.
      "create the package
      DATA(lr_create_resp) = lr_package_api->create( lr_package_api->create_create_package_req(
                                                 tenant                  = ''
                                                 package                 = co_hana_package
                                                 description             = 'Test objects for AUnit tests of HTA'
                                                 responsible             = 'D037590'
                                                 orig_lang               = 'en_US'
                                                 structural              = abap_false
                                                 delivery_unit           = ''
                                                 du_vendor               = ''
                                                 text_collection         = ''
                                                 text_status             = ''
                                                 text_terminology_domain = ''
                                                 hints_for_translation   = ''
                                                 texts                   = VALUE #( )
      ) ).

      IF lr_create_resp->error_code <> '0'.
        cl_abap_unit_assert=>fail( |Error creating package { co_hana_package }: { lr_create_resp->error_code }:{ lr_create_resp->error_msg }| ).
      ENDIF.
      lr_hana_connector->sync_packages_from_hana_to_hot( VALUE #( ( cl_cts_hot_package=>create_instance( co_hana_package ) ) ) ).
      UPDATE cts_hot_package SET abap_no_translation = 'X' WHERE hana_package_id = co_hana_package. "needed to prevent tadir read for master lang
    ENDIF.
  ENDMETHOD.

  METHOD create_jpg_bdata.
    rv_result = cl_abap_codepage=>convert_to( 'This is a JPG' ).
  ENDMETHOD.


  METHOD deploy_hot_objects.
    DATA(lr_hana_connector) = cl_cts_hot_hana_connector=>create_instance( ).
    lr_hana_connector->deploy_objects_to_hana(
      EXPORTING
        i_objects = VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list(
          ( cl_cts_hot_object_v1=>create_instance(
                  iv_hana_package_id = co_hana_package
                  iv_hana_object_name = co_attview_name
                  iv_hana_object_suffix = co_attview_suffix ) )
          ( cl_cts_hot_object_v1=>create_instance(
                  iv_hana_package_id = co_hana_package
                  iv_hana_object_name = co_jpg_name
                  iv_hana_object_suffix = co_jpg_suffix ) ) )
        i_activation_mode            = if_cts_hot_db_access=>co_hot_activation_mode_ok
        i_max_nr_activation_attempts = 1
        i_activate_with_hints        = abap_true
      IMPORTING
        e_successfull_objects = DATA(lt_ok_objects)  ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_ok_objects ) msg = 'Not all test objects could be activated' ).
  ENDMETHOD.

  METHOD create_cts_hot_object.
    rs_object = VALUE cts_hot_object(
        abap_hana_package_id = ir_object->abap_hana_package_id
        abap_hana_object_name_suffix = ir_object->abap_hana_object_name_suffix
        abap_status = 'A'
        hot_status = if_cts_hot_db_access=>co_hot_status_inactive
        hana_package_id = ir_object->hana_package_id
        hana_object_name = ir_object->hana_object_name
        hana_object_suffix = ir_object->hana_object_suffix
        abap_sync_system = 'DEV'
        hana_read_system = 'HAN'
        hana_source_build_version = '1.00.12.00.123456'
        hana_source_object_version = ''
        hana_object_version = ''
        abap_import_timestamp = 0
        hana_activated_by = 'HTA-AUNIT'
        abap_synced_by = 'HTA user' ).

    GET TIME STAMP FIELD rs_object-abap_synced_at.
    GET TIME STAMP FIELD rs_object-hana_activated_at.
  ENDMETHOD.


  METHOD create_objects_in_hana.
    me->read_object_data_from_hana( ).

    SELECT * FROM cts_hot_object INTO TABLE mt_hot_objects
                                 WHERE hana_package_id = co_hana_package
                                   AND ( ( hana_object_name = co_attview_name AND hana_object_suffix = co_attview_suffix )
                                      OR ( hana_object_name = co_jpg_name AND hana_object_suffix = co_jpg_suffix ) ).
    IF lines( mt_hot_objects ) = 2.
      RETURN.
    ENDIF.
    me->create_view_in_cts_hot_object( ).
    me->create_jpg_in_cts_hot_object( ).

    me->deploy_hot_objects( ).

    SELECT * FROM cts_hot_object INTO TABLE mt_hot_objects
                           WHERE hana_package_id = co_hana_package
                             AND ( ( hana_object_name = co_attview_name AND hana_object_suffix = co_attview_suffix )
                                OR ( hana_object_name = co_jpg_name AND hana_object_suffix = co_jpg_suffix ) ).

  ENDMETHOD.


  METHOD read_object_data_from_hana.
    DATA(lr_hana_connector) = cl_cts_hot_hana_connector=>create_instance( ).
    lr_hana_connector->read_objects_from_hana_to_hot(
      VALUE cl_cts_hot_object_v1=>ty_cl_cts_hot_object_list(
          ( cl_cts_hot_object_v1=>create_instance(
                  iv_hana_package_id = co_hana_package
                  iv_hana_object_name = co_attview_name
                  iv_hana_object_suffix = co_attview_suffix ) )
          ( cl_cts_hot_object_v1=>create_instance(
                  iv_hana_package_id = co_hana_package
                  iv_hana_object_name = co_jpg_name
                  iv_hana_object_suffix = co_jpg_suffix ) ) ) ).
  ENDMETHOD.

ENDCLASS.