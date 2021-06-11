*"* use this source file for your ABAP unit test classes

CLASS:
  cl_cts_hot_aftrburnr_scheduler DEFINITION LOCAL FRIENDS ltc_scheduler.

"! Test double for the job API
CLASS ltd_job_api DEFINITION CREATE PUBLIC
  INHERITING FROM lcl_afterburner_job_api
  FOR TESTING
  FRIENDS ltc_scheduler.

  PUBLIC SECTION.
    CONSTANTS mc_dummy_jobcount TYPE btcjobcnt VALUE '007'.
    DATA:
      mv_predecessor_jobname  TYPE tbtcjob-jobname READ-ONLY,
      mv_predecessor_jobcount TYPE tbtcjob-jobcount READ-ONLY.
    METHODS:
      lif_afterburner_job~create_job REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mv_job_create_failed TYPE abap_bool.


ENDCLASS.

CLASS ltd_job_api IMPLEMENTATION.

  METHOD lif_afterburner_job~create_job.
    IF mv_job_create_failed = abap_false.
      mv_predecessor_jobname = iv_predecessor_jobname.
      mv_predecessor_jobcount = iv_predecessor_jobcount.
      rv_jobcount = mc_dummy_jobcount.
    ELSE.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HOT'
        NUMBER '731'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_upgrade DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: lif_upgrade.
    METHODS constructor
      IMPORTING
        iv_upgrade_is_running TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mv_upgrade_is_running TYPE abap_bool.

ENDCLASS.

CLASS ltd_upgrade IMPLEMENTATION.

  METHOD constructor.

    me->mv_upgrade_is_running = iv_upgrade_is_running.

  ENDMETHOD.

  METHOD lif_upgrade~is_running.
    rv_result = SWITCH #( mv_upgrade_is_running
                  WHEN abap_false THEN 1
                  ELSE 0
                ).
  ENDMETHOD.

ENDCLASS.

"! Scheduler test class
CLASS ltc_scheduler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CLASS-DATA mo_mock_sql TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    DATA:
      mr_cut              TYPE REF TO cl_cts_hot_aftrburnr_scheduler,
      mo_mock_job_api     TYPE REF TO ltd_job_api,
      mt_tbtco            TYPE HASHED TABLE OF tbtco WITH UNIQUE KEY jobname jobcount,
      mt_tbtcp            TYPE HASHED TABLE OF tbtcp WITH UNIQUE KEY jobname jobcount stepcount,
      mr_cts_hot_dao_mock TYPE REF TO if_cts_hot_db_access,
      mr_hdi_obj_dao_mock TYPE REF TO if_cts_hdi_object_db_access,
      mr_logger_mock      TYPE REF TO if_cts_hot_logger.

    METHODS:
      "! Set the preconditions for the job to be scheduled
      set_preconditions,
      setup,

      "! It should not schedule a new job when a job is already scheduled
      no_create_if_job_is_schedld FOR TESTING RAISING cx_static_check,

      "! It should create a new job when no jobs are running
      create_new_if_no_job_running FOR TESTING RAISING cx_static_check,

      "! It should create successor job when a job is running
      crea_successor_if_job_running FOR TESTING RAISING cx_static_check,

      "! Raise error when job could not be scheduled
      error_if_scheduling_failed FOR TESTING RAISING cx_static_check,

      "! AfterBurner job should not be scheduled when upgrade is running
      no_schdl_if_upgrade_running FOR TESTING RAISING cx_static_check,

      "! AfterBurner job should not be scheduled if no broken objects exist
      no_schdl_if_no_broken_obj FOR TESTING RAISING cx_static_check,

      "! AfterBurner job should not be scheduled if no configuration exist
      no_schdl_if_no_config_exist FOR TESTING RAISING cx_static_check,

      "! AfterBurner job should not be scheduled if no broken HDI Object after deployment
      no_schdl_no_broknobj_aftr_dply FOR TESTING RAISING cx_static_check
      .
ENDCLASS.

CLASS ltc_scheduler IMPLEMENTATION.

  METHOD class_setup.
    mo_mock_sql = cl_osql_test_environment=>create( VALUE #(
                                                        ( 'TBTCO' )
                                                        ( 'TBTCP' )
                                                    ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_mock_sql->destroy( ).
    cl_osql_test_environment=>garbage_collection( ).
  ENDMETHOD.

  METHOD setup.
    mr_cut = NEW cl_cts_hot_aftrburnr_scheduler( ).

    " Create mockers
    mr_cts_hot_dao_mock = CAST #( cl_abap_testdouble=>create( 'IF_CTS_HOT_DB_ACCESS' ) ).
    mr_hdi_obj_dao_mock = CAST #( cl_abap_testdouble=>create( 'IF_CTS_HDI_OBJECT_DB_ACCESS' ) ).
    mr_logger_mock = CAST #( cl_abap_testdouble=>create( 'IF_CTS_HOT_LOGGER' ) ).

    " Inject mockers
    mr_cut->mr_cts_hot_db_access = mr_cts_hot_dao_mock.
    mr_cut->mr_hdi_object_db_access = mr_hdi_obj_dao_mock.
    lcl_afterburner_job_api=>agent = mo_mock_job_api = NEW ltd_job_api( ).
    lcl_upgrade=>mo_agent = NEW ltd_upgrade( iv_upgrade_is_running = abap_false ). " ... No Upgrade is running

    " ... Broken objects exist
    mr_cut->mv_broken_hdi_exists_before = mr_cut->mv_broken_repo_exists_before = abap_true.

    mo_mock_sql->clear_doubles( ).
  ENDMETHOD.

  METHOD no_create_if_job_is_schedld.
    " Given job is scheduled
    mt_tbtco = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '999' status = tybtc_scheduled ) ).
    mt_tbtcp = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '999' stepcount = 1 progname = 'SCTS_HTA_REDEPLOY_FAILED' ) ).

    mo_mock_sql->insert_test_data(: mt_tbtco ), mt_tbtcp ).

    set_preconditions( ).

    " When scheduling the job
    mr_cut->schedule_job(
      EXPORTING
        ir_logger               = mr_logger_mock
      IMPORTING
        ev_afterburner_jobcount = DATA(lv_jobcount)
    ).

    " Then no job should be scheduled
    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act              = lv_jobcount
        msg              = |It should not schedule a new job when a job is scheduled|
    ).

    cl_abap_testdouble=>verify_expectations(: double = mr_cts_hot_dao_mock ) ,
                                              double = mr_hdi_obj_dao_mock ).
  ENDMETHOD.

  METHOD create_new_if_no_job_running.
    " Given job is finished
    mt_tbtco = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '999' status = tybtc_finished ) ).
    mt_tbtcp = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '999' stepcount = 1 progname = 'SCTS_HTA_REDEPLOY_FAILED' ) ).

    mo_mock_sql->insert_test_data(: mt_tbtco ), mt_tbtcp ).

    set_preconditions( ).

    " When scheduling the job
    mr_cut->schedule_job(
      EXPORTING
        ir_logger               = mr_logger_mock
      IMPORTING
        ev_afterburner_jobcount = DATA(lv_jobcount)
    ).

    " Then a new job should be scheduled
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act              = lv_jobcount
        exp              = ltd_job_api=>mc_dummy_jobcount
        msg              = |It should create a new job when no jobs are running|
    ).
  ENDMETHOD.

  METHOD crea_successor_if_job_running.
    " Given job is finished
    mt_tbtco = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '099' status = tybtc_running ) ).
    mt_tbtcp = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '099' stepcount = 1 progname = 'SCTS_HTA_REDEPLOY_FAILED' ) ).

    mo_mock_sql->insert_test_data(: mt_tbtco ), mt_tbtcp ).

    set_preconditions( ).

    " When scheduling the job
    mr_cut->schedule_job(
      EXPORTING
        ir_logger               = mr_logger_mock
      IMPORTING
        ev_afterburner_jobcount = DATA(lv_jobcount)
    ).

    " Then...

    " ... predecessor job name should be passed
    cl_abap_unit_assert=>assert_equals(
      msg = 'Predecessor job name is passed'
      act = mo_mock_job_api->mv_predecessor_jobname
      exp = 'REDEPLOY_FAILED_HANA_OBJECTS'
      quit = if_aunit_constants=>quit-no ).

    " ... predecessor job count should passed
    cl_abap_unit_assert=>assert_equals(
      msg = 'Predecessor job count is passed'
      act = mo_mock_job_api->mv_predecessor_jobcount
      exp = '099'
      quit = if_aunit_constants=>quit-no ).

    "... a new job should be scheduled
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act              = lv_jobcount
        exp              = ltd_job_api=>mc_dummy_jobcount
        msg              = |It should create a successor job when no jobs are running|
    ).
  ENDMETHOD.

  METHOD error_if_scheduling_failed.

    " Given...
    " ... job is finished & new job shall be scheduled
    mt_tbtco = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '999' status = tybtc_finished ) ).
    mt_tbtcp = VALUE #( ( jobname = 'REDEPLOY_FAILED_HANA_OBJECTS' jobcount = '999' stepcount = 1 progname = 'SCTS_HTA_REDEPLOY_FAILED' ) ).

    mo_mock_sql->insert_test_data(: mt_tbtco ), mt_tbtcp ).

    set_preconditions( ).

    " ... job scheduling has failed
    CAST ltd_job_api( mo_mock_job_api )->mv_job_create_failed = abap_true.

    " When scheduling the job
    mr_cut->schedule_job(
      EXPORTING
        ir_logger             = mr_logger_mock
      IMPORTING
        ev_job_schedule_error = DATA(lv_job_schedule_error)
    ).

    " Then error should be raised when scheduling has failed
    cl_abap_unit_assert=>assert_true(
      act              = lv_job_schedule_error
      msg              = |Error should be raised when scheduling has failed|
    ).

  ENDMETHOD.

  METHOD set_preconditions.

    " Given...

    " ... Previous broken HDI Object exists
    mr_cut->mv_broken_hdi_exists_before = abap_true.

    " ... AfterBurner config is defined
    cl_abap_testdouble=>configure_call( double = mr_cts_hot_dao_mock
                                      )->returning( 'X'
                                      )->ignore_all_parameters(
                                      )->and_expect( )->is_called_once( ).
    mr_cts_hot_dao_mock->get_run_job_redeploy_failed( ).

    " ... Broken object exists after deployment
    cl_abap_testdouble=>configure_call( double = mr_hdi_obj_dao_mock
                                      )->returning( abap_true
                                      )->ignore_all_parameters(
                                      )->and_expect( )->is_called_once( ).
    mr_hdi_obj_dao_mock->exists_broken_object( ).

  ENDMETHOD.

  METHOD no_schdl_if_upgrade_running.

    " Given...

    " ... Upgrade is running
    lcl_upgrade=>mo_agent = NEW ltd_upgrade( iv_upgrade_is_running = abap_true ).

    cl_abap_testdouble=>configure_call( double = mr_logger_mock
                                      )->and_expect( )->is_called_once( ).
    mr_logger_mock->message(
          iv_msg_id   = 'SCTS_HOT'
          iv_msg_nr   = '734'
          iv_level    = if_cts_hot_logger=>co_level_4
          iv_severity = if_cts_hot_logger=>co_severity_info
          iv_var1     = VALUE #( )
          iv_var2     = VALUE #( )
          iv_var3     = VALUE #( )
          iv_var4     = VALUE #( )
    ).

    " When
    mr_cut->schedule_job(
      EXPORTING
        ir_logger               = mr_logger_mock
      IMPORTING
        ev_afterburner_jobcount = DATA(lv_afterburner_jobcount)
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act              = lv_afterburner_jobcount
      msg              = |AfterBurner job should not be scheduled when upgrade is running|
    ).

    cl_abap_testdouble=>verify_expectations( double = mr_logger_mock ).

  ENDMETHOD.

  METHOD no_schdl_if_no_broken_obj.

    " Given ...
    " ... No broken objects exist
    mr_cut->mv_broken_hdi_exists_before = mr_cut->mv_broken_repo_exists_before = abap_false.

    cl_abap_testdouble=>configure_call( double = mr_logger_mock
                                      )->and_expect( )->is_called_once( ).
    mr_logger_mock->message(
          iv_msg_id   = 'SCTS_HOT'
          iv_msg_nr   = '732'
          iv_level    = if_cts_hot_logger=>co_level_4
          iv_severity = if_cts_hot_logger=>co_severity_info
          iv_var1     = VALUE #( )
          iv_var2     = VALUE #( )
          iv_var3     = VALUE #( )
          iv_var4     = VALUE #( )
    ).

    " When
    mr_cut->schedule_job(
      EXPORTING
        ir_logger               = mr_logger_mock
      IMPORTING
        ev_afterburner_jobcount = DATA(lv_afterburner_jobcount)
    ).

    cl_abap_unit_assert=>assert_initial(
      act              = lv_afterburner_jobcount
      msg              = |AfterBurner job should not be scheduled if no broken objects exist|
    ).

    cl_abap_testdouble=>verify_expectations( double = mr_logger_mock ).

  ENDMETHOD.

  METHOD no_schdl_if_no_config_exist.

    " Given ...

    " ... AfterBurner config is not defined
    cl_abap_testdouble=>configure_call( double = mr_cts_hot_dao_mock
                                      )->returning( space
                                      )->ignore_all_parameters(
                                      )->and_expect( )->is_called_once( ).
    mr_cts_hot_dao_mock->get_run_job_redeploy_failed( ).


    cl_abap_testdouble=>configure_call( double = mr_logger_mock
                                      )->and_expect( )->is_called_once( ).
    mr_logger_mock->message(
          iv_msg_id   = 'SCTS_HOT'
          iv_msg_nr   = '733'
          iv_level    = if_cts_hot_logger=>co_level_4
          iv_severity = if_cts_hot_logger=>co_severity_info
          iv_var1     = VALUE #( )
          iv_var2     = VALUE #( )
          iv_var3     = VALUE #( )
          iv_var4     = VALUE #( )
    ).

    " When
    mr_cut->schedule_job(
      EXPORTING
        ir_logger               = mr_logger_mock
      IMPORTING
        ev_afterburner_jobcount = DATA(lv_afterburner_jobcount)
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act              = lv_afterburner_jobcount
      msg              = |AfterBurner job should not be scheduled if no configuration exists|
    ).

    cl_abap_testdouble=>verify_expectations( double = mr_logger_mock ).

  ENDMETHOD.

  METHOD no_schdl_no_broknobj_aftr_dply.

    " Given...

    " ... Previous broken HDI Object exists
    mr_cut->mv_broken_hdi_exists_before = abap_true.

    " ... AfterBurner config is defined
    cl_abap_testdouble=>configure_call( double = mr_cts_hot_dao_mock
                                      )->returning( 'X'
                                      )->ignore_all_parameters(
                                      )->and_expect( )->is_called_once( ).
    mr_cts_hot_dao_mock->get_run_job_redeploy_failed( ).

    " ... No broken object exists after deployment
    cl_abap_testdouble=>configure_call( double = mr_hdi_obj_dao_mock
                                      )->returning( abap_false
                                      )->ignore_all_parameters(
                                      )->and_expect( )->is_called_once( ).
    mr_hdi_obj_dao_mock->exists_broken_object( ).

    cl_abap_testdouble=>configure_call( double = mr_logger_mock
                                      )->and_expect( )->is_called_once( ).
    mr_logger_mock->message(
          iv_msg_id   = 'SCTS_HOT'
          iv_msg_nr   = '733'
          iv_level    = if_cts_hot_logger=>co_level_4
          iv_severity = if_cts_hot_logger=>co_severity_info
          iv_var1     = VALUE #( )
          iv_var2     = VALUE #( )
          iv_var3     = VALUE #( )
          iv_var4     = VALUE #( )
    ).

    " When
    mr_cut->schedule_job(
      EXPORTING
        ir_logger               = mr_logger_mock
      IMPORTING
        ev_afterburner_jobcount = DATA(lv_afterburner_jobcount)
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act              = lv_afterburner_jobcount
      msg              = |AfterBurner job should not be scheduled if no broken HDI object exists after deployment|
    ).

    cl_abap_testdouble=>verify_expectations( double = mr_logger_mock ).


  ENDMETHOD.

ENDCLASS.