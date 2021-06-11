  PRIVATE SECTION.

    DATA mr_cts_hot_db_access TYPE REF TO if_cts_hot_db_access .
    DATA mr_hdi_object_db_access TYPE REF TO if_cts_hdi_object_db_access .
    DATA mv_broken_repo_exists_before TYPE abap_bool .
    DATA mv_broken_hdi_exists_before TYPE abap_bool .
    DATA mr_logger TYPE REF TO if_cts_hot_logger .
    DATA mv_redeploy_mode TYPE char1 .

    METHODS :
      "! <p class="shorttext synchronized" lang="de"></p>
      check_preconditions
        RETURNING
          VALUE(rv_success) TYPE abap_bool ,

      "! <p class="shorttext synchronized" lang="de"></p>
      "! Check if there were any broken objects before deployment started
      "! only schedule job if there were broken objects/packages/HDI objects before the import otherwise job would just redo same activation and fails with same errors.
      chk_prev_broken_object_exist
        RAISING
          cx_cts_hta_hdi ,
      "! <p class="shorttext synchronized" lang="de"></p>
      "! Check if the parameter is set in the CTS_HOT_PARAMS table to start the Afterbrenner
      chk_config_for_afterburner
        RAISING
          cx_cts_hta_hdi ,

      "! <p class="shorttext synchronized" lang="de"></p>
      "! "No job if no broken objects/packages or HDI objects exists or redeploy_mode is not for HANA repo object/HDI objects
      chk_broken_obj_bef_n_aftr_depl
        RETURNING
          VALUE(rv_redpoly) TYPE abap_bool
        RAISING
          cx_cts_hta_hdi ,
      "! <p class="shorttext synchronized" lang="de"></p>
      "! Checks if upgrade is running. Do not create job when import is running in upgrade.
      "! Upgrades should be consistent and will probably not repair any already broken object.
      check_upgrade_is_running
        RAISING
          cx_cts_hta_hdi ,

      "! <p class="shorttext synchronized" lang="de"></p>
      "! Creates job for deployment of all failed objects/packages/HDI objects
      "! Check whether a job is already scheduled but not yet running and do not schedule in this case.
      "!       Otherwise check if a job is already running-->
      "!              we schedule another job as a successor of this running job because there might be
      "!              other error objects or OK objects as in the already running job. The jobs will
      "!              then probably be synchronized in form enqueue_hotas. The later job probably
      "!              initially reads error objects and then waits for enqueue and some of the error
      "!              objects were meanwhile already activated by the first job. This is OK, because
      "!              these objects will be skipped in 2nd job during deployment.
      "!       If no Job is running -->
      "!              Then schedule a new job without any predecessor job
      submit_job
        EXPORTING
          !ev_afterburner_jobcount      TYPE tbtcjob-jobcount
          !ev_redeploy_hana_repo_object TYPE abap_bool
          !ev_redeploy_hdi_object       TYPE abap_bool
          !ev_error_occurred            TYPE abap_bool .