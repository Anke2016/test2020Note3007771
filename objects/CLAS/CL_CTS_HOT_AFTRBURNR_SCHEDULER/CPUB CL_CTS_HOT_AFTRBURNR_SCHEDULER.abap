CLASS cl_cts_hot_aftrburnr_scheduler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      mc_redeploy_jobname  TYPE tbtcjob-jobname VALUE 'REDEPLOY_FAILED_HANA_OBJECTS'.

    METHODS :
      constructor
        IMPORTING
          iv_broken_repo_exists_before TYPE abap_bool DEFAULT abap_false
          iv_broken_hdi_exists_before  TYPE abap_bool DEFAULT abap_false,

      "! Creates job for deployment of all failed objects/packages/HDI objects if certain preconditions are met.
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
      "! @parameter ir_logger | Logger instance from calling program
      "! @parameter ev_afterburner_jobcount | Jobcount of the Reployment job which was scheduled by this class
      "! @parameter ev_job_schedule_error   | Flag for Job schedule error (ev_job_schedule_error= true when job was not scheduled due to error)
      schedule_job
        IMPORTING
          ir_logger               TYPE REF TO if_cts_hot_logger
          iv_transport_logs       TYPE abap_bool DEFAULT abap_false
        EXPORTING
          ev_afterburner_jobcount TYPE btcjobcnt
          ev_job_schedule_error   TYPE abap_bool.
