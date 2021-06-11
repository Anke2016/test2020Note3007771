*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_afterburner_job.
  METHODS:
    "! Selects the jobs in status:
    "! <ul>
    "! <li>Ready(Y)</li>
    "! <li>Scheduled(P)</li>
    "! <li>Released(S)</li>
    "! <li>Running(R)</li>
    "! </ul>
    "! @parameter iv_program_name | Program name
    "! @parameter rt_active_job | List of active jobs
    get_active_jobs
      IMPORTING
                iv_program_name      TYPE sy-repid
      RETURNING
                VALUE(rt_active_job) TYPE tbtcjob_tt
      RAISING   cx_cts_hta_hdi ,

    create_job
      IMPORTING
                iv_jobname                   TYPE tbtcjob-jobname
                iv_redeploy_hana_repo_object TYPE abap_bool
                iv_redeploy_hdi_object       TYPE abap_bool
                iv_predecessor_jobname       TYPE tbtcjob-jobname OPTIONAL
                iv_predecessor_jobcount      TYPE tbtcjob-jobcount OPTIONAL
      CHANGING  cr_logger                    TYPE REF TO if_cts_hot_logger
      RETURNING
                VALUE(rv_jobcount)           TYPE tbtcjob-jobcount

      RAISING   cx_cts_hta_hdi .
ENDINTERFACE.

INTERFACE lif_upgrade.
  METHODS:
    is_running
      RETURNING
        VALUE(rv_result) TYPE sysubrc.
ENDINTERFACE.