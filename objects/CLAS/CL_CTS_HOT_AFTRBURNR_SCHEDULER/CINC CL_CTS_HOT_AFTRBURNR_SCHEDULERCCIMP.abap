*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS ltc_scheduler DEFINITION DEFERRED.

CLASS lcl_afterburner_job_api DEFINITION CREATE PROTECTED
  FRIENDS ltc_scheduler.

  PUBLIC SECTION.
    INTERFACES:
      lif_afterburner_job.
    CLASS-DATA agent TYPE REF TO lif_afterburner_job READ-ONLY.
    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_afterburner_job_api IMPLEMENTATION.

  METHOD class_constructor.
    agent = NEW lcl_afterburner_job_api( ).
  ENDMETHOD.

  METHOD lif_afterburner_job~get_active_jobs.

    SELECT *
      FROM tbtcp AS step INNER JOIN tbtco AS job
        ON job~jobcount = step~jobcount AND
           job~jobname  = step~jobname
        WHERE
            step~progname = @iv_program_name
        AND job~status IN ( @tybtc_ready,
                            @tybtc_scheduled,
                            @tybtc_released,
                            @tybtc_running
                          )
        INTO CORRESPONDING FIELDS OF TABLE @rt_active_job.

  ENDMETHOD.

  METHOD lif_afterburner_job~create_job.
    DATA: lv_msg_text TYPE string.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = iv_jobname
      IMPORTING
        jobcount         = rv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HOT'
        NUMBER '731'.
    ENDIF.

    SUBMIT scts_hta_redeploy_failed LINE-COUNT 65 LINE-SIZE 255
      VIA JOB iv_jobname
      NUMBER rv_jobcount
      WITH p_repo = iv_redeploy_hana_repo_object
      WITH p_hdi = iv_redeploy_hdi_object
      AND RETURN .

    IF iv_predecessor_jobcount IS NOT INITIAL AND iv_predecessor_jobname IS NOT INITIAL .
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = rv_jobcount
          jobname              = iv_jobname
          pred_jobcount        = iv_predecessor_jobcount
          pred_jobname         = iv_predecessor_jobname
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          invalid_time_zone    = 9
          OTHERS               = 10.
    ELSE.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = rv_jobcount
          jobname              = iv_jobname
          strtimmed            = abap_true
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          invalid_time_zone    = 9
          OTHERS               = 10.
    ENDIF.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HOT'
        NUMBER '731'.
    ELSE.

      DATA(lv_redeploy_hdi_object)       = SWITCH #( iv_redeploy_hdi_object       WHEN ' ' THEN '-' ELSE iv_redeploy_hdi_object ).
      DATA(lv_redeploy_hana_repo_object) = SWITCH #( iv_redeploy_hana_repo_object WHEN ' ' THEN '-' ELSE iv_redeploy_hana_repo_object ).

      MESSAGE i735(scts_hot) WITH lv_redeploy_hdi_object lv_redeploy_hana_repo_object rv_jobcount INTO lv_msg_text.
      cr_logger->message(
                iv_msg_id   = sy-msgid
                iv_msg_nr   = CONV #( sy-msgno )
                iv_level    = if_cts_hot_logger=>co_level_3
                iv_severity = if_cts_hot_logger=>co_severity_info
                iv_var1     = sy-msgv1
                iv_var2     = sy-msgv2
                iv_var3     = sy-msgv3 ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_upgrade DEFINITION CREATE PRIVATE
  FRIENDS ltc_scheduler.

  PUBLIC SECTION.
    INTERFACES: lif_upgrade.
    CLASS-DATA mo_agent TYPE REF TO lif_upgrade READ-ONLY.
    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_upgrade IMPLEMENTATION.

  METHOD class_constructor.
    mo_agent = NEW lcl_upgrade( ).
  ENDMETHOD.

  METHOD lif_upgrade~is_running.

    CALL FUNCTION 'SUBST_GET_ACTIVE_UVERS_ENTRY'
      EXCEPTIONS
        no_exchange_active = 1 "no upgrade running
        OTHERS             = 2.

    rv_result = sy-subrc.
  ENDMETHOD.

ENDCLASS.