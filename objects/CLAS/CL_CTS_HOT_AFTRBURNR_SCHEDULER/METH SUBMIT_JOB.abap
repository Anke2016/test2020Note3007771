  METHOD submit_job.
    DATA: lv_msg_text TYPE string.

    CLEAR:
      ev_afterburner_jobcount,
      ev_redeploy_hana_repo_object,
      ev_redeploy_hdi_object,
      ev_error_occurred.

    TRY.
        DATA(lt_active_job) = lcl_afterburner_job_api=>agent->get_active_jobs( iv_program_name = 'SCTS_HTA_REDEPLOY_FAILED' ).

        " Check for job in status Ready, Scheduled or Released
        LOOP AT lt_active_job ASSIGNING FIELD-SYMBOL(<fs_scheduled_job_count>)
          WHERE status = tybtc_ready OR status = tybtc_scheduled OR status = tybtc_released.
          EXIT.
        ENDLOOP.

        IF sy-subrc <> 0. " When no such job is found ...

          ev_redeploy_hana_repo_object = COND #( WHEN ( mv_broken_repo_exists_before = abap_true AND ( me->mv_redeploy_mode = 'X' OR  me->mv_redeploy_mode = 'R' ) ) THEN abap_true ).
          ev_redeploy_hdi_object       = SWITCH #( me->mv_redeploy_mode WHEN 'X' OR 'H' THEN abap_true ).

          TRY.
              DATA(ls_running_job_count) = lt_active_job[ status = tybtc_running ]. " ... then check for a running job

              " If a running job is found, then schedule a successor job
              ev_afterburner_jobcount = lcl_afterburner_job_api=>agent->create_job(
                                          EXPORTING
                                            iv_jobname                   = mc_redeploy_jobname
                                            iv_redeploy_hana_repo_object = ev_redeploy_hana_repo_object
                                            iv_redeploy_hdi_object       = ev_redeploy_hdi_object
                                            iv_predecessor_jobcount      = ls_running_job_count-jobcount
                                            iv_predecessor_jobname       = ls_running_job_count-jobname
                                          CHANGING
                                            cr_logger = mr_logger
                                        ).

            CATCH cx_sy_itab_line_not_found.

              " If no running job is found, then schedule a new job
              ev_afterburner_jobcount = lcl_afterburner_job_api=>agent->create_job(
                                          EXPORTING
                                            iv_jobname                   = mc_redeploy_jobname
                                            iv_redeploy_hana_repo_object = ev_redeploy_hana_repo_object
                                            iv_redeploy_hdi_object       = ev_redeploy_hdi_object
                                          CHANGING
                                            cr_logger = mr_logger
                                        ).
          ENDTRY.
        ELSE.

          MESSAGE i736(scts_hot) WITH <fs_scheduled_job_count>-jobcount INTO lv_msg_text.
          mr_logger->info_level_3(
           EXPORTING
              iv_msg_id = sy-msgid
              iv_msg_nr = CONV #( sy-msgno )
              iv_var1 = sy-msgv1 ).
        ENDIF.

      CATCH cx_cts_hta_hdi INTO DATA(lx_hdi).

        ev_error_occurred = abap_true.
        mr_logger->warning_exception( lx_hdi ).

    ENDTRY.

  ENDMETHOD.