  METHOD chk_config_for_afterburner.

    me->mv_redeploy_mode = me->mr_cts_hot_db_access->get_run_job_redeploy_failed( ) .
    IF me->mv_redeploy_mode CN 'XRH'.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HOT'
        NUMBER '733'.
    ENDIF.
  ENDMETHOD.