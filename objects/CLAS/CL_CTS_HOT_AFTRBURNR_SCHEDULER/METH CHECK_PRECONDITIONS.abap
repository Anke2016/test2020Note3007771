  METHOD check_preconditions.
    TRY.

        "do not create job when import is running in upgrade. Upgrades should be consistent and will probably not repair any already broken object.
        me->check_upgrade_is_running( ).

        "only schedule job if there were broken objects/packages/HDI objects before the import otherwise job would just redo same activation and fails with same errors.
        me->chk_prev_broken_object_exist( ).

        "Check if the parameter is set in the CTS_HOT_PARAMS table
        me->chk_config_for_afterburner( ).

        "Check if the parameter is set in the CTS_HOT_PARAMS table is correct,
        "i.e RUN_JOB_REDEPLOY_FAILED_HANA_OBJECTS=H when HDI afterburner is to be started,
        "RUN_JOB_REDEPLOY_FAILED_HANA_OBJECTS ='R' when HANA Repo Afterburner is to be started
        "RUN_JOB_REDEPLOY_FAILED_HANA_OBJECTS ='X' when both HDI and HANA repo Aftreburner is to be started
        me->chk_broken_obj_bef_n_aftr_depl( ).

        rv_success = abap_true.

      CATCH cx_cts_hta_hdi INTO DATA(lx_hdi).

        mr_logger->message(
          iv_msg_id   = lx_hdi->if_t100_message~t100key-msgid
          iv_msg_nr   = CONV #( lx_hdi->if_t100_message~t100key-msgno )
          iv_level    = if_cts_hot_logger=>co_level_4
          iv_severity = if_cts_hot_logger=>co_severity_info
          iv_var1     = lx_hdi->message_variable_1
          iv_var2     = lx_hdi->message_variable_2
          iv_var3     = lx_hdi->message_variable_3
          iv_var4     = lx_hdi->message_variable_4
        ).

    ENDTRY.

  ENDMETHOD.