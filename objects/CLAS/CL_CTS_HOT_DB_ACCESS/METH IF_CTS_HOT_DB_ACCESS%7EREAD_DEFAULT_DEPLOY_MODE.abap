  METHOD if_cts_hot_db_access~read_default_deploy_mode.
    r_result = if_cts_hot_db_access~co_hot_deploy_mode_always. "default deploy_mode if not overwritten in CTS_HOT_PARAMS

    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_deploy_mode) WHERE name = 'DEPLOY_MODE_DEFAULT'.
    CONDENSE lv_deploy_mode.
    IF lv_deploy_mode = if_cts_hot_db_access~co_hot_deploy_mode_prework.
      r_result = if_cts_hot_db_access~co_hot_deploy_mode_prework.
    ENDIF.
  ENDMETHOD.