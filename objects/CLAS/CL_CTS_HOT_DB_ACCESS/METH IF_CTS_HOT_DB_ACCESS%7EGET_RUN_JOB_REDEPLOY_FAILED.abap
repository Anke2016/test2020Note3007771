  METHOD if_cts_hot_db_access~get_run_job_redeploy_failed.
    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_data) WHERE name = @if_cts_hot_db_access=>co_name_run_job_redeploy_faild.
    CONDENSE lv_data.
    IF sy-subrc = 0.
      r_result = lv_data(1).
    ELSE.
      r_result = 'X'. "if parameter not set, use default, 'X'
    ENDIF.
  ENDMETHOD.