  METHOD if_cts_hot_db_access~read_activation_mode_rf.
    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_activation_mode) WHERE name = @if_cts_hot_db_access=>co_name_hot_activation_mode_rf.
    CONDENSE lv_activation_mode.
    CASE lv_activation_mode.
      WHEN if_cts_hot_db_access~co_hot_activation_mode_all.
        r_result = if_cts_hot_db_access~co_hot_activation_mode_all.
      WHEN if_cts_hot_db_access~co_hot_activation_mode_ok.
        r_result = if_cts_hot_db_access~co_hot_activation_mode_ok.
      WHEN if_cts_hot_db_access~co_hot_activation_mode_ok_rec.
        r_result = if_cts_hot_db_access~co_hot_activation_mode_ok_rec.
      WHEN OTHERS.
        "default activation_mode if not overwritten in CTS_HOT_PARAMS
        "if changed here, also change message SCTS_HOT 622
        r_result = if_cts_hot_db_access~co_hot_activation_mode_ok.
    ENDCASE.
  ENDMETHOD.