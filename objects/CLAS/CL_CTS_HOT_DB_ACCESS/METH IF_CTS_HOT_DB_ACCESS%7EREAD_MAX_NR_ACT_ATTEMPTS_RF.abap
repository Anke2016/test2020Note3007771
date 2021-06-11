  METHOD if_cts_hot_db_access~read_max_nr_act_attempts_rf.
    DATA(lv_default) = 10. "default MAX_NO_ACTIVATION_ATTEMPTS_REDEPL_FAILED if not overwritten in CTS_HOT_PARAMS

    r_result = lv_default.

    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_max_attempts) WHERE name = @if_cts_hot_db_access=>co_name_max_no_act_attempts_rf.
    IF sy-subrc = 0.
      TRY.
          r_result = lv_max_attempts. "try to convert char to i
          IF r_result < 0.
            r_result = 0.
          ENDIF.
        CATCH cx_sy_conversion_no_number.
          r_result = lv_default. "required to set to default because in exception case it was already initialized to 0.
      ENDTRY.
    ENDIF.
  ENDMETHOD.