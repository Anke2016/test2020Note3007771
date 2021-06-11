  METHOD if_cts_hot_db_access~read_max_nr_of_chars_for_log.
    DATA(lv_default) = 10000. "default MAX_NO_OF_CHARS_LOGGED_PER_ERROR_WARNING if not overwritten in CTS_HOT_PARAMS

    r_result = lv_default.

    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_max_no_of_chars) WHERE name = @if_cts_hot_db_access=>co_name_max_no_chars_for_log.
    IF sy-subrc = 0.
      TRY.
          r_result = lv_max_no_of_chars. "try to convert char to i
          IF r_result < 310. "default of non error messages, resulting in a maximum of 3 log lines per message
            r_result = 310.
          ENDIF.
        CATCH cx_sy_conversion_no_number.
          r_result = lv_default. "required to set to default because in exception case it was already initialized to 0.
      ENDTRY.
    ENDIF.
  ENDMETHOD.