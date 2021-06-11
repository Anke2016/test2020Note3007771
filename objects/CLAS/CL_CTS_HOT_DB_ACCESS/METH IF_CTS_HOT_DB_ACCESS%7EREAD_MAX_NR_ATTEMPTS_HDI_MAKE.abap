  METHOD if_cts_hot_db_access~read_max_nr_attempts_hdi_make.
    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_max_attempts) WHERE name = @if_cts_hot_db_access=>co_name_max_no_att_hdi_make.
    IF sy-subrc = 0.
      TRY.
          rv_max_attempts = lv_max_attempts. "try to convert char to i
          IF rv_max_attempts < 0.
            rv_max_attempts = 0.
          ENDIF.
        CATCH cx_sy_conversion_no_number.
          rv_max_attempts = 10. "required to set to default because in exception case it was already initialized to 0.
      ENDTRY.
    ELSE.
      rv_max_attempts = me->if_cts_hot_db_access~read_max_nr_activation_atempts( ).
    ENDIF.
  ENDMETHOD.