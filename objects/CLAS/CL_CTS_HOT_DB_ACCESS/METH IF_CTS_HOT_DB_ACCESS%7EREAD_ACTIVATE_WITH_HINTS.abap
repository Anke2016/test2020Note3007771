  METHOD if_cts_hot_db_access~read_activate_with_hints.
    r_result = abap_false. "default HANA_ACTIVATE_WITH_HINTS if not overwritten in CTS_HOT_PARAMS

    SELECT SINGLE value FROM cts_hot_params INTO @DATA(lv_act_with_hints) WHERE name = @if_cts_hot_db_access=>co_name_hana_log_with_hints.
    IF sy-subrc = 0.
      CONDENSE lv_act_with_hints.
      IF lv_act_with_hints = 'Y' OR lv_act_with_hints = 'X'.
        r_result = abap_true.
      ELSEIF lv_act_with_hints = 'N' OR lv_act_with_hints IS INITIAL.
        r_result = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.