  METHOD check_upgrade_is_running.
    IF lcl_upgrade=>mo_agent->is_running( ) <> 1.
      RAISE EXCEPTION TYPE cx_cts_hta_hdi
        MESSAGE ID 'SCTS_HOT'
        NUMBER '734'.
    ENDIF.
  ENDMETHOD.