  METHOD is_hot_status_ok_for_sync.
    IF i_hot_status IS INITIAL.
      "object/package does not exist in HTA yet, sync allowed
      r_result = abap_true.
    ELSEIF i_hot_status = if_cts_hot_db_access=>co_hot_status_new
           OR i_hot_status = if_cts_hot_db_access=>co_hot_status_active.
      "object/package exists in HTA with status that allows sync
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.