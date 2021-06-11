  method if_cts_hot_logger~abnormal_termination.
    message( iv_msg_id = iv_msg_id
             iv_level = if_cts_hot_logger=>co_level_2
             iv_msg_nr = iv_msg_nr
             iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
             iv_var1 = iv_var1
             iv_var2 = iv_var2
             iv_var3 = iv_var3
             iv_var4 = iv_var4 ).
  endmethod.