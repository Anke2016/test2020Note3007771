  method message.
    append value sprot_u( ag = cond #( when iv_msg_id is initial
                                         then mv_msg_id
                                         else iv_msg_id )
                          langu = mv_langu
                          level = iv_level
                          msgnr = iv_msg_nr
                          newobj = mv_new_log_section
                          severity = iv_severity
                          var1 = iv_var1
                          var2 = iv_var2
                          var3 = iv_var3
                          var4 = iv_var4 ) to mt_messages.

    me->set_max_severity( exporting iv_severity = iv_severity
                          changing cv_severity = mv_max_severity ).

    "In error cases always do a flush to not loose information if layer on top does no flush. With this, layer on top does not always
    "need to do flush after logging of an error or abnormal termination.
    if iv_severity = if_cts_hot_logger~co_severity_error or iv_severity = if_cts_hot_logger~co_severity_abnormal_terminatn.
      me->if_cts_hot_logger~flush( ).
    endif.

    clear mv_new_log_section.
  endmethod.