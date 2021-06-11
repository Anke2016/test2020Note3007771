  method if_cts_hot_logger~long_text.
    if mv_msg_id = 'SCTS_HDI' or iv_msg_id = 'SCTS_HDI'.
      data(lt_messages) = lcl_log_helper=>split_message( iv_text ).
      loop at lt_messages reference into data(lr_message).
        message( iv_level = iv_level
                 iv_msg_id = iv_msg_id
                 iv_msg_nr = iv_msg_nr
                 iv_severity = iv_severity
                 iv_var1 = lr_message->var1
                 iv_var2 = lr_message->var2
                 iv_var3     = lr_message->var3 ).
      endloop.
    else.
      data(lt_messages_scts_hot) = lcl_log_helper_4_scts_hot=>split_message( iv_text ).
      loop at lt_messages_scts_hot reference into data(lr_message_scts_hot).
        message( iv_level = iv_level
                 iv_msg_id = 'SCTS_HOT'
                 iv_msg_nr = iv_msg_nr
                 iv_severity = iv_severity
                 iv_var1 = conv #( lr_message_scts_hot->var1 )
                 iv_var2 = conv #( lr_message_scts_hot->var2 )
                 iv_var3 = conv #( lr_message_scts_hot->var3 )
                 iv_var4 = conv #( lr_message_scts_hot->var4 ) ).
      endloop.
    endif.
  endmethod.