  method if_cts_hot_logger~abnormal_termination_exception.
    data: lr_cast_exception type ref to cx_cts_exception.
    if ix_exception is instance of cx_cts_exception.
      lr_cast_exception ?= ix_exception.
      lr_cast_exception->get_msg(  ).
      me->message(
        exporting
          iv_msg_id   = lr_cast_exception->if_t100_message~t100key-msgid
          iv_msg_nr   = conv #( lr_cast_exception->if_t100_message~t100key-msgno )
          iv_level    = if_cts_hot_logger=>co_level_2
          iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
          iv_var1     = lr_cast_exception->message_variable_1
          iv_var2     = lr_cast_exception->message_variable_2
          iv_var3     = lr_cast_exception->message_variable_3
          iv_var4     = lr_cast_exception->message_variable_4 ).
    else.
      me->if_cts_hot_logger~long_text( iv_level = if_cts_hot_logger=>co_level_2
                                       iv_severity =  if_cts_hot_logger=>co_severity_abnormal_terminatn
                                       iv_text = ix_exception->get_text( ) ).

      DATA(lv_long_text) = ix_exception->get_longtext( ).
      IF lv_long_text IS NOT INITIAL.
        me->if_cts_hot_logger~long_text( iv_level    = if_cts_hot_logger=>co_level_2
                                         iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
                                         iv_text     = lv_long_text ).
      ENDIF.
    endif.

    if ix_exception->previous is bound.
      me->if_cts_hot_logger~abnormal_termination_exception( ix_exception->previous ).
    endif.
  endmethod.