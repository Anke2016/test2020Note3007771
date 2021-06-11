  method if_cts_hot_logger~messages.
    append lines of it_messages to mt_messages.
    loop at it_messages reference into data(lr_message).
      me->set_max_severity( exporting iv_severity = lr_message->severity
                            changing cv_severity = mv_max_severity ).
    endloop.
  endmethod.