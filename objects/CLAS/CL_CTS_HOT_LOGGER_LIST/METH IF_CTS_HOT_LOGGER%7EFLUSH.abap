  METHOD if_cts_hot_logger~flush.
    LOOP AT mt_logger INTO DATA(lr_logger).
      lr_logger->messages( mt_messages ).
      lr_logger->flush( ).
    ENDLOOP.
    CLEAR mt_messages.
  ENDMETHOD.