  METHOD if_cts_hot_logger~set_msg_id.
    super->if_cts_hot_logger~set_msg_id( iv_msg_id ).

    LOOP AT mt_logger INTO DATA(lr_logger).
      lr_logger->set_msg_id( iv_msg_id ).
    ENDLOOP.
  ENDMETHOD.