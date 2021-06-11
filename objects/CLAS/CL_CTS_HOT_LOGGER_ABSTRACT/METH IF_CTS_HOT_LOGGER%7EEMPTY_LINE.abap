  method if_cts_hot_logger~empty_line.
    message(
      exporting
        iv_msg_id   = 'SCTS_HDI'
        iv_msg_nr   = '100'
        iv_level    = iv_level
        iv_severity = iv_severity
    ).
  endmethod.