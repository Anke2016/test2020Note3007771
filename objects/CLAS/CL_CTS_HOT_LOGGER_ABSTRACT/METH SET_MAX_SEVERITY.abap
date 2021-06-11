  method set_max_severity.
    case cv_severity.
      when if_cts_hot_logger=>co_severity_abnormal_terminatn.
        "nothing to do, max already reached
      when if_cts_hot_logger=>co_severity_error.
        if iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn.
          cv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn.
        endif.
      when if_cts_hot_logger=>co_severity_post_processing.
        if iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
           or iv_severity = if_cts_hot_logger=>co_severity_error.
          cv_severity = iv_severity.
        endif.
      when if_cts_hot_logger=>co_severity_warning.
        if iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
           or iv_severity = if_cts_hot_logger=>co_severity_error
           or iv_severity = if_cts_hot_logger=>co_severity_post_processing.
          cv_severity = iv_severity.
        endif.
      when if_cts_hot_logger=>co_severity_info.
        if iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
           or iv_severity = if_cts_hot_logger=>co_severity_error
           or iv_severity = if_cts_hot_logger=>co_severity_post_processing
           or iv_severity = if_cts_hot_logger=>co_severity_warning.
          cv_severity = iv_severity.
        endif.
      when 'I'.
        if iv_severity = if_cts_hot_logger=>co_severity_abnormal_terminatn
           or iv_severity = if_cts_hot_logger=>co_severity_error
           or iv_severity = if_cts_hot_logger=>co_severity_post_processing
           or iv_severity = if_cts_hot_logger=>co_severity_warning.
          cv_severity = iv_severity.
        endif.
        if iv_severity = 'I'
           or iv_severity = if_cts_hot_logger=>co_severity_info.
          cv_severity = if_cts_hot_logger=>co_severity_info.
        endif.
      when others.
        assert 1 = 2. "unknown severity
    endcase.
  endmethod.