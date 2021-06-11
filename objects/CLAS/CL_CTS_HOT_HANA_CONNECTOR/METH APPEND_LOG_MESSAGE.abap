  METHOD append_log_message.
    DATA: ls_log_message TYPE ty_log_message,
          lv_length      TYPE i.
    ls_log_message-is_hana_message = i_is_hana_message.
    ls_log_message-severity = i_severity.
    ls_log_message-error_code = i_error_code.

    lv_length = strlen( i_message ).
    IF i_severity = '1' AND i_error_code = '0' AND lv_length > 310. "310 = 246 + 35 + 29 (29=length of '...123456 chars not logged...' so that we do not create even longer texts)
      "in case of success, only log first 246 chars and last 35 chars (usually "timestamp: 2019-08-16,12:14:43.595") of each message (HANA CheckResult)
      ls_log_message-message = |{ substring( val = i_message len = 246 ) }...{ lv_length - 281 } chars not logged...| &&
                               |{ substring( val = i_message off = ( lv_length - 35 ) ) }|.
    ELSEIF lv_length > mv_max_no_of_chars_for_log.
      "in case of very long messages, only log first mv_max_no_of_chars_for_log chars (default 10000) of each message (HANA CheckResult)
      ls_log_message-message = |{ substring( val = i_message len = mv_max_no_of_chars_for_log ) }...{ lv_length - mv_max_no_of_chars_for_log } chars not logged...|.
    ELSE.
      ls_log_message-message = i_message.
    ENDIF.

    ls_log_message-timestamp = i_timestamp.
    ls_log_message-location = i_location.
    ls_log_message-cts_hot_object = i_hot_object.
    APPEND ls_log_message TO c_logs.
  ENDMETHOD.