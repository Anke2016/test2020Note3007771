  METHOD append_log_message_package.
    DATA ls_log_message TYPE ty_log_message_package.
    ls_log_message-is_hana_message = i_is_hana_message.
    ls_log_message-severity = i_severity.
    ls_log_message-error_code = i_error_code.
    ls_log_message-message = i_message.
    ls_log_message-cts_hot_package = i_hot_package.
    APPEND ls_log_message TO c_logs.
  ENDMETHOD.