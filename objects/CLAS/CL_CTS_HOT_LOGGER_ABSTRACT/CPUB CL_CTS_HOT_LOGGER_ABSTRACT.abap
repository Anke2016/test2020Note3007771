class cl_cts_hot_logger_abstract definition public create public abstract.

  public section.
    interfaces:
      if_cts_hot_logger

      abstract methods:
      flush.

    class-methods:
      "! Checks whether iv_severity is higher than cv_severity and in case it is, changes cv_severity to iv_severity.<br/>
      "! Order is described in if_cts_hot_logger-&gt;get_max_severity.<br/>
      "! In addition if cv_severity = 'I' and ( iv_severity = 'I' or if_cts_hot_logger=&gt;co_severity_info ) then
      "! cv_severity will be if_cts_hot_logger=&gt;co_severity_info.
      set_max_severity
        importing
          iv_severity type sprot_u-severity
        changing
          cv_severity type sprot_u-severity.

    methods:
      constructor
        importing
          iv_msg_id type sprot_u-ag default 'SCTS_HOT'. "NEVER change this default!
