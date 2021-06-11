  private section.
    constants:
      co_new_log_section type sprot_u-newobj value 'X'.

    data:
      mv_msg_id          type sprot_u-ag,
      mv_new_log_section type sprot_u-newobj,
      mv_max_severity    type sprot_u-severity.

    methods:
      message
        importing
          iv_msg_id   type sprot_u-ag optional
          iv_msg_nr   type sprot_u-msgnr
          iv_level    type sprot_u-level
          iv_severity type sprot_u-severity
          iv_var1     type sprot_u-var1 optional
          iv_var2     type sprot_u-var2 optional
          iv_var3     type sprot_u-var3 optional
          iv_var4     type sprot_u-var4 optional.
