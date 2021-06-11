  METHOD create_instance_for_file.
    DATA: lv_log_dir_name TYPE eps2path,
          lv_logname_file TYPE trfile.

    rr_instance = NEW cl_cts_hot_logger_tr( ).

    " second: read parameter DIR_TRANS_LOG
    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
      EXPORTING
        eps_subdir       = '$TR_LOG' " stands for DIR_TRANS_LOG
      IMPORTING
        ev_long_dir_name = lv_log_dir_name.

    " third: create full logfile (path + name of log file)
    lv_logname_file = iv_file_name.
    CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD lv_logname_file
                         ID 'PATH'     FIELD lv_log_dir_name
                         ID 'RESULT'   FIELD rr_instance->mv_logfile.  " complete filename inclusive log directory
  ENDMETHOD.