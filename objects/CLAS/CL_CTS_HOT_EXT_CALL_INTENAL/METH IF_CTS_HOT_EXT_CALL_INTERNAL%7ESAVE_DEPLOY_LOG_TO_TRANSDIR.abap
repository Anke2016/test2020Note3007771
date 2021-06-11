  METHOD if_cts_hot_ext_call_internal~save_deploy_log_to_transdir.
    DATA: lv_log_dir_name TYPE eps2path,
          lv_logname_file TYPE trfile,
          lv_file_name    TYPE stpa-file.

    " first: calculate name of logfile
    lv_file_name = |{ i_log_name_prefix }_{ sy-datum }.{ sy-sysid }|.

    " second: read parameter DIR_TRANS_LOG
    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
      EXPORTING
        eps_subdir       = '$TR_LOG' " stands for DIR_TRANS_LOG
      IMPORTING
        ev_long_dir_name = lv_log_dir_name.

    " third: create full logfile (path + name of log file)
    CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD lv_file_name
                         ID 'PATH'     FIELD lv_log_dir_name
                         ID 'RESULT'   FIELD lv_logname_file.  " complete filename inclusive log directory

    " fourth: write the log to the file
    CALL FUNCTION 'TR_WRITE_LOG'
      EXPORTING
        iv_log_type       = 'FILE'          " Protokolltyp: 'FILE', 'DB', 'MEMORY'
        iv_logname_file   = lv_logname_file " Name des Protokolls (File)
      TABLES
        it_msgs           = i_log_messages  " Tabelle mit den Meldungen
      EXCEPTIONS
        invalid_input     = 1
        file_access_error = 2
        db_access_error   = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    r_result = lv_logname_file.
  ENDMETHOD.