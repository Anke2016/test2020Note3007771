  METHOD if_cts_hot_logger~flush.
    CALL FUNCTION 'TR_WRITE_LOG'
      EXPORTING
        iv_log_type       = 'FILE'     "Protokolltyp 'FILE', 'DB', 'MEMORY'
        iv_logname_file   = mv_logfile     "Name des Protokolls (File)
        iv_condense       = ' '     "Zeilen werden zusammengeschoben
      TABLES
        it_msgs           = mt_messages    "Tabelle mit den Meldungen
      EXCEPTIONS
        invalid_input     = 1
        file_access_error = 2
        db_access_error   = 3
        OTHERS            = 4.

    CLEAR mt_messages.
  ENDMETHOD.