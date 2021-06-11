  METHOD if_cts_hot_ext_call_internal~display_log.
    DATA(lv_log_name) = |HTA_LOG_{ sy-uname }|.

    "first write log to memory
    CALL FUNCTION 'TR_WRITE_LOG'
      EXPORTING
        iv_log_type       = 'MEMORY'
        iv_logname_memory = CONV trfilename( lv_log_name )
        iv_condense       = i_condense
      TABLES
        it_msgs           = i_log_messages          " Tabelle mit den Meldungen
      EXCEPTIONS
        invalid_input     = 1
        file_access_error = 2
        db_access_error   = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " then display log from memory
    CALL FUNCTION 'TR_READ_AND_DISPLAY_LOG'
      EXPORTING
        iv_log_type            = 'MEMORY'
        iv_logname_memory      = CONV trfilename( lv_log_name )
        iv_titlebar            = i_title  " Titelzeile
        iv_heading             = i_heading
*        iv_top_line            = '1'
        iv_display_level       = i_level                               " Anzeigestufe (1 bis 9)
        iv_with_long_text_icon = 'X'                               " Mit Ikone 'Langtext'
      EXCEPTIONS
        invalid_input          = 1
        access_error           = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " finally delete log in memory
    CALL FUNCTION 'TR_DELETE_LOG'
      EXPORTING
        iv_log_type       = 'MEMORY'
        iv_logname_memory = CONV trfilename( lv_log_name ) " Name des Protokolls (Memory)
      EXCEPTIONS
        OTHERS            = 1.
  ENDMETHOD.