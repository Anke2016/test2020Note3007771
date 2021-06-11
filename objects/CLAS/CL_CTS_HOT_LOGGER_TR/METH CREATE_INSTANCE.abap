  METHOD create_instance.
    rr_instance = NEW cl_cts_hot_logger_tr( ).

    CALL FUNCTION 'TR_INITIALIZE_LOG'
      EXPORTING
        acttype         = co_acttype
        sysname         = gv_sysname
        trkorr          = iv_trkorr
        trbat_logname   = iv_logname
        iv_check_levels = 'X'
      IMPORTING
        file            = rr_instance->mv_logfile
      EXCEPTIONS
        OTHERS          = 1.

    DATA(lv_assert_message) = 'Transport log could not be created.'.
    ASSERT
      FIELDS
        lv_assert_message
        iv_trkorr
        iv_logname
      CONDITION
        sy-subrc = 0. "harter Abbruch, da sonst kein Log zum trbat Eintrag
  ENDMETHOD.