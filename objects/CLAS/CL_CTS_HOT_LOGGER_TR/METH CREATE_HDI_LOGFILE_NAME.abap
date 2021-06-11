  METHOD CREATE_HDI_LOGFILE_NAME.
    DATA:
      lv_offset_slash     TYPE i,
      lv_offset_backslash TYPE i,
      lv_offset           TYPE i,
      lv_offset_dot       TYPE i.

    " First reduce the iv_logfile to filename and suffix
    FIND ALL OCCURRENCES OF '/' IN iv_logfile MATCH OFFSET lv_offset_slash IN CHARACTER MODE.
    FIND ALL OCCURRENCES OF '\' IN iv_logfile MATCH OFFSET lv_offset_backslash IN CHARACTER MODE.

    IF lv_offset_slash IS INITIAL AND lv_offset_backslash IS INITIAL.
      lv_offset = 0.
    ELSEIF lv_offset_slash > lv_offset_backslash.
      lv_offset = lv_offset_slash + 1.
    ELSE.
      lv_offset = lv_offset_backslash + 1.
    ENDIF.
    rv_logfile = iv_logfile+lv_offset.

    " Second, add _HDI before the dot
    FIND ALL OCCURRENCES OF '.' IN rv_logfile MATCH OFFSET lv_offset_dot IN CHARACTER MODE.
    IF lv_offset_dot IS INITIAL.
      rv_logfile = |{ rv_logfile }_HDI|.
    ELSE.
      lv_offset = lv_offset_dot + 1.
      rv_logfile = |{ rv_logfile(lv_offset_dot) }_HDI.{ rv_logfile+lv_offset }|.
    ENDIF.
  ENDMETHOD.