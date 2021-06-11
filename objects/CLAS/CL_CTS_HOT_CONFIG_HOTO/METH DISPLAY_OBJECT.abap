  METHOD display_object.
    CALL FUNCTION 'SCWB_NA_TLOGO_DISPLAY'
      EXPORTING
        ir_tlogo_object = ir_tlogo_object
      EXCEPTIONS
        error           = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_svrs_error_in_configclass.
    ENDIF.
  ENDMETHOD.