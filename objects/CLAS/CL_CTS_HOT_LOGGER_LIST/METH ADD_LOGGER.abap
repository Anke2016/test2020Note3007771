  METHOD add_logger.
    IF ir_logger IS BOUND AND NOT line_exists( mt_logger[ table_line = ir_logger ] ).
      APPEND ir_logger TO mt_logger.
    ENDIF.
  ENDMETHOD.