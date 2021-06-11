  METHOD constructor.
    mr_logger = COND #( WHEN ir_logger IS BOUND THEN ir_logger ELSE NEW lcl_logger( ) ).
    mr_helper = COND #( WHEN ir_helper IS BOUND THEN ir_helper ELSE NEW lcl_helper( ) ).
  ENDMETHOD.