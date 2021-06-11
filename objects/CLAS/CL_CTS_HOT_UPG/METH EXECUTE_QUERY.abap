  METHOD execute_query.
    CLEAR et_result.

    DATA(lr_result) = NEW cl_sql_statement( )->execute_query( iv_query ).
    IF et_result IS SUPPLIED.
      lr_result->set_param_table( REF #( et_result ) ).
      lr_result->next_package( ).
    ENDIF.
    lr_result->close( ).
  ENDMETHOD.