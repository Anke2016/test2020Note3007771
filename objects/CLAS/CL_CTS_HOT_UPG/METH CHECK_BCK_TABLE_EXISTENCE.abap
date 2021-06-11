  METHOD check_bck_table_existence.
    TRY.
        me->execute_query( |SELECT ABAP_HANA_PACKAGE_ID FROM "CTS_HOT_PACKAGE~BCK" WHERE ABAP_HANA_PACKAGE_ID='TEST'| ).
      CATCH cx_sql_exception INTO DATA(lr_exc).
        IF lr_exc->sql_code = 259. "invalid table name
          mr_logger->error( '417' ). "  Tabelle CTS_HOT_PACKAGE~BCK fehlt
        ELSE.
          mr_logger->abnormal_termination_exception( lr_exc ).
        ENDIF.
        RAISE EXCEPTION TYPE lcx_error_prepare_redeployment.
    ENDTRY.

    TRY.
        me->execute_query( |SELECT ABAP_HANA_PACKAGE_ID FROM "CTS_HOT_OBJECT~BCK" WHERE ABAP_HANA_PACKAGE_ID='TEST'| ).
      CATCH cx_sql_exception INTO lr_exc.
        IF lr_exc->sql_code = 259. "invalid table name
          mr_logger->error( '418' ). "  Tabelle CTS_HOT_OBJECT~BCK fehlt
        ELSE.
          mr_logger->abnormal_termination_exception( lr_exc ).
        ENDIF.
        RAISE EXCEPTION TYPE lcx_error_prepare_redeployment.
    ENDTRY.
  ENDMETHOD.