  METHOD prepare_redeployment.
    rv_success = abap_true.
    IF sy-dbsys <> 'HDB'.
      RETURN.
    ENDIF.

    mr_logger->message( iv_msg_nr = '415'
                        iv_level  = 2 ). "Beginn Vorbereitung Redeployment der HANA-Repository-Pakete und -Objekte

    TRY.
        me->check_bck_table_existence( ).
        me->add_to_be_deleted_pkgs_objs( ).
        me->set_to_be_deployed_pkgs_objs( ).
      CATCH lcx_error_prepare_redeployment.
        "no logging here, already done in methods above
        rv_success = abap_false.
    ENDTRY.

    mr_logger->message( iv_msg_nr = '416'
                        iv_level  = 2
                        iv_var1   = COND #( WHEN rv_success = abap_true THEN 'Success'(002) ELSE 'Error'(001) ) ). "Ende Vorbereitung Redeployment der HANA-Repository-Pakete und -Objekte:&1

    mr_logger->flush( ).
  ENDMETHOD.