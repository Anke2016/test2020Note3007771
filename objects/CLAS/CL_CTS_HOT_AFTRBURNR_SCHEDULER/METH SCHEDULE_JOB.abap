  METHOD schedule_job.

    CLEAR:
      ev_afterburner_jobcount,
      ev_job_schedule_error.

    GET TIME STAMP  FIELD DATA(lv_tmstmp).
    me->mr_logger = ir_logger.
    me->mr_logger->set_msg_id( 'SCTS_HOT' ).
    me->mr_logger->new_log_section( ).
    me->mr_logger->message( iv_msg_nr = '624'
                            iv_level  = COND #( WHEN iv_transport_logs = abap_true THEN if_cts_hot_logger=>co_level_2 ELSE if_cts_hot_logger=>co_level_1 )
                            iv_var1   = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_tmstmp ) } (UTC)| ).

    IF me->check_preconditions( ) = abap_true.
      me->submit_job(
        IMPORTING
          ev_afterburner_jobcount = ev_afterburner_jobcount
          ev_error_occurred       = ev_job_schedule_error
      ).
    ENDIF.
    GET TIME STAMP  FIELD lv_tmstmp.
    me->mr_logger->message( iv_msg_nr = '631'
                            iv_level = COND #( WHEN iv_transport_logs = abap_true THEN if_cts_hot_logger=>co_level_2 ELSE if_cts_hot_logger=>co_level_1 )
                            iv_var1 = |{ cl_cts_hot_utility=>get_formatted_timestamp( lv_tmstmp ) } (UTC)| ).


  ENDMETHOD.