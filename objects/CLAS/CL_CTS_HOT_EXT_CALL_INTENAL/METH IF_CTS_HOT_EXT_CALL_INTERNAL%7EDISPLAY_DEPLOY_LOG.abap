  METHOD if_cts_hot_ext_call_internal~display_deploy_log.
    me->if_cts_hot_ext_call_internal~display_log(
      EXPORTING
        i_log_messages = i_log_messages
        i_title        = CONV syst_title( 'SAP HANA Repository Deployment-Protokoll'(008) )
        i_heading      = i_heading
        i_level        = i_level
        i_condense     = 'X'
    ).
  ENDMETHOD.