  METHOD if_cts_hta_component~set_deploy_mode.
    me->m_hta_package->set_deploy_mode( i_deploy_mode = i_deploy_mode
                                        i_trkorr = i_trkorr
                                        i_suppress_dialog = i_suppress_dialog ).
  ENDMETHOD.