  METHOD if_cts_hta_component~set_deploy_mode.
    read_hta_data( ).

    IF me->m_package_data_in_hta IS INITIAL.
      me->m_deploy_mode = i_deploy_mode. "to be used during synchronization if it will be called somewhen
    ELSEIF me->m_package_data_in_hta-hot_activation_mode <> i_deploy_mode->value.
      read_hana_data( ).

      rs_corr_check( i_force = abap_true
                     i_suppress_dialog = i_suppress_dialog ).

      rs_corr_insert( i_force = abap_true
                      i_trkorr = i_trkorr
                      i_suppress_dialog = i_suppress_dialog ).

      gr_hot_db_access->set_deploy_mode( i_abap_hana_package_id = me->m_hot_package->abap_hana_package_id
                                         i_deploy_mode = i_deploy_mode ).
    ENDIF.
  ENDMETHOD.