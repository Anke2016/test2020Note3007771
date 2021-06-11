  METHOD if_cts_hta_component~get_deploy_state.
    me->read_hta_data( ).

    IF me->m_object_data_in_hta IS INITIAL.
      RAISE EXCEPTION TYPE cx_cts_hta_not_found
        EXPORTING
          textid             = cx_cts_hta_not_found=>object_not_found_in_hta
          message_variable_1 = CONV #( me->if_cts_hta_object~transport_object_name(40) )
          message_variable_2 = CONV #( me->if_cts_hta_object~transport_object_name+40(70) ).
    ELSEIF me->m_object_data_in_hta-hot_status = if_cts_hot_db_access=>co_hot_status_active
        OR me->m_object_data_in_hta-hot_status = if_cts_hot_db_access=>co_hot_status_new.
      r_result = ce_cts_hta_deploy_state=>deployed.
    ELSE.
      r_result = ce_cts_hta_deploy_state=>not_deployed.
    ENDIF.
  ENDMETHOD.