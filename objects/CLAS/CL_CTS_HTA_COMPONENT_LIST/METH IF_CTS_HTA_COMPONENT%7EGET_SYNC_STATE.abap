  METHOD if_cts_hta_component~get_sync_state.
    DATA lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    CLEAR e_reasons_can_not_be_synced.
    r_result = ce_cts_hta_sync_state=>in_sync. "default before any check

    IF me->m_hta_packages IS INITIAL AND me->m_hta_objects IS INITIAL AND me->m_hta_full_packages IS INITIAL.
      RETURN.
    ENDIF.

    r_result = get_sync_state_aggregated( EXPORTING i_components = me->m_hta_packages
                                                    i_sync_state = r_result
                                          CHANGING  c_reasons_can_not_be_synced = e_reasons_can_not_be_synced ).

    r_result = get_sync_state_aggregated( EXPORTING i_components = me->m_hta_objects
                                                    i_sync_state = r_result
                                          CHANGING  c_reasons_can_not_be_synced = e_reasons_can_not_be_synced ).

    r_result = get_sync_state_aggregated( EXPORTING i_components = me->m_hta_full_packages
                                                    i_sync_state = r_result
                                          CHANGING  c_reasons_can_not_be_synced = e_reasons_can_not_be_synced ).
  ENDMETHOD.