  METHOD get_sync_state_aggregated.
    DATA lt_reasons TYPE if_cts_hta_types=>ty_cx_cts_htas.

    r_result = i_sync_state. "use already reached sync_state as initial return value.

    LOOP AT i_components INTO DATA(lr_component).
      CASE lr_component->get_sync_state( IMPORTING e_reasons_can_not_be_synced = lt_reasons ).
        WHEN ce_cts_hta_sync_state=>in_sync.
          "nothing to do
        WHEN ce_cts_hta_sync_state=>not_in_sync.
          "if no error yet, set to not_in_sync
          IF r_result <> ce_cts_hta_sync_state=>can_not_be_synchronized.
            r_result = ce_cts_hta_sync_state=>not_in_sync.
          ENDIF.
        WHEN ce_cts_hta_sync_state=>can_not_be_synchronized.
          r_result = ce_cts_hta_sync_state=>can_not_be_synchronized.
          " for full packages return the exception with component as full package so that API consumer can remove the full package from the list to sync all syncable entries.
          IF lr_component->component_type = ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
            LOOP AT lt_reasons INTO DATA(lr_reason).
              lr_reason->set_full_package_as_component( CAST #( lr_component ) ).
              APPEND lr_reason TO c_reasons_can_not_be_synced.
            ENDLOOP.
          ELSE.
            APPEND LINES OF lt_reasons TO c_reasons_can_not_be_synced.
          ENDIF.
        WHEN OTHERS.
          "should not happen (only in case ce_cts_hta_deploy_state is extended with new values...)
          ASSERT 1 = 2.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.