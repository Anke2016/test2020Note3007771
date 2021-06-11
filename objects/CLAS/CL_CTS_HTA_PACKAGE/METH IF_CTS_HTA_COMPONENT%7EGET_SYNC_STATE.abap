  METHOD if_cts_hta_component~get_sync_state.
    DATA lr_exc TYPE REF TO cx_cts_hta.

    CLEAR e_reasons_can_not_be_synced.

    TRY.
        me->read_hana_data( ).
        me->read_hta_data( ).

        " 1. check for hot_status and name_conflicts
        me->hta_pre_sync_check( ).

        " 2. compare versions in HANA and HTA
        IF me->is_sync_required( ) = abap_true.
          r_result = ce_cts_hta_sync_state=>not_in_sync.
        ELSE.
          r_result = ce_cts_hta_sync_state=>in_sync.
        ENDIF.
      CATCH cx_cts_hta_no_hana_database INTO DATA(lr_exc_no_hana).
        RAISE EXCEPTION lr_exc_no_hana.
      CATCH cx_cts_hta_wrong_status cx_cts_hta_name_conflict cx_cts_hta INTO lr_exc.
        r_result = ce_cts_hta_sync_state=>can_not_be_synchronized.
        APPEND lr_exc TO e_reasons_can_not_be_synced.
    ENDTRY.
  ENDMETHOD.