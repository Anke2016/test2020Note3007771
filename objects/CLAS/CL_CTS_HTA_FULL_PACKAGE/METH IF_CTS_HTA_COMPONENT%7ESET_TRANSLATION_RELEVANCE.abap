  METHOD if_cts_hta_component~set_translation_relevance.
    DATA(ls_cts_hot_package) = gr_hot_db_access->read_cts_hot_package( i_abap_hana_package_id = me->m_hot_package->abap_hana_package_id ).
    IF ls_cts_hot_package IS INITIAL.
      me->m_translation_relevance = i_translation_relevance. "to be used during synchronization if it will be called somewhen
    ELSEIF ls_cts_hot_package-abap_no_translation <> i_translation_relevance->value.
      hta_pre_sync_check( ).

      rs_corr_check( i_force = abap_true
                     i_suppress_dialog = i_suppress_dialog ).

      rs_corr_insert( i_force = abap_true
                      i_trkorr = i_trkorr
                      i_suppress_dialog = i_suppress_dialog ).

      gr_hot_db_access->set_translation_relevance( i_abap_hana_package_id = me->m_hot_package->abap_hana_package_id
                                                   i_translation_relevance = i_translation_relevance ).
    ENDIF.
  ENDMETHOD.