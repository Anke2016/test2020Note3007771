  METHOD execute_sync.
    "sync package and objects of this full package
    super->execute_sync( i_force = i_force ).

    "set translation relevance if it was set before synchronization
    IF me->m_translation_relevance IS BOUND.
      "set translation relevance decided in rs_corr_insert or in set_translation_mode
      gr_hot_db_access->set_translation_relevance( i_abap_hana_package_id = me->m_hot_package->abap_hana_package_id
                                                   i_translation_relevance = me->m_translation_relevance  ).
    ENDIF.
  ENDMETHOD.