  METHOD execute_sync.
    IF is_sync_required( i_force ) = abap_false.
      RETURN.
    ENDIF.

    create_hot_hana_connector( ).
    gr_hot_hana_connector->sync_packages_from_hana_to_hot( VALUE #( ( me->m_hot_package ) ) ).

    IF me->m_translation_relevance IS BOUND.
      "set translation relevance decided in rs_corr_insert or in set_translation_mode
      gr_hot_db_access->set_translation_relevance( i_abap_hana_package_id = me->m_hot_package->abap_hana_package_id
                                                   i_translation_relevance = me->m_translation_relevance  ).
    ENDIF.
    IF me->m_deploy_mode IS BOUND.
      gr_hot_db_access->set_deploy_mode( i_abap_hana_package_id = me->m_hot_package->abap_hana_package_id
                                         i_deploy_mode = me->m_deploy_mode ).
    ENDIF.
  ENDMETHOD.