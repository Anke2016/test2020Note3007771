  METHOD if_cts_hta_component~set_translation_relevance.
    LOOP AT me->m_hta_full_packages INTO DATA(lr_full_package).
      lr_full_package->set_translation_relevance( i_translation_relevance = i_translation_relevance
                                                  i_trkorr = i_trkorr
                                                  i_suppress_dialog = i_suppress_dialog ).
    ENDLOOP.
    LOOP AT me->m_hta_packages INTO DATA(lr_package).
      lr_package->set_translation_relevance( i_translation_relevance = i_translation_relevance
                                             i_trkorr = i_trkorr
                                             i_suppress_dialog = i_suppress_dialog ).
    ENDLOOP.
    LOOP AT me->m_hta_objects INTO DATA(lr_object).
      lr_object->set_translation_relevance( i_translation_relevance = i_translation_relevance
                                            i_trkorr = i_trkorr
                                            i_suppress_dialog = i_suppress_dialog ).
    ENDLOOP.
  ENDMETHOD.