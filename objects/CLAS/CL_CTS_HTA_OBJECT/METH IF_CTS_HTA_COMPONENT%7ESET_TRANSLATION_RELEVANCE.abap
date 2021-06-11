  METHOD if_cts_hta_component~set_translation_relevance.
    me->m_hta_package->set_translation_relevance( i_translation_relevance = i_translation_relevance
                                                  i_trkorr = i_trkorr
                                                  i_suppress_dialog = i_suppress_dialog ).
  ENDMETHOD.