  METHOD if_cts_hta_component~set_translation_relevance.
    read_hta_data( ).

    IF me->m_package_data_in_hta IS INITIAL.
      me->m_translation_relevance = i_translation_relevance. "to be used during synchronization if it will be called somewhen
    ELSEIF me->m_package_data_in_hta-abap_no_translation <> i_translation_relevance->value.
      DATA(lr_full_package) = cl_cts_hta_full_package=>create_instance_full_package( i_cts_hta_package = me
                                                                                     i_cts_hta_objects = VALUE #( ) ).

      lr_full_package->set_translation_relevance( i_translation_relevance = i_translation_relevance
                                                  i_trkorr = i_trkorr
                                                  i_suppress_dialog = i_suppress_dialog ).
    ENDIF.

  ENDMETHOD.