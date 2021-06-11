  METHOD if_cts_hot_db_access~set_translation_relevance.
    UPDATE cts_hot_package SET abap_no_translation = i_translation_relevance->value WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_status = 'A'.
    IF i_translation_relevance = ce_cts_hta_translation=>not_relevant_for_translation.
      "Finally delete masterlang and text references from DB for all objects of the package for which language is switched off
      DELETE FROM cts_hot_otexts_h WHERE abap_object_reference IN
                                    ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = i_abap_hana_package_id
                                                                                         AND abap_status = 'A' ).
      DELETE FROM cts_hot_otexts_s WHERE abap_object_reference IN
                                    ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = i_abap_hana_package_id
                                                                                         AND abap_status = 'A' ).
      DELETE FROM cts_hot_otexts_l WHERE abap_object_reference IN
                                    ( SELECT abap_object_reference FROM cts_hot_object WHERE abap_hana_package_id = i_abap_hana_package_id
                                                                                         AND abap_status = 'A' ).

      UPDATE cts_hot_object SET abap_object_reference = '' WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_status = 'A'.
    ENDIF.
  ENDMETHOD.