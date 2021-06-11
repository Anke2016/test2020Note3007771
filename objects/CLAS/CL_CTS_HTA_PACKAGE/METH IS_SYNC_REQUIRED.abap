  METHOD is_sync_required.
    IF i_force = abap_true.
      r_result = abap_true.
      RETURN.
    ENDIF.

    IF me->m_package_data_in_hta-hana_pack_description = me->m_package_data_in_hana-hana_pack_description
        "and me->m_package_data_in_hta-hana_pack_delivery_unit = me->m_package_data_in_hana-hana_pack_delivery_unit "different DU name is OK because we do not write it to HANA but might have DU data in HTA
        "and me->m_package_data_in_hta-hana_pack_du_vendor = me->m_package_data_in_hana-hana_pack_du_vendor "different DU vendor is OK because we do not write it to HANA but might have DU data in HTA
        AND me->m_package_data_in_hta-hana_pack_hints_for_transl = me->m_package_data_in_hana-hana_pack_hints_for_transl
        AND me->m_package_data_in_hta-hana_pack_is_structural = me->m_package_data_in_hana-hana_pack_is_structural
        AND me->m_package_data_in_hta-hana_pack_orig_lang = me->m_package_data_in_hana-hana_pack_orig_lang
        AND me->m_package_data_in_hta-hana_pack_responsible = me->m_package_data_in_hana-hana_pack_responsible
        AND me->m_package_data_in_hta-hana_pack_src_system = me->m_package_data_in_hana-hana_pack_src_system
        AND me->m_package_data_in_hta-hana_pack_src_tenant = me->m_package_data_in_hana-hana_pack_src_tenant
        AND me->m_package_data_in_hta-hana_pack_text_collection = me->m_package_data_in_hana-hana_pack_text_collection
        AND me->m_package_data_in_hta-hana_pack_text_status = me->m_package_data_in_hana-hana_pack_text_status
        AND me->m_package_data_in_hta-hana_pack_text_term_domain = me->m_package_data_in_hana-hana_pack_text_term_domain.
      r_result = abap_false.
    ELSE.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.