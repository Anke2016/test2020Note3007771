  METHOD rs_corr_insert.
    DATA: lv_mode     TYPE string,
          lv_trkorr   TYPE trkorr,
          lv_devclass TYPE devclass.

    IF is_sync_required( i_force ) = abap_false.
      RETURN.
    ENDIF.

    "By default try to use trkorr provided by user, only if user did not provide a transport request/task, use request received from rs_corr_check.
    IF i_trkorr IS NOT INITIAL.
      lv_trkorr = i_trkorr.
    ELSE.
      lv_trkorr = me->m_sync_trkorr.
    ENDIF.

    IF me->m_sync_tadir-devclass IS INITIAL. "tadir not existing
      lv_devclass = i_devclass.
      lv_mode = 'INSERT'. "new entry, so must be insert
    ELSE.
      lv_devclass = m_sync_tadir-devclass.
    ENDIF.

    IF me->m_package_data_in_hana IS INITIAL.
      lv_mode = 'DELETE'. "lv_mode=DELETE must be after lv_mode=INSERT because it might be there is no tadir yet and object does not exist in HANA --> deletion
    ELSE.
      IF me->m_sync_tadir-masterlang IS INITIAL. "entry not yet in tadir -> determine master language (m_sync_tadir will be set in rs_corr_check)
        me->m_sync_tadir-masterlang = me->m_external_calls->determine_masterlang_for_tadir(
              i_hana_package_name = me->m_hana_package_name
              i_hana_original_language =  me->m_package_data_in_hana-hana_pack_orig_lang
              i_suppress_dialog = i_suppress_dialog
              i_translation_relevance = me->m_translation_relevance ).

        IF  me->m_package_data_in_hana-hana_pack_orig_lang IS INITIAL.
          "user decided to use logon language, set translation relevance to not relevant for translation
          me->m_translation_relevance = ce_cts_hta_translation=>not_relevant_for_translation.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(lv_used_trkorr) = me->m_external_calls->rs_corr_insert(
      EXPORTING
        i_pgmid             = if_cts_hta_package=>co_pgmid
        i_object_type       = if_cts_hta_package=>co_object_type
        i_object_name       = me->if_cts_hta_component~transport_object_name
        i_suppress_dialog   = i_suppress_dialog
        i_mode              = lv_mode
        i_masterlang        = me->m_sync_tadir-masterlang
        i_trkorr            = lv_trkorr
        i_devclass          = lv_devclass
        i_cts_hta_component = me
    ).

    INSERT VALUE #( trkorr = lv_used_trkorr hta_components = VALUE #( ( me ) ) ) INTO TABLE r_result.
  ENDMETHOD.