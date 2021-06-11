  METHOD sync_packages_from_hana_to_hot.
    DATA: ls_cts_hot_package TYPE cts_hot_package,
          lo_package         TYPE REF TO cl_cts_hot_package.

    DATA(lv_default_deploy_mode) = me->m_cts_hot_db_access->read_default_deploy_mode( ).
    DATA(lv_default_translation_relev) = me->m_cts_hot_db_access->read_default_translation_relev( ).

    LOOP AT i_hana_packages_list INTO lo_package.
      DATA(ls_cts_hot_package_hana) = read_package_data_from_hana( lo_package->hana_package_id ).

      IF ls_cts_hot_package_hana IS INITIAL.
        "package not existing in HANA. delete in HOT
        me->m_cts_hot_db_access->delete_cts_hot_package( lo_package->abap_hana_package_id ).

        "also delete SMODI entries when package is deleted during sync
        me->delete_smodi_entries( i_obj_type = 'HOTP' i_obj_name = CONV cts_hot_object_name( lo_package->abap_hana_package_id ) ).
      ELSE.
        "package read successful from HANA
        CLEAR ls_cts_hot_package.

        "read package from DB to preserve hot_activation_mode (deploy mode)
        ls_cts_hot_package = m_cts_hot_db_access->read_cts_hot_package( lo_package->abap_hana_package_id ).

        IF ls_cts_hot_package IS INITIAL.
          "if package is synchronized the first time

          "set deploy mode
          ls_cts_hot_package-hot_activation_mode = lv_default_deploy_mode.

          "and decide on translation relevance.
          IF ls_cts_hot_package_hana-hana_pack_orig_lang IS INITIAL. "first sync and no hana orig language
            ls_cts_hot_package-abap_no_translation = if_cts_hot_db_access=>co_hot_not_relevant_for_transl.
          ELSE.
            ls_cts_hot_package-abap_no_translation = lv_default_translation_relev.
          ENDIF.
        ELSE.
          IF ls_cts_hot_package-hot_activation_mode IS INITIAL.
            "required for upgrades because hot_activation_mode was introduced after first delivery and thus needs to be set to SAP default
            "if entry was created before when there was no activation mode.
            ls_cts_hot_package-hot_activation_mode = if_cts_hot_db_access=>co_hot_deploy_mode_always.
          ELSE.
            "no change of deploy mode during sync
          ENDIF.
        ENDIF.
        ls_cts_hot_package-abap_hana_package_id = lo_package->abap_hana_package_id.
        ls_cts_hot_package-abap_status = 'A'. "abap_status always A in HOT. only SNOte writes I
        ls_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_new. "sync always writes hot status NEW
        ls_cts_hot_package-hana_package_id = ls_cts_hot_package_hana-hana_package_id.
        ls_cts_hot_package-hana_pack_src_system = ls_cts_hot_package_hana-hana_pack_src_system.
        ls_cts_hot_package-hana_pack_src_tenant = ls_cts_hot_package_hana-hana_pack_src_tenant.
        ls_cts_hot_package-hana_pack_description = ls_cts_hot_package_hana-hana_pack_description.
        ls_cts_hot_package-hana_pack_responsible = ls_cts_hot_package_hana-hana_pack_responsible.
        ls_cts_hot_package-hana_pack_orig_lang = ls_cts_hot_package_hana-hana_pack_orig_lang.
        ls_cts_hot_package-hana_pack_is_structural = ls_cts_hot_package_hana-hana_pack_is_structural.
        ls_cts_hot_package-hana_pack_delivery_unit = ls_cts_hot_package_hana-hana_pack_delivery_unit.
        ls_cts_hot_package-hana_pack_du_vendor = ls_cts_hot_package_hana-hana_pack_du_vendor.
        ls_cts_hot_package-hana_pack_text_collection = ls_cts_hot_package_hana-hana_pack_text_collection.
        ls_cts_hot_package-hana_pack_text_status = ls_cts_hot_package_hana-hana_pack_text_status.
        ls_cts_hot_package-hana_pack_text_term_domain = ls_cts_hot_package_hana-hana_pack_text_term_domain.
        ls_cts_hot_package-hana_pack_hints_for_transl = ls_cts_hot_package_hana-hana_pack_hints_for_transl.
        ls_cts_hot_package-hana_read_system = g_hana_sid.
        ls_cts_hot_package-abap_sync_system = sy-sysid.
        GET TIME STAMP FIELD ls_cts_hot_package-abap_synced_at.
        ls_cts_hot_package-abap_synced_by = sy-uname.

        me->m_cts_hot_db_access->modify_cts_hot_package( ls_cts_hot_package ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.