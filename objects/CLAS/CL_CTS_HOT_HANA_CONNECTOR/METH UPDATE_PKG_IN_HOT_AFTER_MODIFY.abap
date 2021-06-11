  METHOD update_pkg_in_hot_after_modify.
    IF i_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_to_be_deleted
       OR i_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_delete_error.
      m_cts_hot_db_access->delete_cts_hot_package( i_abap_hana_package_id = i_cts_hot_package-abap_hana_package_id i_abap_status = i_cts_hot_package-abap_status ).
      "if CWB/SNote case, also delete active version
      IF i_cts_hot_package-abap_status = 'I'.
        m_cts_hot_db_access->delete_cts_hot_package( i_abap_hana_package_id = i_cts_hot_package-abap_hana_package_id i_abap_status = 'A' ).
      ENDIF.

      "finally delete SMODI entries in all delete cases.
      me->delete_smodi_entries( i_obj_type = 'HOTP' i_obj_name = CONV cts_hot_object_name( i_cts_hot_package-abap_hana_package_id ) ).
    ELSE.
      DATA lv_deployed_at TYPE timestampl.
      DATA ls_cts_hot_package TYPE cts_hot_package.

      lv_deployed_at = me->m_timestamp_provider->get_timestamp( ).

      IF i_cts_hot_package-abap_status = 'I'.
        ls_cts_hot_package = i_cts_hot_package.
        m_cts_hot_db_access->delete_cts_hot_package( i_abap_hana_package_id = i_cts_hot_package-abap_hana_package_id i_abap_status = 'I' ).

        "Move package with abap_status='I' version only to abap_status='A' version if no parallel TP import happened. To check this compare
        "abap_status='A' version before and after create/updated/modify of abap_status='I' version in HANA.
        DATA(ls_cts_hot_package_a) = m_cts_hot_db_access->read_cts_hot_package( i_cts_hot_package-abap_hana_package_id ).
        IF ls_cts_hot_package_a-hot_status = i_cts_hot_package_a-hot_status.
          ls_cts_hot_package-abap_status = 'A'.
          ls_cts_hot_package-hot_status = if_cts_hot_db_access=>co_hot_status_active.
          ls_cts_hot_package-abap_deployed_at = lv_deployed_at.
          ls_cts_hot_package-abap_deployed_by = sy-uname.

          m_cts_hot_db_access->modify_cts_hot_package( ls_cts_hot_package ).
        ENDIF.

        "in SNote/CWB case also modify SMODI entries
        me->update_smodi_entries( i_obj_type = 'HOTP' i_obj_name = CONV #( i_cts_hot_package-abap_hana_package_id ) ).
      ELSE.
        m_cts_hot_db_access->update_package_after_deploymnt( i_old_package = i_cts_hot_package
                                                             i_new_status = if_cts_hot_db_access=>co_hot_status_active
                                                             i_new_deployed_at = lv_deployed_at
                                                             i_new_deployed_by = sy-uname ).
      ENDIF.
    ENDIF.
  ENDMETHOD.