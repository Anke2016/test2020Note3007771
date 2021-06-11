  METHOD hta_pre_sync_check.
    IF me->m_object_data_in_hta IS INITIAL.
      RETURN.
    ELSE.
      IF me->is_hot_status_ok_for_sync( me->m_object_data_in_hta-hot_status ) = abap_false.
        RAISE EXCEPTION TYPE cx_cts_hta_wrong_status
          EXPORTING
            textid                 = cx_cts_hta_wrong_status=>object_requires_deployment
            name_of_obj_or_package = |{ me->m_object_data_in_hta-hana_object_name }.{ me->m_object_data_in_hta-hana_object_suffix }|
            hot_status             = me->m_object_data_in_hta-hot_status
            cts_hta_component      = me.
      ENDIF.

      IF me->m_object_key-hana_package_name <> me->m_object_data_in_hta-hana_package_id.
        RAISE EXCEPTION TYPE cx_cts_hta_name_conflict
          EXPORTING
            textid                      = cx_cts_hta_name_conflict=>package_name_conflict
            name_of_obj_or_package_conf = me->m_object_key-hana_package_name
            name_of_obj_or_package_hta  = me->m_object_data_in_hta-hana_package_id
            cts_hta_component           = me.
      ENDIF.

      IF me->m_object_key-hana_object_name <> me->m_object_data_in_hta-hana_object_name
        OR me->m_object_key-hana_object_suffix <> me->m_object_data_in_hta-hana_object_suffix.
        RAISE EXCEPTION TYPE cx_cts_hta_name_conflict
          EXPORTING
            textid                      = cx_cts_hta_name_conflict=>object_name_conflict
            name_of_obj_or_package_conf = |{ me->m_object_key-hana_object_name }.{ me->m_object_key-hana_object_suffix }|
            name_of_obj_or_package_hta  = |{ me->m_object_data_in_hta-hana_object_name }.{ me->m_object_data_in_hta-hana_object_suffix }|
            cts_hta_component           = me.
      ENDIF.
    ENDIF.
  ENDMETHOD.