  METHOD read_hta_data.
    me->m_object_data_in_hta = gr_hot_db_access->read_cts_hot_object( i_abap_hana_package_id = me->m_hot_object->abap_hana_package_id
                                                                      i_abap_hana_object_name_suffix = me->m_hot_object->abap_hana_object_name_suffix ).
  ENDMETHOD.