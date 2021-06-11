  METHOD read_hta_data.
    me->m_package_data_in_hta = gr_hot_db_access->read_cts_hot_package( me->m_hot_package->abap_hana_package_id ).
  ENDMETHOD.