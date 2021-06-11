  METHOD read_hana_data.
    me->create_hot_hana_connector( ).
    me->m_object_data_in_hana = gr_hot_hana_connector->read_object_data_from_hana( i_cts_hot_object = me->m_hot_object ).
  ENDMETHOD.