  METHOD read_hana_data.
    create_hot_hana_connector( ).
    me->m_package_data_in_hana = gr_hot_hana_connector->read_package_data_from_hana( me->m_hana_package_name ).
  ENDMETHOD.