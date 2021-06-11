  METHOD constructor.
    me->m_nhi_api_user = i_nhi_api_user.
    me->m_nhi_object_api = i_nhi_api->get_object( ).
    me->m_nhi_package_api = i_nhi_api->get_package( ).
    me->m_nhi_delivery_unit_api = i_nhi_api->get_delivery_unit( ).
    me->m_nhi_text_api = i_nhi_api->get_text( ).
    CREATE OBJECT me->m_cts_hot_db_access TYPE cl_cts_hot_db_access.
    CREATE OBJECT me->m_timestamp_provider TYPE lcl_timestamp_provider.
    me->mv_max_no_of_chars_for_log = me->m_cts_hot_db_access->read_max_nr_of_chars_for_log(  ).
  ENDMETHOD.