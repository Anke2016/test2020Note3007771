  METHOD if_cts_hta_component~set_prework.
    g_db_access->set_prework( i_abap_hana_package_id = CONV cts_hot_package_id( me->if_cts_hta_component~transport_object_name )
                              i_prework = i_prework ).
  ENDMETHOD.