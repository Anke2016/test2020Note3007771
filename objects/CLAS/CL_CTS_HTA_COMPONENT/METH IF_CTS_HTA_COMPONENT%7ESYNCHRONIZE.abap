  METHOD if_cts_hta_component~synchronize.
    "0. create hot hana connector. If not on HANA, exception is thrown.
    me->create_hot_hana_connector( ).

    "2.0 read data from HTA
    me->read_hta_data( ).

    "2.1 read data from HAMA
    me->read_hana_data( ).

    "2.2 check whether objects can be synchronized
    me->hta_pre_sync_check( ).

    "3. Check add of package/object to transport request
    me->rs_corr_check( i_suppress_dialog = i_suppress_dialog
                   i_force           = i_force ).

    "4. Add package/object to transport request
    DATA(lr_result) = me->rs_corr_insert( i_trkorr          = i_trkorr
                                      i_devclass        = i_devclass
                                      i_suppress_dialog = i_suppress_dialog
                                      i_force           = i_force
    ).

    "5. Synchronize the component (package/object/full_package/component_list)
    me->execute_sync( i_force = i_force ).

    r_result = lr_result.
  ENDMETHOD.