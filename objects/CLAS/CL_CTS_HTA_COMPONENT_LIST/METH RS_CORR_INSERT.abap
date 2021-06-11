  METHOD rs_corr_insert.
    LOOP AT me->m_hta_full_packages INTO DATA(lr_full_package).
      rs_corr_insert_hta_component( EXPORTING i_hta_component   = lr_full_package
                                              i_trkorr          = i_trkorr
                                              i_devclass        = i_devclass
                                              i_suppress_dialog = i_suppress_dialog
                                              i_force           = i_force
                                    CHANGING  c_sync_results = r_result ).
    ENDLOOP.
    LOOP AT me->m_hta_packages INTO DATA(lr_package).
      rs_corr_insert_hta_component( EXPORTING i_hta_component   = lr_package
                                              i_trkorr          = i_trkorr
                                              i_devclass        = i_devclass
                                              i_suppress_dialog = i_suppress_dialog
                                              i_force           = i_force
                                    CHANGING  c_sync_results = r_result ).
    ENDLOOP.
    LOOP AT me->m_hta_objects INTO DATA(lr_object).
      rs_corr_insert_hta_component( EXPORTING i_hta_component   = lr_object
                                              i_trkorr          = i_trkorr
                                              i_devclass        = i_devclass
                                              i_suppress_dialog = i_suppress_dialog
                                              i_force           = i_force
                                    CHANGING  c_sync_results = r_result ).
    ENDLOOP.
  ENDMETHOD.