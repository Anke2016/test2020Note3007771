  METHOD execute_sync.
    LOOP AT me->m_hta_full_packages INTO DATA(lr_full_package).
      CAST cl_cts_hta_component( lr_full_package )->execute_sync( i_force = i_force ).
    ENDLOOP.
    LOOP AT me->m_hta_packages INTO DATA(lr_package).
      CAST cl_cts_hta_component( lr_package )->execute_sync( i_force = i_force ).
    ENDLOOP.
    LOOP AT me->m_hta_objects INTO DATA(lr_object).
      CAST cl_cts_hta_component( lr_object )->execute_sync( i_force = i_force ).
    ENDLOOP.
  ENDMETHOD.