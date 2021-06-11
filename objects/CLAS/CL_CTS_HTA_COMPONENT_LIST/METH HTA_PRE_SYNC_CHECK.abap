  METHOD hta_pre_sync_check.
    LOOP AT me->m_hta_full_packages INTO DATA(lr_full_package).
      CAST cl_cts_hta_component( lr_full_package )->hta_pre_sync_check( ).
    ENDLOOP.
    LOOP AT me->m_hta_packages INTO DATA(lr_package).
      CAST cl_cts_hta_component( lr_package )->hta_pre_sync_check( ).
    ENDLOOP.
    LOOP AT me->m_hta_objects INTO DATA(lr_object).
      CAST cl_cts_hta_component( lr_object )->hta_pre_sync_check(  ).
    ENDLOOP.
  ENDMETHOD.