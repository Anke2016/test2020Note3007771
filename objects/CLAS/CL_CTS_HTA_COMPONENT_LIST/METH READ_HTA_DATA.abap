  METHOD read_hta_data.
    LOOP AT me->m_hta_full_packages INTO DATA(lr_full_package).
      CAST cl_cts_hta_component( lr_full_package )->read_hta_data( ).
    ENDLOOP.
    LOOP AT me->m_hta_packages INTO DATA(lr_package).
      CAST cl_cts_hta_component( lr_package )->read_hta_data( ).
    ENDLOOP.
    LOOP AT me->m_hta_objects INTO DATA(lr_object).
      CAST cl_cts_hta_component( lr_object )->read_hta_data(  ).
    ENDLOOP.
  ENDMETHOD.