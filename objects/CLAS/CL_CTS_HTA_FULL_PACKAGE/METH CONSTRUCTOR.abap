  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
    me->if_cts_hta_component~transport_object_name = i_cts_hta_package->transport_object_name.
    me->m_hot_package = CAST cl_cts_hta_component( i_cts_hta_package )->m_hot_package.
    me->add_component( i_cts_hta_package ).
    LOOP AT i_cts_hta_objects INTO DATA(lr_hta_object).
      IF i_cts_hta_package->if_cts_hta_component~transport_object_name = lr_hta_object->if_cts_hta_component~transport_object_name(40).
        me->add_component( lr_hta_object ).
      ENDIF.
    ENDLOOP.
    m_instantiation_finished = abap_true.
  ENDMETHOD.