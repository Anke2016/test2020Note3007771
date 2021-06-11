  METHOD constructor.
    super->constructor( ).

    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_object.

    me->m_abap_status = i_abap_status.
    me->m_hot_package = CAST cl_cts_hta_component( i_hta_package )->m_hot_package.
    me->m_hta_package = i_hta_package.
    me->m_hot_object = i_hot_object.

    me->if_cts_hta_component~transport_object_name = i_hot_object->transport_object_name.
    me->if_cts_hta_object~object_key-hana_package_name = i_hot_object->hana_package_id.
    me->if_cts_hta_object~object_key-hana_object_name  = i_hot_object->hana_object_name.
    me->if_cts_hta_object~object_key-hana_object_suffix = i_hot_object->hana_object_suffix.
  ENDMETHOD.