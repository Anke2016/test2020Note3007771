  METHOD constructor.
    super->constructor( ).
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_if_cts_hta_package.
    me->m_abap_status = i_abap_status.
    me->m_hot_package = i_hot_package.
    me->if_cts_hta_component~transport_object_name = i_hot_package->abap_hana_package_id.
    me->m_hana_package_name = i_hot_package->hana_package_id.
  ENDMETHOD.