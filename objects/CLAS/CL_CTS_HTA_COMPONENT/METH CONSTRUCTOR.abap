  METHOD constructor.
    me->if_cts_hta_component~component_type = ce_cts_hta_component_type=>ct_unknown. "set to unknown and force subclasses to override
    me->m_hana_deployer = NEW lcl_hana_deployer( ).
    me->m_db_access = NEW lcl_hta_db_access( ).
    me->m_external_calls = NEW cl_cts_hot_ext_call_intenal( ).
    gr_hot_db_access = NEW cl_cts_hot_db_access( ).
  ENDMETHOD.