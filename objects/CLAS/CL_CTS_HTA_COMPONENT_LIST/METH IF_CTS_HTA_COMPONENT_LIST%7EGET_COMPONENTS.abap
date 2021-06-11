  METHOD if_cts_hta_component_list~get_components.
    CASE i_component_type.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_object.
        r_result = me->m_hta_objects.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_package.
        r_result = me->m_hta_packages.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
        r_result = me->m_hta_full_packages.
    ENDCASE.
  ENDMETHOD.