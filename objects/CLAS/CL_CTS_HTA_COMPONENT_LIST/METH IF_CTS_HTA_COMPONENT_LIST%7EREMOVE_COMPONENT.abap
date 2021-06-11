  METHOD if_cts_hta_component_list~remove_component.
    IF i_cts_hta_component IS NOT BOUND.
      RETURN.
    ENDIF.

    CASE i_cts_hta_component->component_type.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_package.
        DELETE me->m_hta_packages WHERE table_line = i_cts_hta_component.
        IF sy-subrc = 0.
          r_result = abap_true.
        ELSE.
          r_result = abap_false.
        ENDIF.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_object.
        DELETE me->m_hta_objects WHERE table_line = i_cts_hta_component.
        IF sy-subrc = 0.
          r_result = abap_true.
        ELSE.
          r_result = abap_false.
        ENDIF.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
        DELETE me->m_hta_full_packages WHERE table_line = i_cts_hta_component.
        IF sy-subrc = 0.
          r_result = abap_true.
        ELSE.
          r_result = abap_false.
        ENDIF.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_component_list.
        "assume removal is successful, first removing with abap_false will set it to false, because r_result should be false if at least one remove_component was false
        r_result = abap_true.

        "remove all packages of passed list
        DATA(lt_components) = CAST if_cts_hta_component_list( i_cts_hta_component )->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
        LOOP AT lt_components INTO DATA(lr_component).
          DATA(lv_result) = me->if_cts_hta_component_list~remove_component( lr_component ).
          IF lv_result = abap_false AND r_result = abap_true.
            r_result = abap_false.
          ENDIF.
        ENDLOOP.

        "remove all objects of passed list
        lt_components = CAST if_cts_hta_component_list( i_cts_hta_component )->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
        LOOP AT lt_components INTO lr_component.
          lv_result = me->if_cts_hta_component_list~remove_component( lr_component ).
          IF lv_result = abap_false AND r_result = abap_true.
            r_result = abap_false.
          ENDIF.
        ENDLOOP.

        "remove all full packages of passed list
        lt_components = CAST if_cts_hta_component_list( i_cts_hta_component )->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
        LOOP AT lt_components INTO lr_component.
          lv_result = me->if_cts_hta_component_list~remove_component( lr_component ).
          IF lv_result = abap_false AND r_result = abap_true.
            r_result = abap_false.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.