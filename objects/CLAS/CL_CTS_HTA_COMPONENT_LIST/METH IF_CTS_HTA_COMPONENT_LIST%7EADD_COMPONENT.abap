  METHOD if_cts_hta_component_list~add_component.
    IF i_cts_hta_component IS NOT BOUND.
      RETURN.
    ENDIF.

    " So far we only support deployment if all objects/packages have been read from inactive or active
    " abap_status. Inactive used for SNote and CWB only
    IF m_hta_packages IS INITIAL AND m_hta_objects IS INITIAL AND m_hta_full_packages IS INITIAL.
      "first entry makes the list either an abap_status=inactive or abap_status=active list.
      me->m_abap_status = CAST cl_cts_hta_component( i_cts_hta_component )->m_abap_status.
    ELSE.
      IF me->m_abap_status <> CAST cl_cts_hta_component( i_cts_hta_component )->m_abap_status.
        r_result = abap_false. "Do not allow adding of different types of abap_status to one list.
        RETURN.
      ENDIF.
    ENDIF.

    CASE i_cts_hta_component->component_type.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_package.
        DATA(lr_hta_package) = CAST if_cts_hta_package( i_cts_hta_component ).
        "do not add if package already exists or if full package with this package already exists
        IF line_exists( me->m_hta_packages[ table_line->transport_object_name = lr_hta_package->transport_object_name ] )
            OR line_exists( me->m_hta_full_packages[ table_line->transport_object_name = lr_hta_package->transport_object_name ] ).
          r_result = abap_false.
        ELSE.
          APPEND lr_hta_package TO me->m_hta_packages.
          r_result = abap_true.
        ENDIF.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_object.
        DATA(lr_hta_object) = CAST if_cts_hta_object( i_cts_hta_component ).
        "do not add if package already exists or if full package with this package already exists
        IF line_exists( me->m_hta_objects[ table_line->transport_object_name = lr_hta_object->transport_object_name ] )
            OR line_exists( me->m_hta_full_packages[ table_line->transport_object_name = lr_hta_object->transport_object_name(40) ] ).
          r_result = abap_false.
        ELSE.
          APPEND lr_hta_object TO me->m_hta_objects.
          r_result = abap_true.
        ENDIF.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_full_package.
        DATA(lr_hta_full_package) = CAST if_cts_hta_full_package( i_cts_hta_component ).
        IF line_exists( me->m_hta_full_packages[ table_line = lr_hta_full_package ] ).
          r_result = abap_false.
        ELSE.
          APPEND lr_hta_full_package TO me->m_hta_full_packages.
          r_result = abap_true.
          "remove hotps and hotos because hota was added
          DELETE me->m_hta_packages WHERE table_line->transport_object_name = lr_hta_full_package->transport_object_name.
          DELETE me->m_hta_objects WHERE table_line->transport_object_name(40) = lr_hta_full_package->transport_object_name.
        ENDIF.
      WHEN ce_cts_hta_component_type=>ct_if_cts_hta_component_list.
        "assume adding is successful, first adding with abap_false will set it to false, because r_result should be false if at least one add_component was false
        r_result = abap_true.

        "add all packages of passed list
        DATA(lt_components) = CAST if_cts_hta_component_list( i_cts_hta_component )->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_package ).
        LOOP AT lt_components INTO DATA(lr_component).
          DATA(lv_result) = me->if_cts_hta_component_list~add_component( lr_component ).
          IF lv_result = abap_false AND r_result = abap_true.
            r_result = abap_false.
          ENDIF.
        ENDLOOP.

        "add all objects of passed list
        lt_components = CAST if_cts_hta_component_list( i_cts_hta_component )->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_object ).
        LOOP AT lt_components INTO lr_component.
          lv_result = me->if_cts_hta_component_list~add_component( lr_component ).
          IF lv_result = abap_false AND r_result = abap_true.
            r_result = abap_false.
          ENDIF.
        ENDLOOP.

        "add all full packages of passed list
        lt_components = CAST if_cts_hta_component_list( i_cts_hta_component )->get_components( ce_cts_hta_component_type=>ct_if_cts_hta_full_package ).
        LOOP AT lt_components INTO lr_component.
          lv_result = me->if_cts_hta_component_list~add_component( lr_component ).
          IF lv_result = abap_false AND r_result = abap_true.
            r_result = abap_false.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.