  METHOD is_sync_required.
    IF i_force = abap_true.
      r_result = abap_true.
      RETURN.
    ENDIF.

    IF me->m_object_data_in_hta-hana_object_version = me->m_object_data_in_hana-hana_object_version.
      r_result = abap_false.
    ELSE.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.