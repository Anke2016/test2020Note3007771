  METHOD rs_corr_check.
    IF is_sync_required( i_force ) = abap_false.
      RETURN.
    ENDIF.

    me->m_sync_trkorr = me->m_external_calls->rs_corr_check(
      EXPORTING
        i_pgmid       = if_cts_hta_package=>co_pgmid
        i_object_type = if_cts_hta_package=>co_object_type
        i_object_name = me->if_cts_hta_component~transport_object_name
        i_suppress_dialog = i_suppress_dialog
        i_cts_hta_component = me
      IMPORTING
        e_tadir = me->m_sync_tadir
    ).
  ENDMETHOD.