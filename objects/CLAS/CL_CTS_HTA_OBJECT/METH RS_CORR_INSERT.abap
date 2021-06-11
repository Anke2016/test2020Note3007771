  METHOD rs_corr_insert.
    DATA lv_trkorr TYPE trkorr.

    IF is_sync_required( i_force ) = abap_false.
      RETURN.
    ENDIF.

    "By default try to use trkorr provided by user, only if user did not provide a transport request/task, use request received from rs_corr_check.
    IF i_trkorr IS NOT INITIAL.
      lv_trkorr = i_trkorr.
    ELSE.
      lv_trkorr = me->m_sync_trkorr.
    ENDIF.

    "make sure tadir is created before synchronizing the object. (to get correct master language and to have package entry in cts_hot_package as prerequisite for cts_hot_object
    IF me->m_sync_tadir-devclass IS INITIAL. "hotp not yet in HTA
      me->m_hta_package->synchronize( i_trkorr          = lv_trkorr
                                      i_devclass        = i_devclass
                                      i_suppress_dialog = i_suppress_dialog ).
    ENDIF.

    DATA(lv_used_trkorr) = me->m_external_calls->rs_corr_insert(
            i_pgmid             = if_cts_hta_object=>co_pgmid
            i_object_type       = if_cts_hta_object=>co_object_type
            i_object_name       = me->if_cts_hta_component~transport_object_name
            i_suppress_dialog   = i_suppress_dialog
            i_trkorr            = lv_trkorr
            i_cts_hta_component = me
    ).

    INSERT VALUE #( trkorr = lv_used_trkorr hta_components = VALUE #( ( me ) ) ) INTO TABLE r_result.
  ENDMETHOD.