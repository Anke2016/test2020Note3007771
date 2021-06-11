  METHOD rs_corr_insert_hta_component.
    DATA lt_sync_results TYPE if_cts_hta_types=>ty_sync_results.

    lt_sync_results = CAST cl_cts_hta_component( i_hta_component )->rs_corr_insert(
        i_trkorr          = i_trkorr
        i_devclass        = i_devclass
        i_suppress_dialog = i_suppress_dialog
        i_force           = i_force
    ).

    LOOP AT lt_sync_results REFERENCE INTO DATA(lr_sync_result).
      READ TABLE c_sync_results WITH TABLE KEY trkorr = lr_sync_result->trkorr REFERENCE INTO DATA(lr_result).
      IF sy-subrc = 0.
        APPEND LINES OF lr_sync_result->hta_components TO lr_result->hta_components.
      ELSE.
        INSERT lr_sync_result->* INTO TABLE c_sync_results.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.