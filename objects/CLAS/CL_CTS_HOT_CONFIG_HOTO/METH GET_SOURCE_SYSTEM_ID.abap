  METHOD get_source_system_id.

    IF iv_abap_sync_system = 'SAP-SNOTE'.
      rv_result = iv_abap_sync_system+0(3).
    ELSE.
      " We assume that system ID is 3 characters long
      rv_result = COND #( LET pos = strlen( iv_abap_sync_system ) - 3 IN
                          WHEN pos < 0 THEN iv_abap_sync_system
                          ELSE iv_abap_sync_system+pos(3) ).
    ENDIF.
  ENDMETHOD.