  METHOD if_cts_hot_db_access~delete_cts_hot_package.
    DELETE FROM cts_hot_package WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_status = i_abap_status.
    IF i_abap_status = 'A'.
      DELETE FROM cts_hot_prework WHERE abap_hana_package_id = i_abap_hana_package_id.
    ENDIF.
  ENDMETHOD.