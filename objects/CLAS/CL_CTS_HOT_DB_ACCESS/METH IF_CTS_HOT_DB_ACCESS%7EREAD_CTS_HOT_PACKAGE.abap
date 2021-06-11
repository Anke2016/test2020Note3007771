  METHOD if_cts_hot_db_access~read_cts_hot_package.
    SELECT SINGLE * FROM cts_hot_package INTO r_result WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_status = i_abap_status.
  ENDMETHOD.