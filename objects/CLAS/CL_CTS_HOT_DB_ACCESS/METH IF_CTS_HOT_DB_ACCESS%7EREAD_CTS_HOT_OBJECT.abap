  METHOD if_cts_hot_db_access~read_cts_hot_object.
    SELECT SINGLE * FROM cts_hot_object INTO r_result WHERE abap_hana_package_id = i_abap_hana_package_id AND
                                                            abap_hana_object_name_suffix = i_abap_hana_object_name_suffix AND
                                                            abap_status = i_abap_status.
  ENDMETHOD.