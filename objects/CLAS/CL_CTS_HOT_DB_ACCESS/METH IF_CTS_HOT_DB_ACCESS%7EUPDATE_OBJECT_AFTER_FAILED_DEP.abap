  METHOD if_cts_hot_db_access~update_object_after_failed_dep.
    UPDATE cts_hot_object SET hot_status = i_new_status WHERE abap_hana_package_id = i_old_object-abap_hana_package_id
                                                           AND abap_hana_object_name_suffix = i_old_object-abap_hana_object_name_suffix
                                                           AND abap_status = i_old_object-abap_status
                                                           AND hot_status = i_old_object-hot_status
                                                           AND hana_source_object_version = i_old_object-hana_source_object_version.
  ENDMETHOD.