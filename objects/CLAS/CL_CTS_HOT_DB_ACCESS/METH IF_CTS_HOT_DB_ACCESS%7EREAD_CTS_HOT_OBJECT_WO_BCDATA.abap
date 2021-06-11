  METHOD if_cts_hot_db_access~read_cts_hot_object_wo_bcdata.
    SELECT SINGLE  abap_hana_package_id abap_hana_object_name_suffix abap_status hot_status hana_package_id hana_object_name hana_object_suffix
                   abap_sync_system hana_read_system hana_source_object_version hana_object_version hana_source_build_version hana_activated_at
                   hana_activated_by abap_synced_at abap_synced_by abap_deployed_at abap_deployed_by abap_import_timestamp abap_object_reference
           FROM cts_hot_object INTO CORRESPONDING FIELDS OF r_result WHERE abap_hana_package_id = i_abap_hana_package_id AND
                                                                           abap_hana_object_name_suffix = i_abap_hana_object_name_suffix AND
                                                                           abap_status = i_abap_status.
  ENDMETHOD.