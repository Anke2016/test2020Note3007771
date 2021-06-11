  METHOD if_cts_hot_db_access~activate_object_cwb_snote.
    me->if_cts_hot_db_access~delete_cts_hot_object( i_abap_hana_package_id         = i_object->abap_hana_package_id
                                                    i_abap_hana_object_name_suffix = i_object->abap_hana_object_name_suffix
                                                    i_abap_status                  = 'A' ).
    UPDATE cts_hot_object SET abap_status = 'A' WHERE abap_hana_package_id = i_object->abap_hana_package_id AND abap_hana_object_name_suffix = i_object->abap_hana_object_name_suffix AND abap_status = 'I'.

    me->if_cts_hot_db_access~update_smodi_entries( i_obj_type = 'HOTO' i_obj_name = i_object->transport_object_name ).
  ENDMETHOD.