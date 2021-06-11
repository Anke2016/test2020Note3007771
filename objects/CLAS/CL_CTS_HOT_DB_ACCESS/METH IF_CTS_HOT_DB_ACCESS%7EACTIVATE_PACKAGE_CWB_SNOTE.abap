  METHOD if_cts_hot_db_access~activate_package_cwb_snote.
    me->if_cts_hot_db_access~delete_cts_hot_package( i_abap_hana_package_id = i_package i_abap_status = 'A' ).
    UPDATE cts_hot_package SET abap_status = 'A' WHERE abap_hana_package_id = i_package AND abap_status = 'I'  .

    me->if_cts_hot_db_access~update_smodi_entries( i_obj_type = 'HOTP' i_obj_name = CONV #( i_package ) ).
  ENDMETHOD.