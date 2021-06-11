  METHOD if_cts_hot_db_access~delete_smodi_entries.
    DELETE FROM smodilog WHERE obj_name = i_obj_name(40) AND sub_type = i_obj_type AND sub_name = i_obj_name .
    DELETE FROM smodilogi WHERE obj_name = i_obj_name(40) AND sub_type = i_obj_type AND sub_name = i_obj_name .
    DELETE FROM smodisrc WHERE obj_name = i_obj_name(40) AND sub_type = i_obj_type AND sub_name = i_obj_name .
    DELETE FROM smodisrci WHERE obj_name = i_obj_name(40) AND sub_type = i_obj_type AND sub_name = i_obj_name .
  ENDMETHOD.