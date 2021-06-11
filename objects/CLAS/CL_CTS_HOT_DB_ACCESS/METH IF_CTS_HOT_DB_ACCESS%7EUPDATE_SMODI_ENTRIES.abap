  METHOD if_cts_hot_db_access~update_smodi_entries.
    DATA: lt_smodisrc TYPE TABLE OF smodisrci.

    "move all smodiscri entries of the object to smodisrc and delete in smodisrci
    SELECT * FROM smodisrci INTO TABLE lt_smodisrc
          WHERE obj_type = 'HOTA'
            AND obj_name = i_obj_name(40)
            AND sub_type = i_obj_type
            AND sub_name = i_obj_name.

    DELETE FROM smodisrc
        WHERE obj_type = 'HOTA'
          AND obj_name = i_obj_name(40)
          AND sub_type = i_obj_type
          AND sub_name = i_obj_name.

    INSERT smodisrc FROM TABLE lt_smodisrc.

    DELETE FROM smodisrci
        WHERE obj_type = 'HOTA'
          AND obj_name = i_obj_name(40)
          AND sub_type = i_obj_type
          AND sub_name = i_obj_name.

    "handle smodilog entries
    CALL METHOD ('CL_CLM_TOOL_LOG')=>('IF_CLM_TOOL_LOG~ACTIVATE_ENTRIES')
      EXPORTING
        p_obj_type = 'HOTA'
        p_obj_name = i_obj_name(40)
        p_sub_type = i_obj_type
        p_sub_name = CONV eu_lname( i_obj_name ).
  ENDMETHOD.