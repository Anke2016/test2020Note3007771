  METHOD if_cts_hot_db_access~read_hot_status_for_object.
    CLEAR e_return_code.
    SELECT SINGLE hot_status FROM cts_hot_object
                             WHERE abap_hana_package_id = @i_abap_hana_package_id
                                AND abap_hana_object_name_suffix = @i_abap_hana_object_name_suffix
                                AND abap_status = @i_abap_status
                             INTO @r_hot_status.
    e_return_code = sy-subrc.
  ENDMETHOD.