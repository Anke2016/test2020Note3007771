  METHOD if_cts_hot_db_access~update_package_after_deploymnt.
    DATA(lv_deployed_at) = i_old_package-abap_deployed_at.
    DATA(lv_deployed_by) = i_old_package-abap_deployed_by.
    IF i_new_deployed_at IS NOT INITIAL.
      lv_deployed_at = i_new_deployed_at.
    ENDIF.
    IF i_new_deployed_by IS NOT INITIAL.
      lv_deployed_by = i_new_deployed_by.
    ENDIF.
    UPDATE cts_hot_package SET hot_status = i_new_status abap_deployed_at = lv_deployed_at abap_deployed_by = lv_deployed_by
                           WHERE abap_hana_package_id = i_old_package-abap_hana_package_id
                             AND abap_status = i_old_package-abap_status
                             AND hot_status = i_old_package-hot_status.
  ENDMETHOD.