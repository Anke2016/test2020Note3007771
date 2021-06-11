  METHOD if_cts_hot_db_access~set_deploy_mode.
    UPDATE cts_hot_package SET hot_activation_mode = i_deploy_mode->value WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_status = 'A'.
  ENDMETHOD.