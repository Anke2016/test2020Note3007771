  METHOD if_cts_hot_db_access~exists_package.
    DATA: lv_abap_hana_package_id TYPE cts_hot_package_id.

    SELECT SINGLE abap_hana_package_id FROM cts_hot_package INTO lv_abap_hana_package_id WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_status = 'A'.
    IF sy-subrc = 0.
      r_exists = abap_true.
    ELSE.
      r_exists = abap_false.
    ENDIF.
  ENDMETHOD.