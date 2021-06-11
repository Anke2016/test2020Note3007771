  METHOD if_cts_hot_db_access~delete_cts_hot_object.
    "Before deleting the object, delete the texts, but only if i_abap_status = 'A'
    IF i_abap_status = 'A'.
      SELECT SINGLE abap_object_reference FROM cts_hot_object INTO @DATA(lv_object_reference) WHERE abap_hana_package_id = @i_abap_hana_package_id AND abap_hana_object_name_suffix = @i_abap_hana_object_name_suffix AND abap_status = 'A'.

      IF lv_object_reference IS NOT INITIAL.
        DELETE FROM cts_hot_otexts_h WHERE abap_object_reference = lv_object_reference.
        DELETE FROM cts_hot_otexts_s WHERE abap_object_reference = lv_object_reference.
        DELETE FROM cts_hot_otexts_l WHERE abap_object_reference = lv_object_reference.
      ENDIF.
    ENDIF.

    DELETE FROM cts_hot_object WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_hana_object_name_suffix = i_abap_hana_object_name_suffix AND abap_status = i_abap_status.
  ENDMETHOD.