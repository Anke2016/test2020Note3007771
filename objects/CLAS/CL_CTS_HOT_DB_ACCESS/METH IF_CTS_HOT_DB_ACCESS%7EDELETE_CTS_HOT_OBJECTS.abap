  METHOD if_cts_hot_db_access~delete_cts_hot_objects.
    "Before deleting the objects, delete the texts
    SELECT abap_object_reference FROM cts_hot_object INTO TABLE @DATA(lt_object_references) WHERE abap_hana_package_id = @i_abap_hana_package_id AND abap_status = 'A'.

    LOOP AT lt_object_references INTO DATA(lv_object_reference).
      DELETE FROM cts_hot_otexts_h WHERE abap_object_reference = lv_object_reference.
      DELETE FROM cts_hot_otexts_s WHERE abap_object_reference = lv_object_reference.
      DELETE FROM cts_hot_otexts_l WHERE abap_object_reference = lv_object_reference.
    ENDLOOP.

    DELETE FROM cts_hot_object WHERE abap_hana_package_id = i_abap_hana_package_id AND abap_status = 'A'.
  ENDMETHOD.